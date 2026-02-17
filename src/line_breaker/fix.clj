(ns line-breaker.fix
  "Line breaking functions for reformatting Clojure code.

  Provides functions to identify breakable forms, generate line break edits,
  and apply those edits to source code."
  (:require
   [clojure.string :as str]
   [line-breaker.check :as check]
   [line-breaker.rules :as rules]
   [line-breaker.treesitter.node :as node]
   [line-breaker.treesitter.parser :as parser]))

(declare collect-collapse-edits)
(declare ^:private collapse-repositioned-children)

;;; Edit application

(defn byte-offset->char-index
  "Convert a UTF-8 byte offset to a character index.

  Tree-sitter returns byte positions, but Java String operations use
  character indices. This function converts byte offsets to character
  indices by counting how many characters it takes to reach the target
  byte offset.

  Handles surrogate pairs (4-byte UTF-8 characters like emojis) which
  are represented as 2 chars in Java strings."
  [^String s byte-offset]
  (let [len (count s)]
    (loop [char-idx 0
           byte-idx 0]
      (cond
        (>= byte-idx byte-offset) char-idx
        (>= char-idx len) len
        :else
        (let [code-point (.codePointAt s char-idx)
              ;; Number of chars this code point uses (1 or 2 for surrogates)
              char-count (Character/charCount code-point)
              ;; Number of UTF-8 bytes this code point uses
              code-point-bytes (cond
                                 (<= code-point 0x7F) 1
                                 (<= code-point 0x7FF) 2
                                 (<= code-point 0xFFFF) 3
                                 :else 4)]
          (recur (+ char-idx char-count) (+ byte-idx code-point-bytes)))))))

(defn apply-edits
  "Apply replacement edits to source string.

  Takes a source string and a sequence of edits. Each edit is a map with
  :start (byte position, inclusive), :end (byte position, exclusive),
  and :replacement (text to substitute). Edits are applied in reverse
  start offset order to preserve position validity.

  Throws if any edits have overlapping byte ranges. Returns the modified
  source string."
  [source edits]
  (let [sorted (sort-by :start > edits)]
    (doseq [[higher lower] (partition 2 1 sorted)]
      (when (> (:end lower) (:start higher))
        (throw
         (ex-info
          "Overlapping edits detected"
          {:edit-a lower
           :edit-b higher}))))
    (reduce
     (fn [s {:keys [start end replacement]}]
       (let [start-char (byte-offset->char-index s start)
             end-char (byte-offset->char-index s end)]
         (str (subs s 0 start-char) replacement (subs s end-char))))
     source
     sorted)))

(defn edits-change-source?
  "Returns true if at least one edit differs from the current source content."
  [source edits]
  (boolean
   (some
    (fn [{:keys [start end replacement]}]
      (let [start-char (byte-offset->char-index source start)
            end-char (byte-offset->char-index source end)]
        (not= replacement (subs source start-char end-char))))
    edits)))

;;; Node helpers

(defn node-start-line
  "Get the 1-indexed start line of a node."
  [node]
  (first (node/node-line-range node)))

(defn- node-in-ignored-range?
  "Returns true if node falls within any of the ignored byte ranges.
  Ignored ranges are [start-byte end-byte] pairs where start is inclusive
  and end is exclusive."
  [node ignored-ranges]
  (when-let [[start-byte end-byte] (node/node-range node)]
    (some
     (fn [[ign-start ign-end]]
       (and (<= ign-start start-byte) (<= end-byte ign-end)))
     ignored-ranges)))

;;; Byte offset helpers

(defn- element-start-offset
  "Get the start byte offset of a node."
  [node]
  (first (node/node-range node)))

(defn- element-end-offset
  "Get the end byte offset of a node."
  [node]
  (second (node/node-range node)))

;;; Form joining (un-breaking)

(defn join-form-edits
  "Generate edits that collapse a multi-line form back to a single line.

  Given a node that spans multiple lines, iterates through consecutive
  pairs of named children and generates edits replacing inter-child
  whitespace (newlines + indent) with single spaces. Returns a vector
  of {:start :end :replacement} edits, or nil if the node is already
  single-line.

  Assumes only whitespace exists between consecutive named children.
  This holds for tree-sitter-clojure forms because all meaningful
  content (metadata, reader macros, discard forms) are named nodes,
  and anonymous nodes (delimiters) occur only at form boundaries."
  [node]
  (when node
    (let [[start-line end-line] (node/node-line-range node)]
      (when (not= start-line end-line)
        (let [children (node/named-children node)
              edits (into
                     []
                     (keep
                      (fn [[prev-child next-child]]
                        (let [end-byte (element-end-offset prev-child)
                              start-byte (element-start-offset next-child)
                              prev-end-line
                              (second (node/node-line-range prev-child))
                              next-start-line
                              (node-start-line next-child)]
                          ;; Skip blank line gaps — they're intentional
                          ;; grouping separators, not formatting
                          (when (and (> start-byte end-byte)
                                     (< (- next-start-line prev-end-line) 2))
                            {:start end-byte
                             :end start-byte
                             :replacement
                             (if (and
                                  (= :comment (node/node-type next-child))
                                  (not= prev-end-line next-start-line))
                               "\n"
                               " ")}))))
                     (partition 2 1 children))]
          (when (seq edits)
            edits))))))

;;; Form breaking

(defn form-start-column
  "Get the column where the form starts (0-indexed)."
  [node]
  (:column (node/node-position node)))

(defn comment-node?
  "Returns true if node is a comment."
  [node]
  (= :comment (node/node-type node)))

(defn- same-line?
  "Returns true if two nodes start on the same line."
  [node1 node2]
  (= (node-start-line node1) (node-start-line node2)))

(defn single-line-node?
  "Returns true if node starts and ends on the same line."
  [node]
  (let [[start-line end-line] (node/node-line-range node)]
    (= start-line end-line)))

(defn- first-line-end-column
  "Get the end column of a node's first line.
  For single-line nodes, returns the end column. For multi-line nodes,
  computes start column + length of the first line of the node's text."
  [node]
  (if (single-line-node? node)
    (:column (node/node-end-position node))
    (let [start-col (:column (node/node-position node))
          text (node/node-text node)
          newline-idx (.indexOf ^String text "\n")]
      (+ start-col newline-idx))))

(defn- effective-pair-width
  "Compute the first-line pair width accounting for value breaking.
  For breakable values with more children than kept on the first line,
  estimates the width after breaking by using the end column of the last
  kept child. This avoids over-estimating when a collapsed value would
  be broken internally rather than split from its key.
  For metadata-wrapped values, the metadata child shifts named-children
  indices, so keep-count is adjusted to compensate."
  [exc-name exc-value config]
  (let [value-end (if (rules/breakable-node? exc-value)
                    (let [children (node/named-children exc-value)
                          val-rule (rules/get-effective-rule exc-value config)
                          keep-count
                          (cond-> (rules/elements-to-keep-on-first-line
                                   val-rule)
                            (rules/metadata-wrapped? exc-value) inc)]
                      (if (and (seq children) (< keep-count (count children)))
                        (first-line-end-column (nth children (dec keep-count)))
                        (first-line-end-column exc-value)))
                    (first-line-end-column exc-value))]
    (- value-end (form-start-column exc-name))))

(defn- max-line-end-column
  "Get the maximum end column across all lines of a node.
  Returns the end column of whichever line is longest. For single-line
  nodes, returns the end column. For multi-line nodes, computes the
  column where each line ends, accounting for the start column of the
  first line and zero-based indent of subsequent lines."
  [node]
  (if (single-line-node? node)
    (:column (node/node-end-position node))
    (let [start-col (:column (node/node-position node))
          text (node/node-text node)
          lines (.split ^String text "\n" -1)]
      (reduce
       max
       (+ start-col (count (aget lines 0)))
       (mapv count (next (vec lines)))))))

(defn- find-exceeding-pair
  "Find the first pair in a pair-grouped form whose elements are on the
  same line and any line of the value exceeds max-length.

  Returns [name-node value-node] or nil. Checks all lines of the value
  (not just the first) so that deeply nested unbreakable atoms trigger
  pair splitting when moving the value to its own line would reduce
  indent. Pairs are determined by the form's pair grouping structure:
  maps and binding vectors pair from the start, while
  cond/case/condp/cond-> skip a prefix of non-pair elements. Comments
  are filtered before pairing."
  [node rule max-length]
  (when max-length
    (let [children (node/named-children node)
          non-comment (remove comment-node? children)
          prefix (if (#{:map :binding-vector} rule)
                   0
                   (rules/elements-to-keep-on-first-line rule))
          pairs (partition 2 (drop prefix non-comment))]
      (some
       (fn [[name-node value-node]]
         (when (and
                (same-line? name-node value-node)
                (> (max-line-end-column value-node) max-length))
           [name-node value-node]))
       pairs))))

(defn- has-consecutive-children-on-line?
  "Returns true if node has at least two consecutive named children
  where the first ends on the same line as the second starts."
  [node]
  (let [children (node/named-children node)]
    (boolean
     (some
      (fn [[prev-child next-child]]
        (=
         (second (node/node-line-range prev-child))
         (first (node/node-line-range next-child))))
      (partition 2 1 children)))))

(defn indent-column
  "Calculate the indent column for broken elements.
  - :binding-vector uses +1 (align to first element after bracket)
  - :metadata-wrapped aligns to first content element (after metadata)
  - Forms with an indent rule use +2 (standard Clojure body indentation)
  - Plain function calls and data structures use +1 (align to first element)"
  [node rule]
  (let [base-col (form-start-column node)]
    (cond
      (#{:binding-vector :map} rule) (+ 1 base-col)
      (= :metadata-wrapped rule)
      ;; Align to the first content element (second child, after metadata).
      ;; Fall back to base-col + 1 if no content element exists.
      (if-let [content-node (second (node/named-children node))]
        (form-start-column content-node)
        (+ 1 base-col))
      (some? rule) (+ 2 base-col)
      :else (+ 1 base-col))))

(defn make-break-edit
  "Create a break edit between two children.
  Returns nil if no edit needed (comment attached to preceding element).
  When prev-child is a comment (has trailing newline), only inserts indent.
  Preserves blank lines between children as grouping separators."
  [prev-child next-child indent-col]
  (let [indent-spaces (apply str (repeat indent-col \space))
        blank-line? (>= (- (node-start-line next-child)
                           (second (node/node-line-range prev-child)))
                        2)
        newline-str (if blank-line? "\n\n" "\n")]
    (cond
      ;; Comment on same line as prev: keep them together (no edit)
      (and (comment-node? next-child) (same-line? prev-child next-child)) nil
      ;; Prev is comment (ends with newline): just add indent
      (comment-node? prev-child) {:start (element-end-offset prev-child)
                                  :end (element-start-offset next-child)
                                  :replacement (str
                                                (when blank-line? "\n")
                                                indent-spaces)}
      ;; Normal case: add newline + indent
      :else {:start (element-end-offset prev-child)
             :end (element-start-offset next-child)
             :replacement (str newline-str indent-spaces)})))

(defn generate-paired-edits
  "Generate edits for pair-grouped breaking.

  Groups elements in pairs and breaks between pairs. Comments are filtered
  out before pairing to prevent disrupting the grouping logic. Break edits
  are generated for pair-start elements, comments, and elements following
  comments so that whole-line comments within pairs get proper indentation."
  [last-kept breakable-children indent-col]
  ;; Filter out comments before pairing to avoid disrupting pair grouping.
  ;; Comments in binding vectors would otherwise shift the pairing (e.g.,
  ;; [a 1 ;comment b 2] would incorrectly pair as [[;comment b] [2]]).
  (let [non-comment-children (remove comment-node? breakable-children)
        pairs (partition-all 2 non-comment-children)
        pair-starts (into #{} (map first) pairs)
        all-pairs (cons
                   [last-kept (first breakable-children)]
                   (partition 2 1 breakable-children))]
    (into
     []
     (keep
      (fn [[prev-child next-child]]
        (when (or
               (contains? pair-starts next-child)
               (comment-node? next-child)
               (comment-node? prev-child))
          (make-break-edit prev-child next-child indent-col))))
     all-pairs)))

(defn- generate-sequential-edits
  "Generate edits for sequential (non-paired) breaking.
  Each element gets its own line."
  [last-kept breakable-children indent-col]
  (let [all-pairs (cons
                   [last-kept (first breakable-children)]
                   (partition 2 1 breakable-children))]
    (into
     []
     (keep
      (fn [[prev-child next-child]]
        (make-break-edit prev-child next-child indent-col)))
     all-pairs)))

(defn- break-exceeding-pair
  "Split an exceeding pair onto separate lines and generate inter-pair edits.
  When the value is multi-line, recursively collapses it (including nested
  forms) to a single line so it gets re-broken at the new indent position.
  Returns a result map {:edits [...]} or nil."
  [exc-name exc-value children base-keep-count breakable-children indent-col]
  (let [join-edits (collect-collapse-edits exc-value)
        indent-str (apply str (repeat indent-col \space))
        split-edit {:start (element-end-offset exc-name)
                    :end (element-start-offset exc-value)
                    :replacement (str "\n" indent-str)}
        pair-edits (when (seq breakable-children)
                     (generate-paired-edits
                      (nth children (dec base-keep-count))
                      breakable-children
                      indent-col))
        all-edits (into
                   (vec pair-edits)
                   (if join-edits
                     (cons split-edit join-edits)
                     [split-edit]))]
    (when (seq all-edits)
      {:edits all-edits})))

(defn- pair-exceeds-at-indent?
  "Returns true if an exceeding pair's first line would still exceed
  max-length when placed at indent-col. For breakable values, estimates
  the post-break width (key + value head) rather than the full width."
  [exc-name exc-value indent-col max-length config]
  (let [pw (effective-pair-width exc-name exc-value config)]
    (> (+ indent-col pw) max-length)))

(defn- collapse-moved-pair-children
  "Collapse pair-grouped children whose column will change after breaking.
  Filters pairs-to-break for pair-grouped second elements that will move
  to indent-col, then collapses them so they re-indent correctly."
  [pairs-to-break indent-col config]
  (let [moved-pair-children
        (into
         []
         (comp
          (map second)
          (filter
           (fn [child]
             (and
              (rules/uses-pair-grouping? child config)
              (not= (form-start-column child) indent-col)))))
         pairs-to-break)]
    (collapse-repositioned-children moved-pair-children indent-col)))

(defn generate-parent-break-edits
  "When breaking a child will make it multi-line, check whether the
  parent has siblings sharing a line with the child. If so, generate
  break edits to separate them. Only applies to parents without indent
  rules (plain function calls, data structures) — forms with indent
  rules (let, defn, etc.) use forced breaks for body separation.
  Also collapses repositioned pair-grouped siblings whose internal
  indent would be stale at the new column.
  Returns a map {:edits [...] :moves-child? bool}, or nil."
  [child-node config]
  (when-let [parent (node/node-parent child-node)]
    (let [rule (rules/get-effective-rule parent config)]
      (when (and
             (rules/breakable-node? parent)
             (not (rules/uses-pair-grouping? parent config))
             (nil? rule)
             (has-consecutive-children-on-line? parent))
        (let [children (node/named-children parent)
              base-keep-count (rules/elements-to-keep-on-first-line rule)
              indent-col (indent-column parent rule)
              breakable-children (drop base-keep-count children)]
          (when (seq breakable-children)
            (let [last-kept (nth children (dec base-keep-count))
                  all-pairs (cons
                             [last-kept (first breakable-children)]
                             (partition 2 1 breakable-children))
                  sharing-line? (fn [[prev-child next-child]]
                                  (let [[_ prev-end] (node/node-line-range
                                                      prev-child)
                                        next-start (node-start-line next-child)]
                                    (= prev-end next-start)))
                  pairs-to-break (filterv sharing-line? all-pairs)
                  break-edits (into
                               []
                               (keep
                                (fn [[prev-child next-child]]
                                  (make-break-edit
                                   prev-child
                                   next-child
                                   indent-col)))
                               pairs-to-break)
                  collapse-edits (collapse-moved-pair-children
                                  pairs-to-break
                                  indent-col
                                  config)
                  moves-child? (not= (form-start-column child-node) indent-col)]
              (when (seq break-edits)
                {:edits (into break-edits collapse-edits)
                 :moves-child? moves-child?}))))))))

(defn break-form
  "Generate edits to break a form across multiple lines.

  Takes a node and optional config map. Applies indent rules based on the
  form's head symbol:
  - :defn/:def rules keep name on first line (2 elements)
  - Default keeps only first element on first line

  For forms that use pair grouping (maps, cond, case), keeps related pairs
  together (key-value, test-result, etc.) and breaks only between pairs.

  Uses a single-pass-then-backtrack approach for pair-grouped forms:
  generates normal paired edits first, then checks if any exceeding pair's
  first line (key + value head) would still exceed at the indent position.
  If so, backtracks to split the pair onto its own line via
  break-exceeding-pair. Internal lines of multi-line values are left to
  subsequent iterations to fix by breaking sub-forms.

  When breaking repositions children that are already multi-line, also
  collapses them so the next iteration re-breaks at the correct indent.

  When breaking will make a form multi-line, backtracks to the parent:
  if the parent is a plain function call with siblings sharing a line,
  generates break edits for the parent too. If the parent break moves
  the form to a different column, the form's own edits are omitted so
  the next iteration re-breaks at the correct indent.

  Comments on the same line as the preceding element stay attached.
  Comments include their trailing newline, so no extra newline is added after.

  Returns a result map {:edits [...]} or nil.
  Each edit is {:start n :end m :replacement s}."
  ([node]
   (break-form node {}))
  ([node config]
   (when node
     (let [children (node/named-children node)
           rule (rules/get-effective-rule node config)
           indent-col (indent-column node rule)
           base-keep-count (rules/elements-to-keep-on-first-line rule)
           max-length (get config :line-length)
           exceeding-pair (when (rules/uses-pair-grouping? node config)
                            (find-exceeding-pair node rule max-length))
           [exc-name exc-value] exceeding-pair
           breakable-children (drop base-keep-count children)
           split-pair? (and
                        exceeding-pair
                        (rules/breakable-node? exc-value)
                        (pair-exceeds-at-indent?
                         exc-name
                         exc-value
                         indent-col
                         max-length
                         config))]
       (if split-pair?
         ;; Pair's first line (key + value head) exceeds at indent,
         ;; split onto separate lines regardless of other children.
         (break-exceeding-pair
          exc-name
          exc-value
          children
          base-keep-count
          breakable-children
          indent-col)
         (when (seq breakable-children)
           (let [last-kept (nth children (dec base-keep-count))
                 parent-result (generate-parent-break-edits node config)]
             (if (:moves-child? parent-result)
               ;; Parent will move this form — omit internal edits
               ;; (they'd use stale indent). Next iteration re-breaks.
               {:edits (:edits parent-result)}
               (let [edits (if (rules/uses-pair-grouping? node config)
                             (generate-paired-edits
                              last-kept
                              breakable-children
                              indent-col)
                             (generate-sequential-edits
                              last-kept
                              breakable-children
                              indent-col))]
                 (when (seq edits)
                   (let [collapse-targets
                         (if (rules/uses-pair-grouping? node config)
                           ;; Only collapse pair-start elements (keys).
                           ;; Pair values are naturally positioned after
                           ;; their keys, not at indent-col.
                           (let [non-comment (remove
                                              comment-node?
                                              breakable-children)]
                             (into
                              []
                              (comp (partition-all 2) (map first))
                              non-comment))
                           breakable-children)
                         collapse-edits (collapse-repositioned-children
                                         collapse-targets
                                         indent-col)
                         parent-edits (:edits parent-result)]
                     {:edits (cond-> (into edits collapse-edits)
                               parent-edits (into parent-edits))})))))))))))

;;; Line length checking

(defn find-long-lines
  "Find 1-indexed line numbers exceeding max-length.
  Returns a vector of line numbers."
  [source max-length]
  (mapv :line (check/find-violations source max-length)))

;;; Collapse edits

(defn collect-collapse-edits
  "Recursively collect collapse edits for a node and all its descendants.
  Returns a vector of edits that collapse all internal whitespace to single
  spaces (with comment-aware newline preservation), or nil if the node is
  already single-line.

  Parent and child edits never overlap: join-form-edits targets whitespace
  *between* a node's named children (gaps outside child byte ranges), while
  recursive child edits target whitespace *inside* each child's byte range.
  These regions are disjoint by construction."
  [node]
  (when-let [[start-line end-line] (node/node-line-range node)]
    (when (not= start-line end-line)
      (let [own-edits (or (join-form-edits node) [])
            children (or (node/named-children node) [])]
        (reduce
         (fn [edits child]
           (if-let [child-edits (collect-collapse-edits child)]
             (into edits child-edits)
             edits))
         own-edits
         children)))))

(defn- collapse-repositioned-children
  "Generate collapse edits for multi-line children being repositioned.
  When a parent form is broken, children that move to new lines have
  stale internal indent. Collapsing them lets the next iteration
  re-break at the correct position. Only collapses children whose
  current column differs from indent-col, since children already at
  the target indent don't need re-breaking."
  [breakable-children indent-col]
  (into
   []
   (comp
    (remove single-line-node?)
    (filter (fn [child]
              (not= (form-start-column child) indent-col)))
    (mapcat collect-collapse-edits))
   breakable-children))

;;; Iterative multi-pass breaking

(def max-iterations
  "Maximum number of breaking passes to prevent infinite loops.
  100 is generous for deeply nested forms (typical code rarely needs more
  than 10-20 passes) while catching bugs that cause infinite loops."
  100)

(defn- edits-overlap?
  "Returns true if any edit in new-edits overlaps with any in
  existing-edits. Edits overlap when their byte ranges intersect."
  [existing-edits new-edits]
  (some
   (fn [new-edit]
     (some
      (fn [existing]
        (and
         (< (:start new-edit) (:end existing))
         (< (:start existing) (:end new-edit))))
      existing-edits))
   new-edits))

(defn inside-broken-form?
  "Returns true if range is contained within any range in broken-ranges.
  A child form whose parent was already broken in this pass should not
  be broken until the next pass, when it will have correct column
  positions after re-parsing.
  Public for use by reformat.clj batch edit collection."
  [broken-ranges [start end]]
  (some (fn [[s e]]
          (and (<= s start) (<= end e))) broken-ranges))

(defn try-collect-edits
  "Collect edits for a form if they are new, change source, and don't overlap.
  Returns [updated-state edits] on success, [state nil] otherwise.
  State is a map with :seen (set of byte ranges) and :collected (vec of edits).
  Public for use by reformat.clj batch edit collection."
  [state source form edits]
  (let [{:keys [seen collected]} state
        range (node/node-range form)]
    (if (or
         (seen range)
         (not (seq edits))
         (not (edits-change-source? source edits))
         (edits-overlap? collected edits))
      [state nil]
      [(-> state (update :seen conj range) (update :collected into edits))
       edits])))

;;; Form-based walk

(defn- children-sharing-long-line?
  "Returns true if node has consecutive named children where one ends
  and the next starts on a line in long-lines-set."
  [node long-lines-set]
  (let [children (node/named-children node)]
    (boolean
     (some
      (fn [[prev-child next-child]]
        (let [[_ prev-end] (node/node-line-range prev-child)
              next-start (node-start-line next-child)]
          (and (= prev-end next-start) (contains? long-lines-set prev-end))))
      (partition 2 1 children)))))

(defn- contains-long-line?
  "Returns true if node spans any line in long-lines-set."
  [node long-lines-set]
  (let [[start-line end-line] (node/node-line-range node)]
    (boolean (some #(and (>= % start-line) (<= % end-line)) long-lines-set))))

(defn- only-trailing-delimiters?
  "True when everything after node's end on its end-line is closing
  delimiters. This detects lines pushed over the limit by stacked
  ancestor closing parens rather than sibling content."
  [source node]
  (let [{:keys [row column]} (node/node-end-position node)
        lines (str/split-lines source)]
    (when (< row (count lines))
      (let [line (nth lines row)
            trailing (subs line (min column (count line)))]
        (boolean (re-matches #"[\)\]\}]*" trailing))))))

(defn- try-form-break
  "Try to generate edits for a single form.
  Tries each form-breaker function in order, then falls back to
  line-length breaking. A form is broken when it:
  - has children sharing a long line and the form's end column
    reaches the limit or only trailing delimiters follow, OR
  - contains a long line, has unseparated children, and the form's
    end column reaches the line-length limit (breaking reduces
    nesting indirectly)
  The trailing-delimiters check handles lines pushed over the limit
  by stacked ancestor closing parens (e.g. test assertions at the
  end of nested testing blocks)."
  [node source config form-breakers long-lines-set]
  (let [max-length (get config :line-length)]
    (or
     (some (fn [breaker]
             (breaker node source config)) form-breakers)
     (when (and
            (rules/breakable-node? node)
            (seq long-lines-set)
            (or
             (and
              (children-sharing-long-line? node long-lines-set)
              (or (nil? max-length)
                  (>= (max-line-end-column node) max-length)
                  (only-trailing-delimiters? source node)))
             (and
              (or (nil? max-length)
                  (>= (max-line-end-column node) max-length))
              (contains-long-line? node long-lines-set)
              (has-consecutive-children-on-line? node))))
       (break-form node config)))))

(defn- walk-and-collect-edits
  "Pre-order walk collecting non-overlapping edits from all forms.
  Tries form-breakers first, then line-length breaking for each form.
  Skips children of already-broken forms, ignored ranges, and metadata
  nodes. Does not descend into metadata-wrapped forms."
  [root source config form-breakers long-lines-set ignored-ranges]
  (loop [stack (vec (rseq (vec (node/named-children root))))
         state {:seen #{}
                :collected []}]
    (if (empty? stack)
      (:collected state)
      (let [node (peek stack)
            rest-stack (pop stack)]
        (cond
          (nil? node) (recur rest-stack state)
          (rules/metadata-node? node) (recur rest-stack state)
          (node-in-ignored-range? node ignored-ranges) (recur rest-stack state)
          (inside-broken-form? (:seen state) (node/node-range node)) (recur
                                                                      rest-stack
                                                                      state)
          :else
          (let [result (try-form-break
                        node
                        source
                        config
                        form-breakers
                        long-lines-set)
                edits (:edits result)
                [new-state _] (if (seq edits)
                                (try-collect-edits state source node edits)
                                [state nil])
                descend? (not (rules/metadata-wrapped? node))
                children (when descend?
                           (node/named-children node))
                new-stack (if (seq children)
                            (into rest-stack (rseq (vec children)))
                            rest-stack)]
            (recur new-stack new-state)))))))

(defn fix-source
  "Fix line length violations and apply structural breaks to source code.

  Takes a source string and config map with :line-length. Iteratively
  walks all forms in pre-order, applying breaking strategies until stable.
  Returns the fixed source string. Forms preceded by #_:line-breaker/ignore
  are not modified.

  Breaking strategies are tried per-form in this order:
  1. Each function in :form-breakers (if provided)
  2. Line-length breaking (for breakable forms with children on lines
     exceeding the limit)

  The pre-order walk ensures outer forms are broken before inner forms.
  Children of broken forms are skipped until the next iteration when
  re-parsing provides correct column positions."
  [source config & {:keys [form-breakers]}]
  (let [max-length (get config :line-length 80)]
    (loop [source source
           iteration 0]
      (if (>= iteration max-iterations)
        source
        (let [tree (parser/parse-source source)
              root (node/root-node tree)
              long-lines-set (set (find-long-lines source max-length))
              ignored-ranges (check/find-ignored-byte-ranges tree)
              collected (walk-and-collect-edits
                         root
                         source
                         config
                         (or form-breakers [])
                         long-lines-set
                         ignored-ranges)]
          (if (and (seq collected) (edits-change-source? source collected))
            (recur (apply-edits source collected) (inc iteration))
            source))))))
