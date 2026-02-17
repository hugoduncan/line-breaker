(ns line-breaker.fix
  "Line breaking functions for reformatting Clojure code.

  Provides functions to identify breakable forms, generate line break edits,
  and apply those edits to source code."
  (:require
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

;;; Finding breakable forms

(defn- node-contains-line?
  "Returns true if node spans the given 1-indexed line number."
  [node line]
  (when-let [[start-line end-line] (node/node-line-range node)]
    (<= start-line line end-line)))

(defn node-start-line
  "Get the 1-indexed start line of a node."
  [node]
  (first (node/node-line-range node)))

(defn- form-needs-breaking-on-line?
  "Returns true if form has consecutive children both on the target line.
  Uses end-line for prev-child to handle multiline nodes (e.g. after
  forced breaks, a multiline child ends on a line where the next sibling
  starts). A form already broken (children on separate lines) returns
  false."
  [node line]
  (let [children (node/named-children node)]
    (some
     (fn [[prev-child next-child]]
       (let [[_ prev-end-line] (node/node-line-range prev-child)
             next-line (node-start-line next-child)]
         (= prev-end-line next-line line)))
     (partition 2 1 children))))

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

(defn- find-breakable-forms-on-line
  "Find all breakable nodes containing the given line that need breaking.

  Returns a vector of nodes from outermost to innermost. Only includes
  forms that have consecutive children on the target line. Skips forms
  that fall within ignored ranges.

  Forms with metadata attached (first child is meta_lit) are breakable,
  but we don't descend into their children. This keeps the metadata attached
  and prevents inner elements from being broken separately."
  [node line ignored-ranges]
  (when (and
         (node-contains-line? node line)
         (not (node-in-ignored-range? node ignored-ranges))
         (not (rules/metadata-node? node)))
    (if (rules/metadata-wrapped? node)
      ;; Metadata-wrapped form: breakable but don't descend into children
      (when (and
             (rules/breakable-node? node)
             (form-needs-breaking-on-line? node line))
        [node])
      ;; Normal form: check self and recurse into children
      (let [self
            (when (and
                   (rules/breakable-node? node)
                   (form-needs-breaking-on-line? node line))
              [node])
            children-results
            (mapcat
             #(find-breakable-forms-on-line % line ignored-ranges)
             (node/named-children node))]
        (into (vec self) children-results)))))

(defn- has-preceding-sibling-on-line?
  "Returns true if node has a preceding sibling in its parent that
  ends on the same line where node starts."
  [node]
  (when-let [parent (node/node-parent node)]
    (let [node-start (node-start-line node)
          node-range (node/node-range node)]
      (some
       (fn [[prev-child next-child]]
         (and
          (= (node/node-range next-child) node-range)
          (= (second (node/node-line-range prev-child)) node-start)))
       (partition 2 1 (node/named-children parent))))))

(defn- find-ancestors-needing-sibling-separation
  "Walk up from node collecting all ancestors that share their start
  line with a preceding sibling. Returns a seq of ancestor parents
  (breakable forms whose children need separating), outermost first.
  Returns all matches so that if the outermost is already broken,
  intermediate ancestors can be tried."
  [node]
  (loop [n node
         results []]
    (if-not n
      (rseq results)
      (let [parent (node/node-parent n)]
        (if-not parent
          (rseq results)
          (recur
           parent
           (if (and
                (has-preceding-sibling-on-line? n)
                (rules/breakable-node? parent))
             (conj results parent)
             results)))))))

(defn find-breakable-forms
  "Find all breakable forms containing the given line.

  Takes a parsed tree, a 1-indexed line number, and optionally a set of
  ignored ranges. Returns a vector of breakable nodes from outermost to
  innermost that span that line and have consecutive children on that line.
  Forms within ignored ranges are skipped."
  ([tree line]
   (find-breakable-forms tree line #{}))
  ([tree line ignored-ranges]
   (find-breakable-forms-on-line (node/root-node tree) line ignored-ranges)))

(defn find-breakable-form
  "Find the outermost breakable form containing the given line.

  Takes a parsed tree, a 1-indexed line number, and optionally a set of
  ignored ranges. Returns the outermost breakable node (list_lit, vec_lit,
  map_lit, set_lit) that spans that line and has consecutive children on
  that line, or nil if no breakable form is found. Forms within ignored
  ranges are skipped."
  ([tree line]
   (find-breakable-form tree line #{}))
  ([tree line ignored-ranges]
   (first (find-breakable-forms tree line ignored-ranges))))

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
              edits
              (into
               []
               (keep
                (fn [[prev-child next-child]]
                  (let [end-byte (element-end-offset prev-child)
                        start-byte (element-start-offset next-child)]
                    (when (> start-byte end-byte)
                      {:start end-byte
                       :end start-byte
                       :replacement
                       (if (and
                            (= :comment (node/node-type next-child))
                            (not=
                             (second (node/node-line-range prev-child))
                             (node-start-line next-child)))
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

(defn- find-node-on-line
  "Find a named descendant node on the given 1-indexed line.
  Uses tree-sitter getNamedDescendant to find the deepest named
  node at the start of the line."
  [tree line]
  (let [root (node/root-node tree)
        row (dec line)
        p1 (io.github.treesitter.jtreesitter.Point. row 0)
        p2 (io.github.treesitter.jtreesitter.Point. row 1)
        result (.getNamedDescendant root p1 p2)]
    (when (.isPresent result)
      (.get result))))

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

(defn- find-unbroken-breakable-ancestor
  "Walk up from node to find the innermost breakable ancestor that
  still has consecutive children on the same line. Breaking such a
  form would separate its children and potentially reduce indent for
  descendants on the violating line."
  [node]
  (loop [n (node/node-parent node)]
    (when n
      (if (and (rules/breakable-node? n) (has-consecutive-children-on-line? n))
        n
        (recur (node/node-parent n))))))

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
  When prev-child is a comment (has trailing newline), only inserts indent."
  [prev-child next-child indent-col]
  (let [indent-spaces (apply str (repeat indent-col \space))]
    (cond
      ;; Comment on same line as prev: keep them together (no edit)
      (and (comment-node? next-child) (same-line? prev-child next-child)) nil
      ;; Prev is comment (ends with newline): just add indent
      (comment-node? prev-child)
      {:start (element-end-offset prev-child)
       :end (element-start-offset next-child)
       :replacement indent-spaces}
      ;; Normal case: add newline + indent
      :else
      {:start (element-end-offset prev-child)
       :end (element-start-offset next-child)
       :replacement (str "\n" indent-spaces)})))

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
  [exc-name exc-value children base-keep-count breakable-children
   indent-col]
  (let [join-edits (collect-collapse-edits exc-value)
        indent-str (apply str (repeat indent-col \space))
        split-edit {:start (element-end-offset exc-name)
                    :end (element-start-offset exc-value)
                    :replacement (str "\n" indent-str)}
        pair-edits
        (when (seq breakable-children)
          (generate-paired-edits
           (nth children (dec base-keep-count))
           breakable-children
           indent-col))
        all-edits
        (into
         (vec pair-edits)
         (if join-edits
           (cons split-edit join-edits)
           [split-edit]))]
    (when (seq all-edits)
      {:edits all-edits})))

(defn break-form
  "Generate edits to break a form across multiple lines.

  Takes a node and optional config map. Applies indent rules based on the
  form's head symbol:
  - :defn/:def rules keep name on first line (2 elements)
  - Default keeps only first element on first line

  For forms that use pair grouping (maps, cond, case), keeps related pairs
  together (key-value, test-result, etc.) and breaks only between pairs.

  For pair-grouped forms with exceeding pairs:
  - If a pair value would still exceed the limit at the indent position
    (single-line, breakable), backtracks to split the pair onto its own line
  - If the value is multi-line and breakable, collapses it to a single line
    and moves to own line at indent-col for re-breaking

  When breaking repositions children that are already multi-line, also
  collapses them so the next iteration re-breaks at the correct indent.

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
           ;; For pair-grouped forms, check if any pair exceeds limit
           exceeding-pair (when (rules/uses-pair-grouping? node config)
                            (find-exceeding-pair node rule max-length))
           [exc-name exc-value] exceeding-pair
           ;; Phase 3: multi-line breakable value — un-break and move
           ;; to own line. Phase 2 already broke it in-place but the
           ;; first line still exceeds.
           phase-3? (and
                     exceeding-pair
                     (rules/breakable-node? exc-value)
                     (not (single-line-node? exc-value)))
           ;; Non-binding pair split: for cond/case/condp/cond->, split
           ;; the exceeding pair immediately rather than deferring via
           ;; Phase 1. Only splits when the pair would still exceed at
           ;; its indent position, not just at the pre-break column.
           split-pair?
           (and
            exceeding-pair
            (not phase-3?)
            (contains? rules/non-binding-pair-rules rule)
            (let [pair-width (-
                              (first-line-end-column exc-value)
                              (form-start-column exc-name))]
              (> (+ indent-col pair-width) max-length)))
           breakable-children (drop base-keep-count children)]
       (cond
         (or phase-3? split-pair?)
         (break-exceeding-pair
          exc-name exc-value children base-keep-count
          breakable-children indent-col)
         ;; Normal breaking — if a pair value would still exceed at
         ;; the indent position, backtrack to split the pair.
         :else
         (when (seq breakable-children)
           (let [last-kept (nth children (dec base-keep-count))
                 edits
                 (if (rules/uses-pair-grouping? node config)
                   (generate-paired-edits
                    last-kept
                    breakable-children
                    indent-col)
                   (generate-sequential-edits
                    last-kept
                    breakable-children
                    indent-col))]
             (when (seq edits)
               (if (and
                    exceeding-pair
                    (single-line-node? exc-value)
                    (rules/breakable-node? exc-value)
                    (let [pair-width
                          (- (first-line-end-column exc-value)
                             (form-start-column exc-name))]
                      (> (+ indent-col pair-width) max-length)))
                 (break-exceeding-pair
                  exc-name exc-value children base-keep-count
                  breakable-children indent-col)
                 (let [collapse-edits
                       (collapse-repositioned-children
                        breakable-children)]
                   {:edits
                    (into edits collapse-edits)}))))))))))

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
  re-break at the correct position."
  [breakable-children]
  (into
   []
   (comp
    (remove single-line-node?)
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
  (some
   (fn [[s e]]
     (and (<= s start) (<= end e)))
   broken-ranges))

(defn try-collect-edits
  "Collect edits for a form if they are new, change source, and don't overlap.
  Returns [updated-state edits] on success, [state nil] otherwise.
  State is a map with :seen (set of byte ranges) and :collected (vec of edits).
  Public for use by reformat.clj batch edit collection."
  [state source form edits]
  (let [{:keys [seen collected]} state
        range (node/node-range form)]
    (if (or (seen range)
            (not (seq edits))
            (not (edits-change-source? source edits))
            (edits-overlap? collected edits))
      [state nil]
      [(-> state
           (update :seen conj range)
           (update :collected into edits))
       edits])))

(defn- try-guarded-break
  "Try to break a form, guarding against inside-broken-form and already-seen.
  Returns [updated-state result-map] on success, [state nil] otherwise."
  [state source form config]
  (let [range (node/node-range form)]
    (if (or (inside-broken-form? (:seen state) range)
            ((:seen state) range))
      [state nil]
      (let [result (break-form form config)
            edits (:edits result)
            [new-state collected-edits]
            (try-collect-edits state source form edits)]
        (if collected-edits
          [new-state result]
          [new-state nil])))))

(defn- try-first-guarded-break
  "Try to break forms in order, returning first success.
  Returns [updated-state result-map] or [state nil]."
  [state source forms config]
  (reduce
   (fn [[state _] form]
     (let [[new-state result] (try-guarded-break
                               state source form config)]
       (if result
         (reduced [new-state result])
         [new-state nil])))
   [state nil]
   forms))

(defn- break-on-line
  "Try strategies to break forms on a single long line.
  Tries ancestor forms first, then direct forms, then the innermost
  unbroken breakable ancestor of any node on the line.
  Returns [updated-state result] or [state nil]."
  [state source tree line ignored-ranges config]
  (let [forms (find-breakable-forms tree line ignored-ranges)
        ancestor-forms
        (sort-by
         (fn [f]
           (let [[s e] (node/node-range f)]
             (- s e)))
         (into
          []
          (comp
           (mapcat find-ancestors-needing-sibling-separation)
           (filter #(not (node-in-ignored-range? % ignored-ranges)))
           (distinct))
          forms))
        [state result] (try-first-guarded-break
                        state source ancestor-forms config)]
    (if result
      [state result]
      (let [[state result] (try-first-guarded-break
                            state source forms config)]
        (if result
          [state result]
          (if-let [anc (some-> (find-node-on-line tree line)
                               find-unbroken-breakable-ancestor)]
            (try-guarded-break state source anc config)
            [state nil]))))))

(defn- try-break-on-lines
  "Break the outermost form on every long line in a single pass.

  Breadth-first: breaks all outermost forms across all long lines before
  descending into sub-forms. Deduplicates by byte range so a form spanning
  multiple long lines is only broken once. Skips forms whose edits would
  overlap with already-collected edits (retried next iteration). Skips
  forms contained within an already-broken form to avoid using stale
  column positions for indent computation.
  Falls back to deeper forms when the outermost form on a line produces
  no change.
  Returns the new source if any forms were broken, nil otherwise."
  [source tree long-lines ignored-ranges config]
  (let [{:keys [all-edits]}
        (reduce
         (fn [state line]
           (let [[state result] (break-on-line
                                 state source tree line
                                 ignored-ranges config)]
             (if result
               (update state :all-edits into (:edits result))
               state)))
         {:seen #{} :collected [] :all-edits []}
         long-lines)]
    (when (seq all-edits)
      (let [new-source (apply-edits source all-edits)]
        (when (not= new-source source)
          new-source)))))

;;; Multiline child breaking

(defn- has-multiline-child?
  "Returns true if any named child of node spans multiple lines."
  [node]
  (some
   (fn [child]
     (let [[start-line end-line] (node/node-line-range child)]
       (and start-line end-line (not= start-line end-line))))
   (node/named-children node)))

(defn- needs-multiline-child-breaking?
  "Returns true if node is breakable, not pair-grouped, has a multi-line
  child, and has consecutive named children sharing a line.
  Pair-grouped forms (maps, binding vectors, cond, etc.) are excluded
  because pair-breaking already handles their layout."
  [node config]
  (and
   (rules/breakable-node? node)
   (not (rules/uses-pair-grouping? node config))
   (has-consecutive-children-on-line? node)
   (has-multiline-child? node)))

(defn- generate-multiline-child-break-edits
  "Generate edits to separate children sharing lines in a form with
  multi-line children. Inserts breaks between children that share a
  line. Also collapses repositioned pair-grouped children (maps,
  binding vectors) whose internal indentation would be stale at
  their new column position."
  [node config]
  (let [rule (rules/get-effective-rule node config)
        children (node/named-children node)
        base-keep-count (rules/elements-to-keep-on-first-line rule)
        indent-col (indent-column node rule)
        breakable-children (drop base-keep-count children)]
    (when (seq breakable-children)
      (let [last-kept (nth children (dec base-keep-count))
            all-pairs (cons
                       [last-kept (first breakable-children)]
                       (partition 2 1 breakable-children))
            sharing-line?
            (fn [[prev-child next-child]]
              (let [[_ prev-end] (node/node-line-range prev-child)
                    next-start (node-start-line next-child)]
                (= prev-end next-start)))
            pairs-to-break (filterv sharing-line? all-pairs)
            break-edits
            (into
             []
             (keep
              (fn [[prev-child next-child]]
                (make-break-edit prev-child next-child indent-col)))
             pairs-to-break)
            ;; Collapse pair-grouped children moving to a new column.
            ;; Only pair-grouped forms have stale pair indentation
            ;; after repositioning; other forms are re-indented by
            ;; forced-breaks or fix-source on the next iteration.
            moved-pair-children
            (into
             []
             (comp
              (map second)
              (filter
               (fn [child]
                 (and (rules/uses-pair-grouping? child config)
                      (not= (form-start-column child) indent-col)))))
             pairs-to-break)
            collapse-edits
            (collapse-repositioned-children moved-pair-children)]
        (when (seq break-edits)
          (into break-edits collapse-edits))))))

(defn- find-multiline-child-forms
  "Walk tree pre-order to find forms needing multiline-child breaking.
  Does not recurse into found forms — their children may be collapsed,
  so inner forms are deferred to the next iteration."
  [root config]
  (let [results (transient [])]
    (letfn
     [(walk
        [node]
        (when node
          (if (needs-multiline-child-breaking? node config)
            (conj! results node)
            (doseq [child (node/named-children node)]
              (walk child)))))]
      (walk root))
    (persistent! results)))

(defn- try-break-multiline-children
  "Find and break forms with multi-line children sharing lines.
  Returns the new source if any changes were made, nil otherwise."
  [source tree config]
  (let [root (node/root-node tree)
        forms (find-multiline-child-forms root config)]
    (when (seq forms)
      (let [edits
            (into
             []
             (mapcat
              (fn [form]
                (generate-multiline-child-break-edits form config)))
             forms)]
        (when (and (seq edits) (edits-change-source? source edits))
          (let [new-source (apply-edits source edits)]
            (when (not= new-source source)
              new-source)))))))

(defn fix-source
  "Fix line length violations and multiline-child sharing in source code.

  Takes a source string and config map with :line-length. Iteratively breaks
  forms until all lines fit or only unbreakable atoms remain. Also separates
  children sharing lines with multi-line siblings. Returns the fixed source
  string. Forms preceded by #_:line-breaker/ignore are not modified.

  The algorithm uses breadth-first breaking:
  1. Find lines exceeding max-length
  2. Collect ignored byte ranges (re-collected each pass as positions shift)
  3. Break the outermost form on every long line in a single pass
  4. Separate children sharing lines with multi-line siblings
  5. Re-parse and repeat until stable

  This ensures sibling forms at the same depth are all broken before
  descending into sub-forms."
  [source config]
  (let [max-length (get config :line-length 80)]
    (loop [source source
           iteration 0]
      (if (>= iteration max-iterations)
        source
        (let [long-lines (find-long-lines source max-length)
              tree (parser/parse-source source)
              ignored-ranges
              (check/find-ignored-byte-ranges tree)
              line-source
              (when (seq long-lines)
                (try-break-on-lines
                 source tree long-lines ignored-ranges config))
              ;; Check for multiline-child sharing after line fixes
              source' (or line-source source)
              tree' (if line-source
                      (parser/parse-source source')
                      tree)
              child-source
              (try-break-multiline-children source' tree' config)
              new-source (or child-source source')]
          (if (not= new-source source)
            (recur new-source (inc iteration))
            source))))))

