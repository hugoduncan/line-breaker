(ns line-breaker.fix
  "Line breaking functions for reformatting Clojure code.

  Provides functions to identify breakable forms, generate line break edits,
  and apply those edits to source code."
  (:require
   [line-breaker.check :as check]
   [line-breaker.treesitter.node :as node]
   [line-breaker.treesitter.parser :as parser]))

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

;;; Breakable node detection

(def ^:private breakable-types
  "Node types that can be broken across multiple lines.
  Includes anonymous functions and reader conditionals which have
  list-like structure."
  #{:list_lit
    :vec_lit
    :map_lit
    :set_lit
    :anon_fn_lit
    :read_cond_lit
    :splicing_read_cond_lit})

;;; Indent rules

;;; NOTE: Keep in sync with default-force-break-rules in reformat.clj
(def ^:private default-indent-rules
  "Default mappings from form head symbols to indent rules.
  :defn - keep name on first line
  :def - keep name on first line
  :fn - keep arg vector on first line
  :binding - keep binding vector on first line
  :if - keep test on first line
  :case - keep test-expr on first line, pair group remaining
  :cond - pair group all clauses
  :condp - keep pred+expr on first line, pair group remaining
  :cond-> - keep initial expr on first line, pair group remaining
  :try - body on next line
  :do - body on next line"
  {'defn :defn
   'defn- :defn
   'defmacro :defn
   'defmethod :defn
   'deftest :defn
   'def :def
   'defonce :def
   'defmulti :def
   'ns :def
   'fn :fn
   'bound-fn :fn
   'let :binding
   'when-let :binding
   'if-let :binding
   'binding :binding
   'doseq :binding
   'for :binding
   'loop :binding
   'with-open :binding
   'with-local-vars :binding
   'if :if
   'if-not :if
   'when :if
   'when-not :if
   'when-first :if
   'testing :if
   'case :case
   'cond :cond
   'condp :condp
   'cond-> :cond->
   'cond->> :cond->
   'try :try
   'do :do})

(defn get-head-symbol
  "Get the head symbol of a list_lit node as a symbol.
  Returns nil if node is not a list_lit or has no sym_lit first child."
  [node]
  (when (= :list_lit (node/node-type node))
    (let [first-child (first (node/named-children node))]
      (when (= :sym_lit (node/node-type first-child))
        (symbol (node/node-text first-child))))))

(defn- get-indent-rule
  "Look up the indent rule for a node.
  Checks config's :indents map first, then falls back to defaults.
  Returns nil if no rule applies (use default breaking)."
  [node config]
  (when-let [head-sym (get-head-symbol node)]
    (or
     (get-in config [:indents head-sym])
     (get default-indent-rules head-sym))))

(defn- binding-vector?
  "Returns true if node is the binding vector of a :binding form.
  A binding vector is a vec_lit that is the second child of a form
  with the :binding indent rule (let, for, doseq, loop, etc.)."
  [node config]
  (and
   (= :vec_lit (node/node-type node))
   (when-let [parent (node/node-parent node)]
     (and
      (= :binding (get-indent-rule parent config))
      (= node (second (node/named-children parent)))))))

(defn- metadata-node?
  "Returns true if node is a metadata (meta_lit) node."
  [node]
  (= :meta_lit (node/node-type node)))

(defn- metadata-wrapped?
  "Returns true if node's first named child is a meta_lit.
  Such forms need special handling when breaking: keep metadata + first
  content element together, indent to first content element's position."
  [node]
  (when-let [first-child (first (node/named-children node))]
    (metadata-node? first-child)))

(defn elements-to-keep-on-first-line
  "Number of elements to keep on the first line based on indent rule.
  :defn/:def keep 2 (head + name)
  :fn keeps 2 (head + arg vector)
  :binding keeps 2 (head + binding vector)
  :if keeps 2 (head + test)
  :case keeps 2 (head + test-expr)
  :cond keeps 1 (head only, pair group remaining)
  :condp keeps 3 (head + pred + expr, pair group remaining)
  :cond-> keeps 2 (head + initial-expr, pair group remaining)
  :try/:do keep 1 (body on next line)
  :map keeps 2 (first key-value pair)
  :binding-vector keeps 2 (first binding pair)
  :metadata-wrapped keeps 2 (metadata + first content element)
  Default keeps 1 (head only)."
  [rule]
  (case rule
    (:defn
     :def
     :fn
     :binding
     :if
     :case
     :cond->
     :map
     :binding-vector
     :metadata-wrapped)
    2
    :condp 3
    (:cond :try :do) 1
    1))

(defn get-effective-rule
  "Get the effective indent rule for a node, considering both head symbol
  and node type. Maps use :map rule, binding vectors use :binding-vector.
  Forms with metadata as first child use :metadata-wrapped rule."
  [node config]
  (or
   (get-indent-rule node config)
   (when (= :map_lit (node/node-type node))
     :map)
   (when (binding-vector? node config)
     :binding-vector)
   (when (metadata-wrapped? node)
     :metadata-wrapped)))

(defn uses-pair-grouping?
  "Returns true if the node should use pair grouping when breaking.
  Pair grouping keeps related pairs together (key-value, test-result, etc.)."
  [node config]
  (let [rule (get-effective-rule node config)]
    (#{:cond :condp :case :cond-> :map :binding-vector} rule)))

(def ^:private non-binding-pair-rules
  "Pair-grouping rules for non-binding forms (cond, case, condp, cond->)."
  #{:cond :condp :case :cond->})

(defn breakable-node?
  "Returns true if node is a breakable collection type."
  [node]
  (contains? breakable-types (node/node-type node)))

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
         (not (metadata-node? node)))
    (if (metadata-wrapped? node)
      ;; Metadata-wrapped form: breakable but don't descend into children
      (when (and
             (breakable-node? node)
             (form-needs-breaking-on-line? node line))
        [node])
      ;; Normal form: check self and recurse into children
      (let [self
            (when (and
                   (breakable-node? node)
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
           (if (and (has-preceding-sibling-on-line? n) (breakable-node? parent))
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
                   (elements-to-keep-on-first-line rule))
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
      (if (and (breakable-node? n) (has-consecutive-children-on-line? n))
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

(defn- has-stale-indent?
  "Returns true if node is multi-line and its second named child
  is indented more than expected based on the node's current position.
  This detects forms that were broken at one column then moved to
  another without adjusting internal indentation."
  [node config]
  (when (and (breakable-node? node) (not (single-line-node? node)))
    (let [children (node/named-children node)
          rule (get-effective-rule node config)
          expected-col (indent-column node rule)]
      (when (>= (count children) 2)
        (let [second-child (second children)
              actual-col (form-start-column second-child)]
          (and
           (not (same-line? (first children) second-child))
           (> actual-col (+ expected-col 2))))))))

(defn- find-stale-indent-ancestor
  "Walk up from node to find the innermost breakable ancestor with
  stale indentation — its children are at a column inconsistent with
  the form's current position."
  [node config]
  (loop [n (node/node-parent node)]
    (when n
      (if (has-stale-indent? n config)
        n
        (recur (node/node-parent n))))))

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

(defn- break-form-phase-3
  "Phase 3 breaking: un-break a multi-line value, split name/value onto
  separate lines, and generate inter-pair edits."
  [exc-name exc-value children base-keep-count breakable-children
   indent-col]
  (let [join-edits (join-form-edits exc-value)
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
      all-edits)))

(defn- break-form-split-pair
  "Split an exceeding pair onto separate lines and generate inter-pair edits."
  [exc-name exc-value children base-keep-count breakable-children
   indent-col]
  (let [indent-str (apply str (repeat indent-col \space))
        split-edit {:start (element-end-offset exc-name)
                    :end (element-start-offset exc-value)
                    :replacement (str "\n" indent-str)}
        pair-edits
        (when (seq breakable-children)
          (generate-paired-edits
           (nth children (dec base-keep-count))
           breakable-children
           indent-col))
        all-edits (into (vec pair-edits) [split-edit])]
    (when (seq all-edits)
      all-edits)))

(defn break-form
  "Generate edits to break a form across multiple lines.

  Takes a node and optional config map. Applies indent rules based on the
  form's head symbol:
  - :defn/:def rules keep name on first line (2 elements)
  - Default keeps only first element on first line

  For forms that use pair grouping (maps, cond, case), keeps related pairs
  together (key-value, test-result, etc.) and breaks only between pairs.

  Three-phase escalation for pair-grouped forms with exceeding pairs:
  - Phase 1: value is single-line and breakable — defer splitting, let the
    iterative loop break the value in-place
  - Phase 3: value is multi-line and breakable — un-break value (collapse to
    single line), move to own line at indent-col, let iterative loop re-break

  Comments on the same line as the preceding element stay attached.
  Comments include their trailing newline, so no extra newline is added after.

  Returns a vector of edits replacing whitespace between consecutive
  elements with newline+indent. Each edit is {:start n :end m :replacement s}.
  Returns nil if node is nil or has fewer than 2 children."
  ([node]
   (break-form node {}))
  ([node config]
   (when node
     (let [children (node/named-children node)
           rule (get-effective-rule node config)
           indent-col (indent-column node rule)
           base-keep-count (elements-to-keep-on-first-line rule)
           max-length (get config :line-length)
           ;; For pair-grouped forms, check if any pair exceeds limit
           exceeding-pair (when (uses-pair-grouping? node config)
                            (find-exceeding-pair node rule max-length))
           [exc-name exc-value] exceeding-pair
           ;; Phase 3: multi-line breakable value — un-break and move
           ;; to own line. Phase 2 already broke it in-place but the
           ;; first line still exceeds.
           phase-3? (and
                     exceeding-pair
                     (breakable-node? exc-value)
                     (not (single-line-node? exc-value)))
           ;; Non-binding pair split: for cond/case/condp/cond->, split
           ;; the exceeding pair immediately rather than deferring via
           ;; Phase 1. Only splits when the pair would still exceed at
           ;; its indent position, not just at the pre-break column.
           split-pair?
           (and
            exceeding-pair
            (not phase-3?)
            (contains? non-binding-pair-rules rule)
            (let [pair-width (-
                              (first-line-end-column exc-value)
                              (form-start-column exc-name))]
              (> (+ indent-col pair-width) max-length)))
           breakable-children (drop base-keep-count children)]
       (cond
         phase-3?
         (break-form-phase-3
          exc-name exc-value children base-keep-count
          breakable-children indent-col)
         split-pair?
         (break-form-split-pair
          exc-name exc-value children base-keep-count
          breakable-children indent-col)
         ;; Normal breaking (Phase 1 deferral is implicit — single-line
         ;; breakable values are kept together by generate-paired-edits)
         :else
         (when (seq breakable-children)
           (let [last-kept (nth children (dec base-keep-count))
                 edits
                 (if (uses-pair-grouping? node config)
                   (generate-paired-edits
                    last-kept
                    breakable-children
                    indent-col)
                   (generate-sequential-edits
                    last-kept
                    breakable-children
                    indent-col))]
             (when (seq edits)
               edits))))))))

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

(defn- inside-broken-form?
  "Returns true if range is contained within any range in broken-ranges.
  A child form whose parent was already broken in this pass should not
  be broken until the next pass, when it will have correct column
  positions after re-parsing."
  [broken-ranges [start end]]
  (some (fn [[s e]]
          (and (<= s start) (<= end e))) broken-ranges))

(defn- try-collect-edits
  "Collect edits for a form if they are new, change source, and don't overlap.
  Returns [updated-state edits] on success, [state nil] otherwise.
  State is a map with :seen (set of byte ranges) and :collected (vec of edits)."
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
  Returns [updated-state edits] on success, [state nil] otherwise."
  [state source form config]
  (let [range (node/node-range form)]
    (if (or (inside-broken-form? (:seen state) range)
            ((:seen state) range))
      [state nil]
      (try-collect-edits
       state source form (break-form form config)))))

(defn- try-first-guarded-break
  "Try to break forms in order, returning first success.
  Returns [updated-state edits] or [state nil]."
  [state source forms config]
  (reduce
   (fn [[state _] form]
     (let [[new-state edits] (try-guarded-break
                              state source form config)]
       (if edits
         (reduced [new-state edits])
         [new-state nil])))
   [state nil]
   forms))

(defn- break-on-line
  "Try strategies to break forms on a single long line.
  Tries ancestor forms first, then direct forms, then fallback paths.
  Returns [updated-state edits] or [state nil]."
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
        [state edits] (try-first-guarded-break
                       state source ancestor-forms config)]
    (if edits
      [state edits]
      (let [[state edits] (try-first-guarded-break
                           state source forms config)]
        (if edits
          [state edits]
          (if-let [n (find-node-on-line tree line)]
            (let [[state edits]
                  (if-let [anc (find-unbroken-breakable-ancestor n)]
                    (try-guarded-break state source anc config)
                    [state nil])]
              (if edits
                [state edits]
                ;; Collapse a stale-indent ancestor so
                ;; fix-source re-breaks at the correct position.
                (if-let [stale (find-stale-indent-ancestor n config)]
                  (try-collect-edits
                   state source stale (collect-collapse-edits stale))
                  [state nil])))
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
           (let [[state edits] (break-on-line
                                state source tree line
                                ignored-ranges config)]
             (if edits
               (update state :all-edits into edits)
               state)))
         {:seen #{} :collected [] :all-edits []}
         long-lines)]
    (when (seq all-edits)
      (let [new-source (apply-edits source all-edits)]
        (when (not= new-source source)
          new-source)))))

(defn fix-source
  "Fix line length violations in source code.

  Takes a source string and config map with :line-length. Iteratively breaks
  forms until all lines fit or only unbreakable atoms remain. Returns the
  fixed source string. Forms preceded by #_:line-breaker/ignore are not
  modified.

  The algorithm uses breadth-first breaking:
  1. Find lines exceeding max-length
  2. Collect ignored byte ranges (re-collected each pass as positions shift)
  3. Break the outermost form on every long line in a single pass
  4. Re-parse and repeat until no violations or no breakable forms

  This ensures sibling forms at the same depth are all broken before
  descending into sub-forms."
  [source config]
  (let [max-length (get config :line-length 80)]
    (loop [source source
           iteration 0]
      (if (>= iteration max-iterations)
        source
        (let [long-lines (find-long-lines source max-length)]
          (if (empty? long-lines)
            source
            (let [tree (parser/parse-source source)
                  ;; Re-collect ignored ranges (positions shift after edits)
                  ignored-ranges (check/find-ignored-byte-ranges tree)
                  new-source (try-break-on-lines
                              source
                              tree
                              long-lines
                              ignored-ranges
                              config)]
              (if new-source
                (recur new-source (inc iteration))
                source))))))))

