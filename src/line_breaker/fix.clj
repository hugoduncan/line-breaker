(ns line-breaker.fix
  "Line breaking functions for reformatting Clojure code.

  Provides functions to identify breakable forms, generate line break edits,
  and apply those edits to source code."
  (:require
   [line-breaker.check :as check]
   [line-breaker.treesitter.node :as node]
   [line-breaker.treesitter.parser :as parser]))

;;; Edit application

(defn- byte-offset->char-index
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

  Returns the modified source string."
  [source edits]
  (reduce
   (fn [s {:keys [start end replacement]}]
     (let [start-char (byte-offset->char-index s start)
           end-char (byte-offset->char-index s end)]
       (str (subs s 0 start-char) replacement (subs s end-char))))
   source
   (sort-by :start > edits)))

;;; Breakable node detection

(def ^:private breakable-types
  "Node types that can be broken across multiple lines.
  Includes anonymous functions and reader conditionals which have
  list-like structure."
  #{:list_lit :vec_lit :map_lit :set_lit
    :anon_fn_lit :read_cond_lit :splicing_read_cond_lit})

;;; Indent rules

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
  {'defn            :defn
   'defn-           :defn
   'defmacro        :defn
   'defmethod       :defn
   'deftest         :defn
   'def             :def
   'defonce         :def
   'defmulti        :def
   'fn              :fn
   'bound-fn        :fn
   'let             :binding
   'when-let        :binding
   'if-let          :binding
   'binding         :binding
   'doseq           :binding
   'for             :binding
   'loop            :binding
   'with-open       :binding
   'with-local-vars :binding
   'if              :if
   'if-not          :if
   'when            :if
   'when-not        :if
   'when-first      :if
   'case            :case
   'cond            :cond
   'condp           :condp
   'cond->          :cond->
   'cond->>         :cond->
   'try             :try
   'do              :do})

(defn- get-head-symbol
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
    (or (get-in config [:indents head-sym])
        (get default-indent-rules head-sym))))

(defn- binding-vector?
  "Returns true if node is the binding vector of a :binding form.
  A binding vector is a vec_lit that is the second child of a form
  with the :binding indent rule (let, for, doseq, loop, etc.)."
  [node config]
  (and (= :vec_lit (node/node-type node))
       (when-let [parent (node/node-parent node)]
         (and (= :binding (get-indent-rule parent config))
              (= node (second (node/named-children parent)))))))

(defn- elements-to-keep-on-first-line
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
  Default keeps 1 (head only)."
  [rule]
  (case rule
    (:defn :def :fn :binding :if :case :cond-> :map :binding-vector) 2
    :condp 3
    (:cond :try :do) 1
    1))

(defn- get-effective-rule
  "Get the effective indent rule for a node, considering both head symbol
  and node type. Maps use :map rule, binding vectors use :binding-vector."
  [node config]
  (or (get-indent-rule node config)
      (when (= :map_lit (node/node-type node))
        :map)
      (when (binding-vector? node config)
        :binding-vector)))

(defn- uses-pair-grouping?
  "Returns true if the node should use pair grouping when breaking.
  Pair grouping keeps related pairs together (key-value, test-result, etc.)."
  [node config]
  (let [rule (get-effective-rule node config)]
    (#{:cond :condp :case :cond-> :map :binding-vector} rule)))

(defn breakable-node?
  "Returns true if node is a breakable collection type."
  [node]
  (contains? breakable-types (node/node-type node)))

;;; Finding breakable forms

(defn- metadata-node?
  "Returns true if node is a metadata (meta_lit) node."
  [node]
  (= :meta_lit (node/node-type node)))

(defn- has-metadata-child?
  "Returns true if any named child of node is a meta_lit.
  Forms with metadata attached should be treated as atomic for breaking
  purposes - we don't descend into their children to find breakable forms."
  [node]
  (some metadata-node? (node/named-children node)))

(defn- node-contains-line?
  "Returns true if node spans the given 1-indexed line number."
  [node line]
  (when-let [[start-line end-line] (node/node-line-range node)]
    (<= start-line line end-line)))

(defn- node-start-line
  "Get the 1-indexed start line of a node."
  [node]
  (first (node/node-line-range node)))

(defn- form-needs-breaking-on-line?
  "Returns true if form has consecutive children both on the target line.
  A form already broken (children on separate lines) returns false."
  [node line]
  (let [children (node/named-children node)]
    (some (fn [[prev-child next-child]]
            (let [prev-line (node-start-line prev-child)
                  next-line (node-start-line next-child)]
              (= prev-line next-line line)))
          (partition 2 1 children))))

(defn- node-in-ignored-range?
  "Returns true if node falls within any of the ignored byte ranges.
  Ignored ranges are [start-byte end-byte] pairs where start is inclusive
  and end is exclusive."
  [node ignored-ranges]
  (when-let [[start-byte end-byte] (node/node-range node)]
    (some (fn [[ign-start ign-end]]
            (and (<= ign-start start-byte)
                 (<= end-byte ign-end)))
          ignored-ranges)))

(defn- find-breakable-forms-on-line
  "Find all breakable nodes containing the given line that need breaking.

  Returns a vector of nodes from outermost to innermost. Only includes
  forms that have consecutive children on the target line. Skips forms
  that fall within ignored ranges.

  Forms with metadata attached (having a meta_lit child) are treated as
  atomic - they are not considered breakable and the search does not
  descend into their children."
  [node line ignored-ranges]
  (when (and (node-contains-line? node line)
             (not (node-in-ignored-range? node ignored-ranges)))
    (if (has-metadata-child? node)
      ;; Form has metadata - treat as atomic, don't break or descend
      []
      (let [self (when (and (breakable-node? node)
                            (form-needs-breaking-on-line? node line))
                   [node])
            children-results
            (mapcat #(find-breakable-forms-on-line % line ignored-ranges)
                    (node/named-children node))]
        (into (vec self) children-results)))))

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

;;; Form breaking

(defn- element-start-offset
  "Get the start byte offset of a node."
  [node]
  (first (node/node-range node)))

(defn- element-end-offset
  "Get the end byte offset of a node."
  [node]
  (second (node/node-range node)))

(defn- form-start-column
  "Get the column where the form starts (0-indexed)."
  [node]
  (:column (node/node-position node)))

(defn- comment-node?
  "Returns true if node is a comment."
  [node]
  (= :comment (node/node-type node)))

(defn- same-line?
  "Returns true if two nodes start on the same line."
  [node1 node2]
  (= (node-start-line node1) (node-start-line node2)))

(defn- make-break-edit
  "Create a break edit between two children.
  Returns nil if no edit needed (comment attached to preceding element).
  When prev-child is a comment (has trailing newline), only inserts indent."
  [prev-child next-child indent-col]
  (let [indent-spaces (apply str (repeat indent-col \space))]
    (cond
      ;; Comment on same line as prev: keep them together (no edit)
      (and (comment-node? next-child)
           (same-line? prev-child next-child))
      nil

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

(defn- generate-paired-edits
  "Generate edits for pair-grouped breaking.
  Groups elements in pairs and breaks only between pairs."
  [last-kept breakable-children indent-col]
  (let [pairs (partition-all 2 breakable-children)
        ;; For each pair, break before its first element.
        ;; Prev elem: last-kept for 1st pair, last of prev pair for rest
        prev-elements (cons last-kept (map last (butlast pairs)))
        first-of-pairs (map first pairs)
        ;; Generate edits between prev-element and first-of-pair
        break-points (map vector prev-elements first-of-pairs)]
    (into []
          (keep (fn [[prev-child next-child]]
                  (make-break-edit prev-child next-child indent-col)))
          break-points)))

(defn- generate-sequential-edits
  "Generate edits for sequential (non-paired) breaking.
  Each element gets its own line."
  [last-kept breakable-children indent-col]
  (let [all-pairs (cons [last-kept (first breakable-children)]
                        (partition 2 1 breakable-children))]
    (into []
          (keep (fn [[prev-child next-child]]
                  (make-break-edit prev-child next-child indent-col)))
          all-pairs)))

(defn- indent-column
  "Calculate the indent column for broken elements.
  - :binding-vector uses +1 (align to first element after bracket)
  - Forms with an indent rule use +2 (standard Clojure body indentation)
  - Plain function calls and data structures use +1 (align to first element)"
  [node rule]
  (let [base-col (form-start-column node)]
    (cond
      (= :binding-vector rule) (+ 1 base-col)
      (some? rule)             (+ 2 base-col)
      :else                    (+ 1 base-col))))

(defn break-form
  "Generate edits to break a form across multiple lines.

  Takes a node and optional config map. Applies indent rules based on the
  form's head symbol:
  - :defn/:def rules keep name on first line (2 elements)
  - Default keeps only first element on first line

  For forms that use pair grouping (maps, cond, case), keeps related pairs
  together (key-value, test-result, etc.) and breaks only between pairs.

  Comments on the same line as the preceding element stay attached.
  Comments include their trailing newline, so no extra newline is added after.

  Returns a vector of edits replacing whitespace between consecutive
  elements with newline+indent. Each edit is {:start n :end m :replacement s}.
  Returns nil if node is nil or has fewer than 2 children."
  ([node] (break-form node {}))
  ([node config]
   (when node
     (let [children (node/named-children node)
           rule (get-effective-rule node config)
           indent-col (indent-column node rule)
           keep-count (elements-to-keep-on-first-line rule)
           ;; Elements that need breaking: skip the ones kept on first line
           breakable-children (drop keep-count children)]
       (when (seq breakable-children)
         (let [;; Get the last element that stays on first line
               last-kept (nth children (dec keep-count))
               ;; Generate edits based on whether pair grouping applies
               edits (if (uses-pair-grouping? node config)
                       (generate-paired-edits
                        last-kept breakable-children indent-col)
                       (generate-sequential-edits
                        last-kept breakable-children indent-col))]
           (when (seq edits)
             edits)))))))

;;; Line length checking

(defn find-long-lines
  "Find 1-indexed line numbers exceeding max-length.
  Returns a vector of line numbers."
  [source max-length]
  (mapv :line (check/find-violations source max-length)))

;;; Iterative multi-pass breaking

(def ^:private max-iterations
  "Maximum number of breaking passes to prevent infinite loops.
  100 is generous for deeply nested forms (typical code rarely needs more
  than 10-20 passes) while catching bugs that cause infinite loops."
  100)

(defn- try-break-forms
  "Try breaking each form in order until one produces a change.
  Returns the new source if a form was successfully broken, nil otherwise."
  [source forms config]
  (reduce
   (fn [_ form]
     (let [edits (break-form form config)]
       (when (seq edits)
         (let [new-source (apply-edits source edits)]
           (when (not= new-source source)
             (reduced new-source))))))
   nil
   forms))

(defn fix-source
  "Fix line length violations in source code.

  Takes a source string and config map with :line-length. Iteratively breaks
  forms until all lines fit or only unbreakable atoms remain. Returns the
  fixed source string. Forms preceded by #_:line-breaker/ignore are not
  modified.

  The algorithm:
  1. Find lines exceeding max-length
  2. Collect ignored byte ranges (re-collected each pass as positions shift)
  3. Find breakable forms on first violating line (outermost to innermost)
  4. Try breaking each form until one produces a change
  5. Re-parse and repeat until no violations or no breakable forms"
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
                  first-long-line (first long-lines)
                  breakable-forms
                  (find-breakable-forms tree first-long-line ignored-ranges)
                  new-source (try-break-forms source breakable-forms config)]
              (if new-source
                (recur new-source (inc iteration))
                source))))))))
