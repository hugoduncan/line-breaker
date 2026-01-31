(ns line-sitter.fix
  "Line breaking functions for reformatting Clojure code.

  Provides functions to identify breakable forms, generate line break edits,
  and apply those edits to source code."
  (:require
   [clojure.string :as str]
   [line-sitter.check :as check]
   [line-sitter.treesitter.node :as node]
   [line-sitter.treesitter.parser :as parser]))

;;; Edit application

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
     (str (subs s 0 start) replacement (subs s end)))
   source
   (sort-by :start > edits)))

;;; Breakable node detection

(def ^:private breakable-types
  "Node types that can be broken across multiple lines."
  #{:list_lit :vec_lit :map_lit :set_lit})

;;; Indent rules

(def ^:private default-indent-rules
  "Default mappings from form head symbols to indent rules.
  :defn - keep name on first line
  :def - keep name on first line
  :fn - keep arg vector on first line
  :binding - keep binding vector on first line
  :if - keep test on first line
  :case - keep test-expr on first line
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

(defn- elements-to-keep-on-first-line
  "Number of elements to keep on the first line based on indent rule.
  :defn/:def keep 2 (head + name)
  :fn keeps 2 (head + arg vector)
  :binding keeps 2 (head + binding vector)
  :if keeps 2 (head + test)
  :case keeps 2 (head + test-expr)
  :try/:do keep 1 (body on next line)
  Default keeps 1 (head only)."
  [rule]
  (case rule
    (:defn :def :fn :binding :if :case) 2
    1))

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
  that fall within ignored ranges."
  [node line ignored-ranges]
  (when (and (node-contains-line? node line)
             (not (node-in-ignored-range? node ignored-ranges)))
    (let [self (when (and (breakable-node? node)
                          (form-needs-breaking-on-line? node line))
                 [node])
          children-results (mapcat #(find-breakable-forms-on-line % line ignored-ranges)
                                   (node/named-children node))]
      (into (vec self) children-results))))

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
   (first (find-breakable-forms-on-line (node/root-node tree) line ignored-ranges))))

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

(defn break-form
  "Generate edits to break a form across multiple lines.

  Takes a node and optional config map. Applies indent rules based on the
  form's head symbol:
  - :defn/:def rules keep name on first line (2 elements)
  - Default keeps only first element on first line

  Returns a vector of edits replacing whitespace between consecutive
  elements with newline+indent. Each edit is {:start n :end m :replacement s}.
  Returns nil if node is nil or has fewer than 2 children."
  ([node] (break-form node {}))
  ([node config]
   (when node
     (let [children (node/named-children node)
           indent-col (+ 2 (form-start-column node))
           indent-str (str "\n" (apply str (repeat indent-col \space)))
           rule (get-indent-rule node config)
           keep-count (elements-to-keep-on-first-line rule)
           ;; Elements that need breaking: skip the ones kept on first line
           breakable-children (drop keep-count children)]
       (when (seq breakable-children)
         (let [;; Get the last element that stays on first line
               last-kept (nth children (dec keep-count))
               ;; Create edit from last-kept to first breakable
               first-edit {:start (element-end-offset last-kept)
                           :end (element-start-offset (first breakable-children))
                           :replacement indent-str}
               ;; Create edits between remaining breakable children
               remaining-edits (into []
                                     (map (fn [[prev-child next-child]]
                                            {:start (element-end-offset prev-child)
                                             :end (element-start-offset next-child)
                                             :replacement indent-str}))
                                     (partition 2 1 breakable-children))]
           (into [first-edit] remaining-edits)))))))

;;; Line length checking

(defn find-long-lines
  "Find 1-indexed line numbers exceeding max-length.
  Returns a vector of line numbers."
  [source max-length]
  (into []
        (comp
         (map-indexed (fn [idx line]
                        (when (> (count line) max-length)
                          (inc idx))))
         (filter some?))
        (str/split-lines source)))

;;; Iterative multi-pass breaking

(def ^:private max-iterations
  "Maximum number of breaking passes to prevent infinite loops."
  100)

(defn fix-source
  "Fix line length violations in source code.

  Takes a source string and config map with :line-length. Iteratively breaks
  forms until all lines fit or only unbreakable atoms remain. Returns the
  fixed source string. Forms preceded by #_:line-sitter/ignore are not modified.

  The algorithm:
  1. Find lines exceeding max-length
  2. Collect ignored byte ranges (re-collected each pass since byte positions shift)
  3. Find outermost breakable form on first violating line (skipping ignored)
  4. Break that form
  5. Re-parse and repeat until no violations or no breakable forms"
  [source config]
  (let [max-length (get config :line-length 80)]
    (loop [src source
           iteration 0]
      (if (>= iteration max-iterations)
        src
        (let [long-lines (find-long-lines src max-length)]
          (if (empty? long-lines)
            src
            (let [tree (parser/parse-source src)
                  ;; Collect ignored ranges each pass since byte positions shift after edits
                  ignored-ranges (check/find-ignored-byte-ranges tree)
                  first-long-line (first long-lines)
                  breakable-form (find-breakable-form tree first-long-line ignored-ranges)]
              (if-not breakable-form
                src
                (let [edits (break-form breakable-form config)]
                  (if (empty? edits)
                    src
                    (recur (apply-edits src edits) (inc iteration))))))))))))
