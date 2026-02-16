(ns line-breaker.rules
  "Indent rules and form classification for Clojure code breaking.

  Defines which forms are breakable, how many elements to keep on the
  first line, and which forms use pair grouping."
  (:require
   [line-breaker.treesitter.node :as node]))

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

(defn breakable-node?
  "Returns true if node is a breakable collection type."
  [node]
  (contains? breakable-types (node/node-type node)))

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
  "Returns true if node is the binding vector of a :binding form."
  [node config]
  (and
   (= :vec_lit (node/node-type node))
   (when-let [parent (node/node-parent node)]
     (and
      (= :binding (get-indent-rule parent config))
      (= node (second (node/named-children parent)))))))

(defn metadata-node?
  "Returns true if node is a metadata (meta_lit) node."
  [node]
  (= :meta_lit (node/node-type node)))

(defn metadata-wrapped?
  "Returns true if node's first named child is a meta_lit."
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
  "Returns true if the node should use pair grouping when breaking."
  [node config]
  (let [rule (get-effective-rule node config)]
    (#{:cond :condp :case :cond-> :map :binding-vector} rule)))

(def non-binding-pair-rules
  "Pair-grouping rules for non-binding forms (cond, case, condp, cond->)."
  #{:cond :condp :case :cond->})
