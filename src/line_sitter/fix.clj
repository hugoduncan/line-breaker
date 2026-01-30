(ns line-sitter.fix
  "Line breaking functions for reformatting Clojure code.

  Provides functions to identify breakable forms, generate line break edits,
  and apply those edits to source code."
  (:require
   [line-sitter.treesitter.node :as node]))

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

(defn- find-breakable-forms-on-line
  "Find all breakable nodes containing the given line.

  Returns a vector of nodes from outermost to innermost."
  [node line]
  (when (node-contains-line? node line)
    (let [self (when (breakable-node? node) [node])
          children-results (mapcat #(find-breakable-forms-on-line % line)
                                   (node/named-children node))]
      (into (vec self) children-results))))

(defn find-breakable-form
  "Find the outermost breakable form containing the given line.

  Takes a parsed tree and a 1-indexed line number. Returns the outermost
  breakable node (list_lit, vec_lit, map_lit, set_lit) that spans that line,
  or nil if no breakable form is found."
  [tree line]
  (first (find-breakable-forms-on-line (node/root-node tree) line)))

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

  Applies the default breaking rule:
  - First element stays on the same line as opening delimiter
  - Remaining elements each get their own line with 2-space indent
  - Closing delimiter stays on the same line as last element

  Returns a vector of edits replacing whitespace between consecutive
  elements with newline+indent. Each edit is {:start n :end m :replacement s}."
  [node]
  (let [children (node/named-children node)
        indent-col (+ 2 (form-start-column node))
        indent-str (str "\n" (apply str (repeat indent-col \space)))]
    (when (> (count children) 1)
      (into []
            (map (fn [[prev-child next-child]]
                   {:start (element-end-offset prev-child)
                    :end (element-start-offset next-child)
                    :replacement indent-str}))
            (partition 2 1 children)))))
