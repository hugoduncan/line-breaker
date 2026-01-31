(ns line-breaker.treesitter.node
  "Clojure interface for tree-sitter Node access.

  Provides convenient accessors that return Clojure-native data structures,
  hiding Java interop details."
  (:import [io.github.treesitter.jtreesitter Tree Node]))

(defn root-node
  "Get the root node from a Tree.

  Returns the Node at the root of the syntax tree, or nil if tree is nil."
  ^Node [^Tree tree]
  (when tree
    (.getRootNode tree)))

(defn node-type
  "Get the grammar rule name for a node as a keyword.

  Returns a keyword like :list_lit, :sym_lit, :source.
  Returns nil if node is nil."
  [^Node node]
  (when node
    (keyword (.getType node))))

(defn node-text
  "Get the source text for a node.

  Returns the string of source code that the node spans.
  Returns nil if node is nil."
  [^Node node]
  (when node
    (.getText node)))

(defn node-range
  "Get the byte range for a node.

  Returns a vector [start-byte end-byte] representing the byte offsets.
  Returns nil if node is nil."
  [^Node node]
  (when node
    [(.getStartByte node) (.getEndByte node)]))

(defn node-position
  "Get the start position for a node.

  Returns a map {:row :column} with 0-based indices.
  Returns nil if node is nil."
  [^Node node]
  (when node
    (let [point (.getStartPoint node)]
      {:row (.row point)
       :column (.column point)})))

(defn- optional->node
  "Unwrap an Optional<Node> to Node or nil."
  [^java.util.Optional opt]
  (when (.isPresent opt)
    (.get opt)))

(defn children
  "Get all child nodes.

  Returns a vector of all child nodes (both named and anonymous).
  Returns nil if node is nil."
  [^Node node]
  (when node
    (let [cnt (.getChildCount node)]
      (into [] (keep #(optional->node (.getChild node %))) (range cnt)))))

(defn named-children
  "Get named child nodes only.

  Returns a vector of named child nodes (excludes punctuation like parens).
  Returns nil if node is nil."
  [^Node node]
  (when node
    (let [cnt (.getNamedChildCount node)]
      (into [] (keep #(optional->node (.getNamedChild node %))) (range cnt)))))

(defn next-named-sibling
  "Get the next named sibling node.

  Returns the next sibling that is a named node (not punctuation).
  Returns nil if node is nil or has no next named sibling."
  ^Node [^Node node]
  (when node
    (optional->node (.getNextNamedSibling node))))

(defn node-line-range
  "Get the line range for a node.

  Returns a vector [start-line end-line] with 1-indexed line numbers.
  Both endpoints are inclusive.
  Returns nil if node is nil."
  [^Node node]
  (when node
    [(inc (.row (.getStartPoint node)))
     (inc (.row (.getEndPoint node)))]))

(defn node-parent
  "Get the parent node.

  Returns the parent Node, or nil if node is nil or has no parent."
  ^Node [^Node node]
  (when node
    (optional->node (.getParent node))))
