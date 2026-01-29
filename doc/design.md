# line-sitter Design

## Tree-sitter Integration

### Overview

line-sitter uses [tree-sitter](https://tree-sitter.github.io/) for
structure-aware parsing of Clojure source code. Tree-sitter provides
incremental parsing with concrete syntax trees, enabling the tool to
understand code structure rather than treating source as plain text.

### Java Bindings: jtreesitter

The official Java bindings are provided by
[java-tree-sitter](https://github.com/tree-sitter/java-tree-sitter)
(jtreesitter).

**Maven coordinates:**
```clojure
io.github.tree-sitter/jtreesitter {:mvn/version "0.26.0"}
```

**Requirements:** JRE 23+ (uses Java Foreign Function & Memory API)

**API documentation:**
https://tree-sitter.github.io/java-tree-sitter/

### Loading the Clojure Grammar

jtreesitter loads language grammars from native shared libraries at
runtime using `SymbolLookup`. The Clojure grammar must be built as a
native library from
[tree-sitter-clojure](https://github.com/sogaiu/tree-sitter-clojure).

```clojure
(import '[java.lang.foreign Arena SymbolLookup]
        '[io.github.treesitter.jtreesitter Language Parser])

(defn load-clojure-language
  "Load the Clojure language grammar from a native library.
  The library must be on java.library.path or specified with full path."
  [library-path]
  (let [arena   (Arena/global)
        symbols (SymbolLookup/libraryLookup library-path arena)]
    (Language/load symbols "tree_sitter_clojure")))
```

The grammar symbol name follows the convention `tree_sitter_<language>`.

### Building the Native Library

The tree-sitter-clojure grammar must be compiled to a platform-specific
shared library:

```bash
git clone https://github.com/sogaiu/tree-sitter-clojure
cd tree-sitter-clojure

# macOS
cc -shared -fPIC -I src src/parser.c -o libtree-sitter-clojure.dylib

# Linux
cc -shared -fPIC -I src src/parser.c -o libtree-sitter-clojure.so

# Windows (MSVC)
cl /LD /I src src\parser.c /Fe:tree-sitter-clojure.dll
```

For distribution, pre-built libraries should be bundled in the JAR under
`native/<os>-<arch>/` and extracted at runtime.

### Parsing Source Code

```clojure
(defn parse-source
  "Parse Clojure source code, returning a Tree or nil on failure."
  [^Parser parser ^String source]
  (.orElse (.parse parser source) nil))

;; Usage
(let [language (load-clojure-language "path/to/libtree-sitter-clojure.dylib")
      parser   (Parser. language)]
  (when-let [tree (parse-source parser "(defn foo [x] (+ x 1))")]
    ;; work with tree
    ))
```

The parser returns `Optional<Tree>`, empty if parsing was cancelled.

### Tree and Node Structure

A `Tree` contains the root `Node` of the syntax tree. Each node has:

- **Type:** The grammar rule name (e.g., `"list_lit"`, `"sym_lit"`)
- **Position:** Start/end byte offsets and row/column points
- **Children:** Ordered child nodes
- **Text:** The source text this node spans

```clojure
(defn root-node
  "Get the root node of a parsed tree."
  [^Tree tree]
  (.getRootNode tree))

(defn node-type
  "Get the type/kind of a node (e.g., 'list_lit', 'sym_lit')."
  [^Node node]
  (.getType node))

(defn node-text
  "Get the source text for a node, or nil if unavailable."
  [^Node node]
  (.getText node))

(defn node-range
  "Get the byte range [start end] of a node."
  [^Node node]
  [(.getStartByte node) (.getEndByte node)])

(defn node-position
  "Get the start position as [row column] (0-indexed)."
  [^Node node]
  (let [point (.getStartPoint node)]
    [(.row point) (.column point)]))
```

### Clojure Grammar Node Types

The [sogaiu/tree-sitter-clojure](https://github.com/sogaiu/tree-sitter-clojure)
grammar defines nodes for Clojure primitives only (not higher-level
constructs like `defn`):

**Literals:**
- `num_lit` - numbers
- `str_lit` - strings
- `kwd_lit` - keywords
- `sym_lit` - symbols
- `char_lit` - characters
- `nil_lit` - nil
- `bool_lit` - booleans

**Collections:**
- `list_lit` - lists `(...)`
- `vec_lit` - vectors `[...]`
- `map_lit` - maps `{...}`
- `set_lit` - sets `#{...}`

**Reader Macros:**
- `quoting_lit` - quote `'`
- `syn_quoting_lit` - syntax-quote `` ` ``
- `unquoting_lit` - unquote `~`
- `unquote_splicing_lit` - unquote-splice `~@`
- `derefing_lit` - deref `@`
- `meta_lit` - metadata `^`
- `regex_lit` - regex `#"..."`
- `anon_fn_lit` - anonymous function `#(...)`
- `var_quoting_lit` - var quote `#'`
- `tagged_or_ctor_lit` - tagged literals `#inst`, `#uuid`
- `read_cond_lit` - reader conditionals `#?(...)`
- `splicing_read_cond_lit` - splicing reader conditionals `#?@(...)`
- `ns_map_lit` - namespaced maps `#:ns{...}`
- `dis_expr` - discard `#_`

**Other:**
- `comment` - comments `;`

### Traversing the Tree

**Direct child access:**
```clojure
(defn children
  "Get all child nodes."
  [^Node node]
  (.getChildren node))

(defn named-children
  "Get named (non-anonymous) child nodes."
  [^Node node]
  (.getNamedChildren node))

(defn child-at
  "Get child at index, or nil if out of bounds."
  [^Node node index]
  (.orElse (.getChild node index) nil))
```

**TreeCursor for efficient traversal:**
```clojure
(import '[io.github.treesitter.jtreesitter TreeCursor])

(defn walk-tree
  "Walk all nodes depth-first, calling f on each."
  [^Node root f]
  (let [cursor (.walk root)]
    (try
      (loop []
        (f (.getCurrentNode cursor))
        (cond
          ;; Try to go to first child
          (.gotoFirstChild cursor) (recur)
          ;; Try next sibling
          (.gotoNextSibling cursor) (recur)
          ;; Go up and try sibling
          :else
          (loop []
            (when (.gotoParent cursor)
              (if (.gotoNextSibling cursor)
                (recur)
                (recur))))))
      (finally
        (.close cursor)))))
```

**Simpler recursive traversal:**
```clojure
(defn visit-nodes
  "Visit all nodes depth-first, calling f on each."
  [^Node node f]
  (f node)
  (doseq [child (.getChildren node)]
    (visit-nodes child f)))
```

### Example: Finding Long Lines

```clojure
(defn find-long-lines
  "Find nodes that span lines exceeding max-length."
  [^Tree tree max-length source-lines]
  (let [root   (.getRootNode tree)
        result (atom [])]
    (visit-nodes root
      (fn [^Node node]
        (let [start-row (.row (.getStartPoint node))
              end-row   (.row (.getEndPoint node))]
          (doseq [row (range start-row (inc end-row))]
            (when (> (count (nth source-lines row)) max-length)
              (swap! result conj {:node node :row row}))))))
    @result))
```

### Resource Management

jtreesitter uses Java's Foreign Function & Memory API. Key
considerations:

1. **Arena lifecycle:** The `Arena` used to load languages must remain
   open while the language is in use
2. **Parser/TreeCursor:** Implement `AutoCloseable`; use `with-open` or
   call `.close()` explicitly
3. **Trees and Nodes:** Lightweight references; no explicit cleanup
   needed

```clojure
(defn with-parser
  "Execute f with a parser, ensuring cleanup."
  [language f]
  (with-open [parser (Parser. language)]
    (f parser)))
```
