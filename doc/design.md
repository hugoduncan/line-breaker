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

### Using Tree-sitter for Code Editing

Tree-sitter is a parsing library that produces read-only syntax trees.
It does not modify source code directly. For tools like line-sitter that
need to reformat code, the workflow is:

1. **Parse** the source to get node positions (byte offsets, row/column)
2. **Analyze** the tree to determine where edits are needed
3. **Edit** the source string using the position information
4. Optionally **re-parse** incrementally if making multiple passes

**Single-pass editing (recommended for line-sitter):**

For line-sitter, a single parse can identify all needed line breaks.
Collect edits as a list of insertions, then apply them to the source
string in reverse order (so earlier positions remain valid):

```clojure
(defn apply-edits
  "Apply edits to source. Each edit is {:offset n :insert s}.
  Edits must be sorted by offset descending."
  [source edits]
  (reduce
    (fn [s {:keys [offset insert]}]
      (str (subs s 0 offset) insert (subs s offset)))
    source
    edits))

;; Example: insert line breaks
(let [edits [{:offset 45 :insert "\n  "}
             {:offset 30 :insert "\n  "}
             {:offset 15 :insert "\n  "}]]
  (apply-edits source edits))
```

**Incremental re-parsing (for multi-pass editing):**

If edits require multiple analysis passes, use `InputEdit` to tell
tree-sitter what changed for efficient re-parsing:

```clojure
(import '[io.github.treesitter.jtreesitter InputEdit Point])

(defn make-input-edit
  "Create an InputEdit describing an insertion at offset."
  [source offset insert-text]
  (let [;; Calculate positions before edit
        before-text (subs source 0 offset)
        start-row   (count (filter #(= % \newline) before-text))
        start-col   (- offset (or (str/last-index-of before-text "\n") -1) 1)
        start-point (Point. start-row start-col)
        ;; Old end = start (insertion, no deletion)
        old-end-byte  offset
        old-end-point start-point
        ;; New end accounts for inserted text
        new-end-byte  (+ offset (count insert-text))
        insert-rows   (count (filter #(= % \newline) insert-text))
        new-end-row   (+ start-row insert-rows)
        new-end-col   (if (pos? insert-rows)
                        (- (count insert-text)
                           (inc (or (str/last-index-of insert-text "\n") -1)))
                        (+ start-col (count insert-text)))
        new-end-point (Point. new-end-row new-end-col)]
    (InputEdit. offset old-end-byte new-end-byte
                start-point old-end-point new-end-point)))

(defn edit-and-reparse
  "Apply an edit and incrementally re-parse."
  [parser tree source offset insert-text]
  (let [edit     (make-input-edit source offset insert-text)
        new-src  (str (subs source 0 offset) insert-text (subs source offset))
        _        (.edit tree edit)
        new-tree (.orElse (.parse parser new-src tree) nil)]
    {:source new-src :tree new-tree}))
```

**When to use each approach:**

| Approach | Use when |
|----------|----------|
| Single-pass | All edits can be determined from one parse (line-sitter) |
| Incremental | Edits depend on results of previous edits |

For line-sitter, single-pass editing is sufficient: parse once, collect
all line break insertions, apply in reverse offset order.

## Configuration

line-sitter reads configuration from a `.line-sitter.edn` file, searched
starting from the file being processed and walking up the directory
hierarchy.

### Configuration File Format

The configuration file is an EDN map with optional keys:

```clojure
{:line-length 80
 :extensions [".clj" ".cljs" ".cljc" ".edn"]
 :indents {my-macro :defn}}
```

**Keys:**

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `:line-length` | integer | `80` | Maximum allowed line length in characters |
| `:extensions` | vector of strings | `[".clj" ".cljs" ".cljc" ".edn"]` | File extensions to process |
| `:indents` | map | `{}` | Custom indent rules for project-specific macros (see Line Breaking Algorithm) |

### Configuration Search Algorithm

When processing a file, line-sitter searches for `.line-sitter.edn`:

1. Start from the directory containing the file being processed
2. Look for `.line-sitter.edn` in the current directory
3. If not found, move to the parent directory
4. Repeat until a config file is found or filesystem root is reached
5. If no config file is found, use default values

This behavior mirrors how tools like `.gitignore` work, allowing
project-level configuration that applies to all files within a
directory tree.

**Example directory structure:**

```
/home/user/projects/
├── .line-sitter.edn          # Project-wide config
└── myproject/
    ├── src/
    │   └── myproject/
    │       └── core.clj      # Uses /home/user/projects/.line-sitter.edn
    └── legacy/
        ├── .line-sitter.edn  # Override for legacy code
        └── old_code.clj      # Uses legacy/.line-sitter.edn
```

### Example Configurations

**Minimal config (uses all defaults):**
```clojure
{}
```

**Standard 80-column config:**
```clojure
{:line-length 80}
```

**Extended line length for modern displays:**
```clojure
{:line-length 120}
```

**Clojure files only:**
```clojure
{:line-length 80
 :extensions [".clj"]}
```

**Include EDN data files with longer lines:**
```clojure
{:line-length 100
 :extensions [".clj" ".cljs" ".cljc" ".edn"]}
```

### Loading Implementation

```clojure
(require '[clojure.java.io :as io]
         '[clojure.edn :as edn])

(def default-config
  {:line-length 80
   :extensions [".clj" ".cljs" ".cljc" ".edn"]})

(defn find-config-file
  "Search for .line-sitter.edn starting from dir, walking up to root.
  Returns the File if found, nil otherwise."
  [dir]
  (loop [current (io/file dir)]
    (when current
      (let [config-file (io/file current ".line-sitter.edn")]
        (if (.exists config-file)
          config-file
          (recur (.getParentFile current)))))))

(defn load-config
  "Load configuration for the given file path.
  Searches for .line-sitter.edn starting from the file's directory.
  Returns merged config with defaults for any missing keys."
  [file-path]
  (let [dir         (.getParent (io/file file-path))
        config-file (find-config-file dir)]
    (if config-file
      (merge default-config (edn/read-string (slurp config-file)))
      default-config)))

## CLI Interface

line-sitter provides a command-line interface for checking and fixing
line length violations in Clojure source files.

### Command Syntax

```
line-sitter [options] [paths...]
```

**Arguments:**
- `paths...` - Files or directories to process (default: current directory)

When a directory is given, line-sitter recursively finds files matching
the configured extensions.

### Options

| Option | Description |
|--------|-------------|
| `--check` | Report violations and exit with code 1 if any found (default mode) |
| `--fix` | Rewrite files in place to fix violations |
| `--stdout` | Output reformatted content to stdout instead of modifying files |
| `--line-length N` | Override configured line length (takes precedence over config file) |
| `--help` | Show usage information and exit |

**Mode precedence:** If multiple modes are specified, the last one wins.
However, `--help` always takes precedence and shows help regardless of
other options.

### Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success - no violations found (check mode) or files processed successfully (fix/stdout mode) |
| 1 | Violations found (check mode only) |
| 2 | Error - invalid arguments, file not found, parse error, or other failures |

### Usage Examples

**Check current directory for violations:**
```bash
line-sitter --check
```

**Check specific files:**
```bash
line-sitter --check src/myproject/core.clj src/myproject/util.clj
```

**Check a directory recursively:**
```bash
line-sitter --check src/
```

**Fix all files in place:**
```bash
line-sitter --fix src/
```

**Preview fixes without modifying files:**
```bash
line-sitter --stdout src/myproject/core.clj
```

**Use a different line length:**
```bash
line-sitter --check --line-length 120 src/
```

**CI integration (exits 1 if violations found):**
```bash
line-sitter --check src/ test/ || echo "Line length violations found"
```

### Output Format

**Check mode:** Reports violations to stderr in the format:
```
path/to/file.clj:42: line exceeds 80 characters (actual: 95)
```

**Fix mode:** Reports files modified to stdout:
```
Fixed: path/to/file.clj (3 lines)
```

**Stdout mode:** Outputs the reformatted file content to stdout. If
multiple files are specified, each file's content is preceded by a
comment header:
```clojure
;;; path/to/file.clj
(ns myproject.core)
...
```

### Default Behavior

When invoked without arguments:
```bash
line-sitter
```

This is equivalent to:
```bash
line-sitter --check .
```

The tool processes all files matching configured extensions in the
current directory and its subdirectories, reporting any line length
violations.

## Line Breaking Algorithm

This section describes the algorithm for reformatting Clojure code to
enforce maximum line length while preserving semantics.

### Overview

The algorithm uses tree-sitter to understand code structure, enabling
intelligent line breaking at form boundaries rather than arbitrary text
positions. When a line exceeds the configured limit, the algorithm
breaks forms from the outside in, placing each element on its own line
with consistent 2-space indentation.

### Algorithm Steps

1. **Parse** the source file with tree-sitter to obtain the syntax tree
2. **Scan** for lines exceeding the configured maximum length
3. **Identify** the outermost breakable form on each long line
4. **Break** that form by placing each child element on its own line
5. **Re-check** the resulting lines; if any still exceed the limit,
   break the next inner form
6. **Recurse** until all lines fit or only unbreakable atoms remain
7. **Leave** unbreakable content silently (no warning, no error)

### Form Breaking Rules

When breaking a form, apply these rules:

1. **First element** stays on the same line as the opening delimiter
2. **Special arguments** (based on indent rules) stay on the first line
3. **Remaining elements** each go on their own line with 2-space indent
   from the opening delimiter's column
4. **Closing delimiter** stays on the same line as the last element

#### Indent Rules (cljfmt-compatible)

Certain forms have "special" arguments that should stay on the first
line with the form name. line-sitter follows cljfmt's indent rules:

| Rule | Forms | First line keeps |
|------|-------|------------------|
| `:defn` | `defn`, `defn-`, `defmacro`, `defmethod`, `defprotocol`, `defrecord`, `deftype`, `reify`, `extend-type`, `extend-protocol` | name (and dispatch value for defmethod) |
| `:def` | `def`, `defonce`, `defmulti` | name |
| `:fn` | `fn`, `bound-fn` | argument vector |
| `:binding` | `let`, `when-let`, `if-let`, `binding`, `with-open`, `with-local-vars`, `with-redefs`, `doseq`, `for`, `loop` | bindings vector |
| `:cond` | `cond`, `condp`, `cond->`, `cond->>` | (none - each clause on own line) |
| `:case` | `case` | test expression |
| `:if` | `if`, `if-not`, `when`, `when-not` | test expression |
| `:try` | `try` | (none - body on next line) |
| `:do` | `do` | (none - body on next line) |
| default | everything else | (none - all args break) |

**Examples:**

```clojure
;; :defn rule - keep name on first line
(defn my-function
  [x y z]
  (+ x y z))

;; :def rule - keep name on first line
(def my-constant
  "A very long value that exceeds the line limit")

;; :binding rule - keep bindings on first line
(let [x 1 y 2 z 3]
  (+ x y z))

;; :if rule - keep test on first line
(if (some-condition? x)
  then-expression
  else-expression)

;; default rule - plain function call
(some-function
  arg1
  arg2
  arg3)
```

The 2-space indent is fixed and does not align with arguments. This
produces consistent, predictable output regardless of function name
length.

#### Configuration

Indent rules can be extended via configuration:

```clojure
;; .line-sitter.edn
{:line-length 80
 :indents {my-special-macro :defn
           with-my-resource :binding}}
```

This allows project-specific macros to use appropriate indent rules.

### Breakable vs Unbreakable Nodes

**Breakable nodes** (can have children placed on separate lines):
- `list_lit` - lists `(...)`
- `vec_lit` - vectors `[...]`
- `map_lit` - maps `{...}`
- `set_lit` - sets `#{...}`

**Unbreakable atoms** (cannot be split):
- `sym_lit` - symbols
- `kwd_lit` - keywords
- `str_lit` - strings (including multi-line)
- `num_lit` - numbers
- `char_lit` - characters
- `nil_lit` - nil
- `bool_lit` - booleans
- `regex_lit` - regular expressions

**Reader macros** preserve their prefix attached to the following form:
- `quoting_lit` (`'form`) - quote stays attached
- `syn_quoting_lit` (`` `form ``) - syntax-quote stays attached
- `unquoting_lit` (`~form`) - unquote stays attached
- `meta_lit` (`^meta form`) - metadata stays attached
- `derefing_lit` (`@form`) - deref stays attached
- `var_quoting_lit` (`#'var`) - var-quote stays attached
- `anon_fn_lit` (`#(...)`) - treated as breakable like list_lit
- `dis_expr` (`#_form`) - discard stays attached

### Identifying the Outermost Form

When a line exceeds the limit, the algorithm must find the correct form
to break. The **outermost form** is the largest breakable form whose
content contributes to the line length violation.

**Procedure:**
1. Find all nodes that span the long line
2. Filter to breakable nodes (list_lit, vec_lit, map_lit, set_lit)
3. Select the outermost (closest to root) among those whose breaking
   would address the violation
4. If the form starts on a previous line, consider only the portion
   on the current line

### Example 1: Simple Function Call

A function call with many arguments exceeding the 40-character limit.

**Before:**
```clojure
(println "Hello" "World" "from" "Clojure")
```

**Tree structure:**
```
list_lit
├── sym_lit "println"
├── str_lit "\"Hello\""
├── str_lit "\"World\""
├── str_lit "\"from\""
└── str_lit "\"Clojure\""
```

**After breaking (limit: 40):**
```clojure
(println
  "Hello"
  "World"
  "from"
  "Clojure")
```

The `list_lit` is identified as the outermost breakable form. Each child
(the symbol and strings) is placed on its own line with 2-space indent.

### Example 2: Nested Form Breaking with Indent Rules

When breaking the outer form isn't sufficient, break inner forms. Note
how `defn` keeps its name on the first line (`:defn` indent rule).

**Before (limit: 50):**
```clojure
(defn process [x] (-> x (transform-alpha) (transform-beta) (transform-gamma)))
```

**Tree structure:**
```
list_lit (defn)
├── sym_lit "defn"
├── sym_lit "process"
├── vec_lit
│   └── sym_lit "x"
└── list_lit (->)
    ├── sym_lit "->"
    ├── sym_lit "x"
    ├── list_lit
    │   └── sym_lit "transform-alpha"
    ├── list_lit
    │   └── sym_lit "transform-beta"
    └── list_lit
        └── sym_lit "transform-gamma"
```

**After first break (outer defn form with :defn indent rule):**
```clojure
(defn process
  [x]
  (-> x (transform-alpha) (transform-beta) (transform-gamma)))
```

The `:defn` rule keeps `process` on the first line. Line 3 still exceeds
50 characters. Break the inner `->` form:

**After second break:**
```clojure
(defn process
  [x]
  (->
    x
    (transform-alpha)
    (transform-beta)
    (transform-gamma)))
```

All lines now fit within the limit.

### Example 3: Unbreakable Content

Long strings and symbols cannot be broken and are left as-is.

**Before (limit: 40):**
```clojure
(def message "This is a very long string that cannot be broken")
```

**Tree structure:**
```
list_lit
├── sym_lit "def"
├── sym_lit "message"
└── str_lit "\"This is a very long string that cannot be broken\""
```

**After breaking (with :def indent rule):**
```clojure
(def message
  "This is a very long string that cannot be broken")
```

Line 2 still exceeds 40 characters, but `str_lit` is an unbreakable
atom. The algorithm leaves it silently—no warning or error is produced.
The user can apply the ignore mechanism if desired.

### Example 4: Map Literals

Maps break with each key-value pair, keeping pairs together when possible.

**Before (limit: 50):**
```clojure
{:name "Alice" :age 30 :occupation "Engineer" :city "Boston"}
```

**After breaking:**
```clojure
{:name "Alice"
  :age 30
  :occupation "Engineer"
  :city "Boston"}
```

Note: Key-value pairs are not kept on the same line by default. Each
element (whether key or value) gets its own line. Future enhancement
could keep pairs together.

### Example 5: Metadata Preservation

Metadata stays attached to its target form.

**Before (limit: 40):**
```clojure
(defn ^:private ^:deprecated helper-fn [x y z] (+ x y z))
```

**Tree structure:**
```
list_lit
├── sym_lit "defn"
├── meta_lit
│   ├── kwd_lit ":private"
│   └── meta_lit
│       ├── kwd_lit ":deprecated"
│       └── sym_lit "helper-fn"
├── vec_lit [x y z]
└── list_lit (+ x y z)
```

**After breaking (with :defn indent rule):**
```clojure
(defn ^:private ^:deprecated helper-fn
  [x y z]
  (+ x y z))
```

The `:defn` indent rule keeps the name on the first line. The metadata
chain `^:private ^:deprecated` stays attached to `helper-fn` as a single
unit, so the entire `meta_lit` node stays on line 1.

### Comments

Comments (`;`) within forms are treated as their own elements and
receive the same 2-space indentation when a form is broken.

**Before:**
```clojure
(process input ; transform the input
         intermediate ; apply rules
         output) ; return result
```

**After breaking:**
```clojure
(process
  input ; transform the input
  intermediate ; apply rules
  output) ; return result
```

Comments remain attached to the preceding element on the same line.

### Implementation Sketch

```clojure
(defn needs-breaking?
  "Check if a line exceeds the maximum length."
  [line max-length]
  (> (count line) max-length))

(defn find-outermost-breakable
  "Find the outermost breakable form spanning the given line."
  [tree line-number]
  ;; Walk tree, find breakable nodes (list_lit, vec_lit, map_lit, set_lit)
  ;; that span line-number, return the outermost one
  ...)

(defn break-form
  "Break a form by placing each child on its own line.
  Returns a sequence of edits {:offset n :insert s}."
  [node source]
  ;; For each child after the first, insert newline + indent before it
  ;; Indent = column of opening delimiter + 2
  ...)

(defn reformat
  "Reformat source to fit within max-length.
  Returns the reformatted source string."
  [source max-length]
  (let [tree (parse-source source)]
    (loop [src source
           tree tree]
      (let [lines         (str/split-lines src)
            long-line-idx (find-first-long-line lines max-length)]
        (if-not long-line-idx
          src ; all lines fit
          (if-let [form (find-outermost-breakable tree long-line-idx)]
            (let [edits   (break-form form src)
                  new-src (apply-edits src (sort-by :offset > edits))
                  new-tree (parse-source new-src)] ; or incremental reparse
              (recur new-src new-tree))
            src)))))) ; no breakable form found, return as-is
```

### Edge Cases

**Empty collections:** No children to break; left unchanged.
```clojure
[] {} () #{}
```

**Single-element collections:** Breaking doesn't help; left unchanged.
```clojure
(x) [x] {:a 1}
```

**Already-broken forms:** Lines already within limit are not modified.

**Mixed indentation in input:** The algorithm uses 2-space indent
regardless of existing indentation. It does not preserve alignment-based
formatting.

**Strings with newlines:** Multi-line strings are single `str_lit` nodes
and are unbreakable. They may cause lines to exceed limits.

## Ignore Mechanism

line-sitter provides an escape hatch for code that should not be
reformatted. Use the `#_:line-sitter/ignore` discard form immediately
before any form to prevent line-sitter from breaking it.

### Syntax

```clojure
#_:line-sitter/ignore
(form that should not be broken)
```

The ignore marker consists of:
1. `#_` - Clojure's discard reader macro
2. `:line-sitter/ignore` - a namespaced keyword

This is valid Clojure syntax that the reader discards, so it has no
runtime effect. The parser sees three nodes: `dis_expr`, `kwd_lit`, and
the following form.

### Detection

When line-sitter encounters a `dis_expr` node containing the keyword
`:line-sitter/ignore`, it marks the immediately following sibling form
as ignored. Ignored forms are:

- **Not broken** even if they exceed the line limit
- **Not recursively processed** (children are also left unchanged)
- **Left silently** without warning or error

### Examples

**Ignore a long definition:**
```clojure
#_:line-sitter/ignore
(def api-url "https://api.example.com/v2/very/long/path/to/resource/endpoint")
```

The long string would normally cause this line to exceed limits.
With the ignore marker, line-sitter leaves it unchanged.

**Ignore a formatted map:**
```clojure
#_:line-sitter/ignore
{:small 1  :medium 10  :large 100  :xlarge 1000}
```

If you've intentionally formatted a map with aligned spacing, use
ignore to preserve it.

**Ignore a complex threading macro:**
```clojure
#_:line-sitter/ignore
(-> data (transform {:opt-a 1 :opt-b 2}) (filter pred?) (map f) (into []))
```

Some developers prefer keeping threading macros on one line for
readability. The ignore marker preserves this choice.

**Ignore within a larger form:**
```clojure
(defn process-data
  [input]
  #_:line-sitter/ignore
  (let [config {:alpha 1 :beta 2 :gamma 3 :delta 4 :epsilon 5}]
    (transform input config)))
```

Only the `let` form is ignored; the outer `defn` can still be
reformatted if needed.

### When to Use

Use `#_:line-sitter/ignore` for:
- URL strings or paths that shouldn't be broken
- Intentionally formatted data (aligned maps, tables)
- One-liner threading macros kept for readability
- Generated or templated code

Avoid overusing ignore markers. If you find yourself adding many,
consider whether the line length limit is appropriate for your project.

## Example Transformations

This section shows complete before/after examples of how line-sitter
reformats code to fit within a maximum line length.

### Example 1: Long Function Call

A common case: a function call with many arguments.

**Configuration:** `{:line-length 50}`

**Before:**
```clojure
(send-notification user-id "Welcome!" {:urgent true :channel "email"})
```

**After:**
```clojure
(send-notification
  user-id
  "Welcome!"
  {:urgent true :channel "email"})
```

The outermost `list_lit` is broken. The map literal fits on one line
(under 50 characters) so it remains unbroken.

### Example 2: Nested Let Bindings

A `let` form with multiple bindings that exceed the limit.

**Configuration:** `{:line-length 60}`

**Before:**
```clojure
(let [user (fetch-user id) permissions (get-permissions user) role (:role user)]
  (authorize permissions role))
```

**After:**
```clojure
(let [user (fetch-user id)
      permissions (get-permissions user)
      role (:role user)]
  (authorize permissions role))
```

The `:binding` indent rule keeps the bindings vector on the first line
with `let`. The vector's children are broken with 6-space indent
(aligning with the first binding). The body fits on one line.

### Example 3: Map Literal

A map literal with many key-value pairs.

**Configuration:** `{:line-length 45}`

**Before:**
```clojure
{:id 1 :name "Alice" :email "alice@example.com" :active true}
```

**After:**
```clojure
{:id 1
  :name "Alice"
  :email "alice@example.com"
  :active true}
```

Each element (key or value) gets its own line with 2-space indent from
the opening brace.

### Example 4: Threading Macro

A threading macro with multiple transformation steps.

**Configuration:** `{:line-length 50}`

**Before:**
```clojure
(-> request (validate-input) (transform-payload) (add-metadata {:ts (now)}) (send-to-service))
```

**After (first pass):**
```clojure
(->
  request
  (validate-input)
  (transform-payload)
  (add-metadata {:ts (now)})
  (send-to-service))
```

The outer `->` form is broken. Each step goes on its own line. The
`(add-metadata {:ts (now)})` form fits under 50 characters, so it
remains unbroken.

### Example 5: Nested Form Requiring Multiple Breaks

Sometimes breaking the outer form isn't enough.

**Configuration:** `{:line-length 40}`

**Before:**
```clojure
(defn calc [x] (+ (* x x) (* 2 x) 1))
```

**After (first pass - break defn):**
```clojure
(defn calc
  [x]
  (+ (* x x) (* 2 x) 1))
```

Line 3 is 22 characters, which fits. But consider a longer variant:

**Before:**
```clojure
(defn calculate [x] (+ (multiply x x) (multiply 2 x) (constant 1)))
```

**After (first pass):**
```clojure
(defn calculate
  [x]
  (+ (multiply x x) (multiply 2 x) (constant 1)))
```

Line 3 exceeds 40 characters. Break the inner `+` form:

**After (second pass):**
```clojure
(defn calculate
  [x]
  (+
    (multiply x x)
    (multiply 2 x)
    (constant 1)))
```

All lines now fit within 40 characters.

### Example 6: Ignored Form Alongside Regular Formatting

Showing how ignore interacts with normal reformatting.

**Configuration:** `{:line-length 50}`

**Before:**
```clojure
(defn process [data] #_:line-sitter/ignore {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7} (transform data))
```

**After:**
```clojure
(defn process
  [data]
  #_:line-sitter/ignore
  {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7}
  (transform data))
```

The outer `defn` is broken normally. The map literal following
`#_:line-sitter/ignore` exceeds 50 characters but is left unchanged
because it's ignored. The final `(transform data)` call fits and
remains on one line.
