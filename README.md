# line-breaker

A tool to reformat Clojure code to enforce a maximum line length.

Uses tree-sitter to understand the structure of the code, enabling
structure-aware line breaking rather than naive text wrapping.

## Installation

### Quick install

```bash
curl -sL https://raw.githubusercontent.com/hugoduncan/line-breaker/master/scripts/install.sh | bash
```

To install to a custom directory:

```bash
curl -sL https://raw.githubusercontent.com/hugoduncan/line-breaker/master/scripts/install.sh | bash -s -- --install-dir ~/.local/bin
```

For all available options, run `install.sh --help`.

### Manual download

Download the latest release for your platform from the
[releases page](https://github.com/hugoduncan/line-breaker/releases).

Available platforms:
- `darwin-x86_64` - macOS (also works on ARM via Rosetta)
- `linux-x86_64` - Linux x86_64

## Usage

Check files for line length violations (exits with code 1 if violations
found):

```bash
line-breaker --check src/myfile.clj
```

Automatically fix line length violations in-place:

```bash
line-breaker --fix src/myfile.clj
```

Output fixed content to stdout (leaves original file unchanged):

```bash
line-breaker --stdout src/myfile.clj
```

Paths can be files or directories. When given a directory, all matching
files are processed recursively.

## Behavior

When a line exceeds the limit, line-breaker reformats it according to
its rules.  line-breaker is opinionated: the way it breaks lines is not
configurable.

### Breaking Strategy

line-breaker breaks outer forms before inner ones, keeping code towards
the left margin rather than letting it pile up on the right.

Before:
```clojure
(defn foo [x] (let [y (+ x 1)] (when (pos? y) (println y))))
```

After (at 40 columns):
```clojure
(defn foo
  [x]
  (let [y (+ x 1)]
    (when (pos? y) (println y))))
```

### Indentation

Function calls use 1-space indentation; body forms like `defn`, `let`,
and `when` use 2-space indentation.

```clojure
;; Function call: 1-space indent
(some-fn
 arg1
 arg2
 arg3)

;; Body form: 2-space indent
(let [x 1]
  (println x))
```

### Selective Processing

Only lines exceeding the limit are reformatted. Short lines remain
untouched, preserving your formatting choices where possible.

### Ignoring Forms

Use `#_:line-breaker/ignore` before a form to prevent line-breaker from
reformatting it:

```clojure
#_:line-breaker/ignore
(this-form will-not-be-reformatted even-if-it-exceeds the-line-limit)
```

The marker also protects nested forms within the ignored form.

## Configuration

### CLI Options

- `--line-length N` — Maximum line length (default: 80)
- `-q, --quiet` — Suppress summary output

```bash
line-breaker --line-length 100 --check src/
```

### Config File

Create `.line-breaker.edn` in your project root:

```clojure
{:line-length 100
 :extensions [".clj" ".cljs"]
 :indents {my-defn :defn}}
```

- `:line-length` — Maximum line length (default: 80)
- `:extensions` — File extensions to process (default: `[".clj" ".cljs" ".cljc" ".edn"]`)
- `:indents` — Custom indent rules mapping symbols to rule types:
  - `:defn` — Keeps name on first line, 2-space indent (for definitions)
  - `:fn` — Keeps arg vector on first line (for anonymous functions)
  - `:binding` — Keeps binding vector on first line, 2-space indent (for `let`, `for`, etc.)
  - `:if` — 2-space indent (for conditionals)
  - `:cond` — Body on next line, pair grouping
  - `:condp` — Pair grouping
  - `:case` — Pair grouping
  - `:cond->` — Pair grouping
  - `:try` — Body on next line
  - `:do` — Body on next line

CLI options override config file values, which override defaults.
