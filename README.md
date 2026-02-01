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
