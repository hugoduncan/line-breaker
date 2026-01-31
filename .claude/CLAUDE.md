# line-breaker

A tool to reformat Clojure code to enforce a maximum line length.

Uses tree-sitter (via jtreesitter Java bindings) to understand the
structure of the code, enabling structure-aware line breaking rather
than naive text wrapping.

## Development

### Setup

After cloning, compile the Java sources (required for tree-sitter native library loading):
```bash
clojure -T:build javac
```

Or via babashka:
```bash
bb javac
```

### REPL

Start an nREPL server:
```bash
clojure -M:nrepl
```

### Running

```bash
clojure -X:run
```

### Testing

Run tests with kaocha:
```bash
bb test
```

### Code Quality

Format code:
```bash
bb format
```

Check formatting (CI):
```bash
bb format-check
```

Lint with clj-kondo:
```bash
bb lint
```

Check line length (max 80 characters):
```bash
bb line-length check
```

Fix line length violations:
```bash
bb line-length fix
```

Import clj-kondo configs from dependencies:
```bash
bb import-kondo-config
```

### Git Hooks

Install pre-commit hooks (auto-formats and lints):
```bash
./scripts/install-hooks.sh
```

## Conventions

- Use semantic commit messages
- Tests use kaocha; place tests in `test/` directory
- Pre-commit hook auto-formats staged files, enforces line length, and blocks on lint errors
