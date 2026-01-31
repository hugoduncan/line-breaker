#!/usr/bin/env bash
# Install git pre-commit hook for code quality checks

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

# Handle both regular repos and worktrees
GIT_DIR="${REPO_ROOT}/.git"
if [ -f "${GIT_DIR}" ]; then
    # Worktree: .git is a file with gitdir pointer
    GIT_DIR="$(grep '^gitdir:' "${GIT_DIR}" | cut -d' ' -f2)"
fi
# Hooks are shared across worktrees in commondir
COMMON_DIR="$(git rev-parse --git-common-dir 2>/dev/null || echo "${GIT_DIR}")"
HOOKS_DIR="${COMMON_DIR}/hooks"

echo "Installing git hooks..."

# Create pre-commit hook
cat > "${HOOKS_DIR}/pre-commit" << 'EOF'
#!/usr/bin/env bash
# Pre-commit hook: formats code, restages, and runs lint/line-length checks
# Auto-fixes formatting and line length; blocks on lint errors or unfixable violations

set -euo pipefail

echo "Running pre-commit checks..."

# Get list of staged clj/cljs/cljc/edn files
STAGED_FILES=$(git diff --cached --name-only --diff-filter=ACM | grep -E '\.(clj|cljs|cljc|edn)$' || true)

if [ -n "${STAGED_FILES}" ]; then
    echo "Formatting staged files with cljfmt..."
    # shellcheck disable=SC2086
    cljfmt fix ${STAGED_FILES}

    echo "Re-staging formatted files..."
    # shellcheck disable=SC2086
    git add ${STAGED_FILES}

    echo "Fixing line length violations..."
    # shellcheck disable=SC2086
    bb line-length fix ${STAGED_FILES}

    echo "Re-staging line-length fixed files..."
    # shellcheck disable=SC2086
    git add ${STAGED_FILES}
fi

echo "Running linter..."
bb lint || {
    echo "Lint check failed. Fix the issues above."
    exit 1
}

if [ -n "${STAGED_FILES}" ]; then
    echo "Checking line length..."
    # shellcheck disable=SC2086
    bb line-length check ${STAGED_FILES} || {
        echo "Some lines exceed max length and cannot be auto-fixed"
        exit 1
    }
fi

echo "Pre-commit checks passed."
EOF

chmod +x "${HOOKS_DIR}/pre-commit"

echo "Git hooks installed successfully."
