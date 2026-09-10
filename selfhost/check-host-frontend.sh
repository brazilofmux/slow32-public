#!/usr/bin/env bash
set -euo pipefail

# Host-compile the two cross drivers that include the shared frontend
# (parser.h / hir_lower.h / ... via symlink from src/).
#
# stage07 and s12cc accept a static used before its definition; modern
# clang treats that as an error (C99).  None of the other gates build
# these TUs, which is how GitHub issue 71 sat unnoticed.
#
# Usage:  bash selfhost/check-host-frontend.sh

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

CC="${HOSTCC:-${CC:-cc}}"

check() {
    local dir="$1"
    local src="$2"
    local tag="$3"
    echo "  $tag"
    (cd "$dir" && "$CC" -std=c11 -O0 -fno-builtin -fsyntax-only \
        -Werror=implicit-function-declaration -Werror=implicit-int \
        -Wno-comment \
        "$src")
}

echo "=== Host frontend (GitHub issue 71) ==="
check "$ROOT_DIR/selfhost/stage08-cross-x64" cc-x64.c cc-x64
check "$ROOT_DIR/selfhost/stage08-cross-a64" cc-a64.c cc-a64
echo "OK: host frontend"
