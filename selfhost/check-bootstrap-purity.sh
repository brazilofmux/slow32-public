#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

SELFHOST_DIR="$ROOT_DIR/selfhost"

if ! command -v rg >/dev/null 2>&1; then
    echo "ERROR: ripgrep (rg) is required for purity checks" >&2
    exit 1
fi

fail=0

scan_no_match() {
    local desc="$1"
    local pattern="$2"
    shift 2

    local out
    out="$(rg -n -P "$pattern" "$@" || true)"
    if [[ -n "$out" ]]; then
        echo "ERROR: bootstrap purity violation: $desc" >&2
        echo "$out" >&2
        fail=1
    fi
}

# Restrict checks to stage00..stage04 orchestration/build files.
SCAN_PATHS=(
    "$SELFHOST_DIR/run-stages.sh"
    "$SELFHOST_DIR/stage00"
    "$SELFHOST_DIR/stage01"
    "$SELFHOST_DIR/stage02"
    "$SELFHOST_DIR/stage03"
    "$SELFHOST_DIR/stage04"
)

BASE_GLOBS=(
    --glob '*.sh'
    --glob '*.fth'
    --glob 'Makefile'
)

# 1) No active references to prebuilt runtime blobs in stage00..04 flow.
# Ignore comment lines in shell (# ...) and Forth (\ ...).
scan_no_match \
    "active reference to runtime/*.s32o or runtime/*.s32a" \
    '^(?!\s*[#\\]).*runtime/[^[:space:]]*\.s32[oa]\b' \
    "${BASE_GLOBS[@]}" "${SCAN_PATHS[@]}"

# 2) No direct host compiler invocations in stage01..04 scripts.
# (stage00 Makefile is the accepted seed toolchain root)
scan_no_match \
    "direct host compiler command in stage01..04 scripts" \
    '^(?!\s*[#\\]).*(^|[;&|()[:space:]])(cc|gcc|clang)[[:space:]]' \
    --glob '*.sh' \
    "$SELFHOST_DIR/stage01" \
    "$SELFHOST_DIR/stage02" \
    "$SELFHOST_DIR/stage03" \
    "$SELFHOST_DIR/stage04"

if [[ "$fail" -ne 0 ]]; then
    echo "Bootstrap purity check FAILED." >&2
    echo "Allowed external seeds: stage00/s32-emu build + forth/kernel.s32x + forth/prelude.fth." >&2
    exit 1
fi

echo "Bootstrap purity check passed (stage00..04)."
