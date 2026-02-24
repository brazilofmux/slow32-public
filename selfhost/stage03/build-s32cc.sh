#!/usr/bin/env bash
set -euo pipefail

# Build cc.s32x: the stage03 compiler (lex/parse split, evolved from cc-min).
# Uses: Stage 02 cc-min.s32x (compiler), Stage 02 s32-as.s32x (assembler),
#       Stage 02 s32-ld.s32x (linker).
# Deposits the artifact in the script's directory.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$SELFHOST_DIR/../.." && pwd 2>/dev/null || cd "$SELFHOST_DIR/.." && pwd)"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

EMU="${SELFHOST_EMU:-$SELFHOST_DIR/stage00/s32-emu}"
STAGE2_AS="$SELFHOST_DIR/stage02/s32-as.s32x"
STAGE2_LD="$SELFHOST_DIR/stage02/s32-ld.s32x"
GEN2_CC="$SELFHOST_DIR/stage02/cc-min.s32x"

LIBC_DIR="$SCRIPT_DIR/libc"
CRT0_SRC="$SCRIPT_DIR/crt0.s"
MMIO_NO_START_SRC="$SCRIPT_DIR/mmio_no_start.s"
OUT_EXE="$SCRIPT_DIR/cc.s32x"

for f in "$EMU" "$STAGE2_AS" "$STAGE2_LD" "$GEN2_CC" \
         "$CRT0_SRC" "$MMIO_NO_START_SRC" \
         "$SCRIPT_DIR/s32cc.c" "$SCRIPT_DIR/s32cc_lex.h" "$SCRIPT_DIR/s32cc_parse.h"; do
    [[ -f "$f" ]] || { echo "Missing: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/stage03-build.XXXXXX)"
trap 'rm -rf "$WORKDIR"' EXIT

cd "$ROOT_DIR"

compile() {
    local src="$1" asm="$2" log="$3"
    set +e
    timeout "${EXEC_TIMEOUT:-1200}" "$EMU" "$GEN2_CC" "$src" "$asm" >"$log" 2>&1
    local rc=$?
    set -e
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        echo "cc-min failed (rc=$rc): $src" >&2
        tail -n 40 "$log" >&2
        return 1
    fi
    [[ -s "$asm" ]] || { echo "cc-min produced no output: $src" >&2; return 1; }
}

assemble() {
    local src="$1" obj="$2" log="$3"
    set +e
    timeout "${EXEC_TIMEOUT:-1200}" "$EMU" "$STAGE2_AS" "$src" "$obj" >"$log" 2>&1
    local rc=$?
    set -e
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        echo "assemble failed (rc=$rc): $src" >&2
        tail -n 40 "$log" >&2
        return 1
    fi
    [[ -s "$obj" ]] || { echo "assemble produced no output: $src" >&2; return 1; }
}

link_exe() {
    local log="$1"
    shift
    set +e
    timeout "${EXEC_TIMEOUT:-1200}" "$EMU" "$STAGE2_LD" "$@" >"$log" 2>&1
    local rc=$?
    set -e
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        echo "link failed (rc=$rc)" >&2
        tail -n 40 "$log" >&2
        return 1
    fi
}

# --- Build runtime objects ---
echo "[1/4] Assemble runtime"
assemble "$CRT0_SRC" "$WORKDIR/crt0.s32o" "$WORKDIR/crt0.log"
assemble "$MMIO_NO_START_SRC" "$WORKDIR/mmio_no_start.s32o" "$WORKDIR/mmio_no_start.log"

# --- Build libc (compiled by cc-min) ---
echo "[2/4] Build libc"
LIBC_OBJS=""
for name in string_extra string_more ctype convert stdio malloc; do
    compile "$LIBC_DIR/${name}.c" "$WORKDIR/${name}.s" "$WORKDIR/${name}.cc.log"
    assemble "$WORKDIR/${name}.s" "$WORKDIR/${name}.s32o" "$WORKDIR/${name}.as.log"
    LIBC_OBJS="$LIBC_OBJS $WORKDIR/${name}.s32o"
done
compile "$LIBC_DIR/start.c" "$WORKDIR/start.s" "$WORKDIR/start.cc.log"
assemble "$WORKDIR/start.s" "$WORKDIR/start.s32o" "$WORKDIR/start.as.log"

# --- Compile s32-cc ---
echo "[3/4] Compile s32-cc"
cat "$SCRIPT_DIR/s32cc_lex.h" "$SCRIPT_DIR/s32cc_parse.h" > "$WORKDIR/s32cc_combined.h"
cp "$SCRIPT_DIR/s32cc.c" "$WORKDIR/s32cc.c"
compile "$WORKDIR/s32cc.c" "$WORKDIR/s32cc.s" "$WORKDIR/s32cc.cc.log"
assemble "$WORKDIR/s32cc.s" "$WORKDIR/s32cc.s32o" "$WORKDIR/s32cc.as.log"

# --- Link cc.s32x ---
echo "[4/4] Link cc.s32x"
link_exe "$WORKDIR/link.log" -o "$OUT_EXE" --mmio 64K \
    "$WORKDIR/crt0.s32o" "$WORKDIR/s32cc.s32o" "$WORKDIR/start.s32o" \
    "$WORKDIR/mmio_no_start.s32o" \
    $LIBC_OBJS
[[ -s "$OUT_EXE" ]] || { echo "link failed" >&2; exit 1; }

echo "OK: $OUT_EXE ($(wc -c < "$OUT_EXE") bytes)"
