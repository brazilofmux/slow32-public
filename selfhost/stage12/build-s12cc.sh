#!/usr/bin/env bash
set -euo pipefail

# Build s12cc.s32x: the stage12 AST-based C compiler.
# Uses: Stage 11 s32cc.s32x (compiler), Stage 05 s32-as.s32x (assembler),
#       Stage 07 s32-ld.s32x (linker).
# Deposits the artifact in the script's directory.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$SELFHOST_DIR/.." && pwd)"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

EMU="${SELFHOST_EMU:-}"
if [[ -z "$EMU" ]]; then
    dbt="$ROOT_DIR/tools/dbt/slow32-dbt"
    if [[ -x "$dbt" ]]; then
        EMU="$dbt"
    else
        EMU="$SELFHOST_DIR/stage00/s32-emu"
    fi
fi

STAGE11_CC="$SELFHOST_DIR/stage11/s32cc.s32x"
STAGE5_AS="$SELFHOST_DIR/stage05/s32-as.s32x"
STAGE7_LD="$SELFHOST_DIR/stage07/s32-ld.s32x"

LIBC_DIR="$SELFHOST_DIR/stage05/libc"
CRT0_SRC="$SELFHOST_DIR/stage05/crt0.s"
MMIO_NO_START_SRC="$SELFHOST_DIR/stage05/mmio_no_start.s"
OUT_EXE="$SCRIPT_DIR/s12cc.s32x"

for f in "$EMU" "$STAGE11_CC" "$STAGE5_AS" "$STAGE7_LD" \
         "$CRT0_SRC" "$MMIO_NO_START_SRC" \
         "$SCRIPT_DIR/s12cc.c" "$SCRIPT_DIR/c_lexer_gen.c" \
         "$SCRIPT_DIR/ast.h" "$SCRIPT_DIR/parser.h" "$SCRIPT_DIR/sema.h" \
         "$SCRIPT_DIR/optimize.h" "$SCRIPT_DIR/codegen.h" "$SCRIPT_DIR/pp.h"; do
    [[ -f "$f" ]] || { echo "Missing: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/stage12-build.XXXXXX)"
trap 'rm -rf "$WORKDIR"' EXIT

cd "$ROOT_DIR"

compile() {
    local src="$1" asm="$2" log="$3"
    set +e
    timeout "${EXEC_TIMEOUT:-300}" "$EMU" "$STAGE11_CC" "$src" "$asm" >"$log" 2>&1
    local rc=$?
    set -e
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        echo "s32cc failed (rc=$rc): $src" >&2
        tail -n 40 "$log" >&2
        return 1
    fi
    [[ -s "$asm" ]] || { echo "s32cc produced no output: $src" >&2; return 1; }
}

assemble() {
    local src="$1" obj="$2" log="$3"
    set +e
    timeout 120 "$EMU" "$STAGE5_AS" "$src" "$obj" >"$log" 2>&1
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
    timeout 120 "$EMU" "$STAGE7_LD" "$@" >"$log" 2>&1
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

# --- Build libc (compiled by stage11 s32cc) ---
echo "[2/4] Build libc"
LIBC_OBJS=""
for name in string_extra string_more ctype convert stdio malloc; do
    compile "$LIBC_DIR/${name}.c" "$WORKDIR/${name}.s" "$WORKDIR/${name}.cc.log"
    assemble "$WORKDIR/${name}.s" "$WORKDIR/${name}.s32o" "$WORKDIR/${name}.as.log"
    LIBC_OBJS="$LIBC_OBJS $WORKDIR/${name}.s32o"
done
compile "$LIBC_DIR/start.c" "$WORKDIR/start.s" "$WORKDIR/start.cc.log"
assemble "$WORKDIR/start.s" "$WORKDIR/start.s32o" "$WORKDIR/start.as.log"

# --- Compile s12cc ---
echo "[3/4] Compile s12cc"
compile "$SCRIPT_DIR/s12cc.c" "$WORKDIR/s12cc.s" "$WORKDIR/s12cc.cc.log"
assemble "$WORKDIR/s12cc.s" "$WORKDIR/s12cc.s32o" "$WORKDIR/s12cc.as.log"

# --- Link s12cc.s32x ---
echo "[4/4] Link s12cc.s32x"
link_exe "$WORKDIR/link.log" -o "$OUT_EXE" --mmio 64K \
    "$WORKDIR/crt0.s32o" "$WORKDIR/s12cc.s32o" "$WORKDIR/start.s32o" \
    "$WORKDIR/mmio_no_start.s32o" \
    $LIBC_OBJS
[[ -s "$OUT_EXE" ]] || { echo "link failed" >&2; exit 1; }

echo "OK: $OUT_EXE ($(wc -c < "$OUT_EXE") bytes)"
