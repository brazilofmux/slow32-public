#!/usr/bin/env bash
set -euo pipefail

# Build cc.s32x: the stage06 compiler.
# Uses: Stage 05 cc.s32x (compiler), Stage 05 s32-as.s32x (assembler),
#       Stage 05 s32-ld.s32x (linker).
#
# Two-phase build:
#   Phase 1: stage05 compiles s12cc.c + libc → gen1 (tree-walk ABI)
#   Phase 2: gen1 recompiles libc → stage06 libc (HIR/SSA ABI, r11-r28 callee-saved)
#
# gen1 (cc.s32x) uses tree-walk ABI internally but its codegen produces
# HIR/SSA ABI code.  Programs compiled by gen1 must link against gen1-compiled
# libc to avoid register clobbering (stage05 bootstrap libc may differ).
# Deposits cc.s32x and libc objects in the script's directory.

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

STAGE5_CC="$SELFHOST_DIR/stage05/cc.s32x"
STAGE5_AS="$SELFHOST_DIR/stage05/s32-as.s32x"
STAGE5_LD="$SELFHOST_DIR/stage05/s32-ld.s32x"

LIBC_DIR="$SCRIPT_DIR/libc"
CRT0_SRC="$SCRIPT_DIR/crt0.s"
MMIO_NO_START_SRC="$SCRIPT_DIR/mmio_no_start.s"
OUT_EXE="$SCRIPT_DIR/cc.s32x"

for f in "$EMU" "$STAGE5_CC" "$STAGE5_AS" "$STAGE5_LD" \
         "$CRT0_SRC" "$MMIO_NO_START_SRC" \
         "$SCRIPT_DIR/s12cc.c" "$SCRIPT_DIR/c_lexer_gen.c" \
         "$SCRIPT_DIR/ast.h" "$SCRIPT_DIR/parser.h" "$SCRIPT_DIR/sema.h" \
         "$SCRIPT_DIR/optimize.h" "$SCRIPT_DIR/codegen.h" "$SCRIPT_DIR/pp.h"; do
    [[ -f "$f" ]] || { echo "Missing: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/stage06-build.XXXXXX)"
trap 'rm -rf "$WORKDIR"' EXIT

cd "$ROOT_DIR"

compile() {
    local src="$1" asm="$2" log="$3"
    set +e
    timeout "${EXEC_TIMEOUT:-300}" "$EMU" "$STAGE5_CC" "$src" "$asm" >"$log" 2>&1
    local rc=$?
    set -e
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        echo "s12cc failed (rc=$rc): $src" >&2
        tail -n 40 "$log" >&2
        return 1
    fi
    [[ -s "$asm" ]] || { echo "s12cc produced no output: $src" >&2; return 1; }
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
    timeout 120 "$EMU" "$STAGE5_LD" "$@" >"$log" 2>&1
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

BUILTINS64_SRC="$SCRIPT_DIR/builtins64.s"
BUILTINS64_OBJ=""
if [[ -f "$BUILTINS64_SRC" ]]; then
    assemble "$BUILTINS64_SRC" "$WORKDIR/builtins64.s32o" "$WORKDIR/builtins64.log"
    BUILTINS64_OBJ="$WORKDIR/builtins64.s32o"
fi

# --- Build libc (compiled by stage05 s12cc) ---
echo "[2/4] Build libc"
LIBC_OBJS=""
for name in string_extra string_more ctype convert stdio malloc; do
    compile "$LIBC_DIR/${name}.c" "$WORKDIR/${name}.s" "$WORKDIR/${name}.cc.log"
    assemble "$WORKDIR/${name}.s" "$WORKDIR/${name}.s32o" "$WORKDIR/${name}.as.log"
    LIBC_OBJS="$LIBC_OBJS $WORKDIR/${name}.s32o"
done
compile "$LIBC_DIR/start.c" "$WORKDIR/start.s" "$WORKDIR/start.cc.log"
assemble "$WORKDIR/start.s" "$WORKDIR/start.s32o" "$WORKDIR/start.as.log"

# --- Compile compiler with stage05 compiler ---
echo "[3/4] Compile compiler"
compile "$SCRIPT_DIR/s12cc.c" "$WORKDIR/s12cc.s" "$WORKDIR/s12cc.cc.log"
assemble "$WORKDIR/s12cc.s" "$WORKDIR/s12cc.s32o" "$WORKDIR/s12cc.as.log"

# --- Link cc.s32x ---
echo "[4/6] Link cc.s32x (gen1, tree-walk ABI)"
link_exe "$WORKDIR/link.log" -o "$OUT_EXE" --mmio 64K \
    "$WORKDIR/crt0.s32o" "$WORKDIR/s12cc.s32o" "$WORKDIR/start.s32o" \
    "$WORKDIR/mmio_no_start.s32o" \
    $BUILTINS64_OBJ $LIBC_OBJS
[[ -s "$OUT_EXE" ]] || { echo "link failed" >&2; exit 1; }

echo "  gen1: $OUT_EXE ($(wc -c < "$OUT_EXE") bytes)"

# --- Phase 2: Recompile libc with gen1 (HIR/SSA ABI) ---
# gen1's codegen uses r11-r28 as callee-saved registers.  Programs compiled by gen1
# must link against gen1-compiled libc to get a consistent ABI.

compile_gen1() {
    local src="$1" asm="$2" log="$3"
    set +e
    timeout "${EXEC_TIMEOUT:-300}" "$EMU" "$OUT_EXE" "$src" "$asm" >"$log" 2>&1
    local rc=$?
    set -e
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        echo "gen1 compile failed (rc=$rc): $src" >&2
        tail -n 40 "$log" >&2
        return 1
    fi
    [[ -s "$asm" ]] || { echo "gen1 produced no output: $src" >&2; return 1; }
}

echo "[5/6] Recompile libc with gen1 (HIR/SSA ABI)"
LIBC_OUT_DIR="$SCRIPT_DIR/lib"
mkdir -p "$LIBC_OUT_DIR"

for name in string_extra string_more ctype convert stdio malloc; do
    compile_gen1 "$LIBC_DIR/${name}.c" "$WORKDIR/g1_${name}.s" "$WORKDIR/g1_${name}.cc.log"
    assemble "$WORKDIR/g1_${name}.s" "$LIBC_OUT_DIR/${name}.s32o" "$WORKDIR/g1_${name}.as.log"
done
compile_gen1 "$LIBC_DIR/start.c" "$WORKDIR/g1_start.s" "$WORKDIR/g1_start.cc.log"
assemble "$WORKDIR/g1_start.s" "$LIBC_OUT_DIR/start.s32o" "$WORKDIR/g1_start.as.log"
compile_gen1 "$LIBC_DIR/printf_varargs.c" "$WORKDIR/g1_printf_varargs.s" "$WORKDIR/g1_printf_varargs.cc.log"
assemble "$WORKDIR/g1_printf_varargs.s" "$LIBC_OUT_DIR/printf_varargs.s32o" "$WORKDIR/g1_printf_varargs.as.log"

# Runtime asm objects are ABI-neutral (hand-written assembly)
cp "$WORKDIR/crt0.s32o" "$LIBC_OUT_DIR/crt0.s32o"
cp "$WORKDIR/mmio_no_start.s32o" "$LIBC_OUT_DIR/mmio_no_start.s32o"
if [[ -n "$BUILTINS64_OBJ" ]]; then
    cp "$BUILTINS64_OBJ" "$LIBC_OUT_DIR/builtins64.s32o"
fi

echo "[6/6] Verify gen1 + gen1-libc"
# Quick smoke test: gen1 compiles a program that calls strcmp (libc function),
# verifying the ABI is consistent (r11-r28 preserved across calls).
cat > "$WORKDIR/smoke.c" <<'SMOKE_EOF'
int strcmp(char *a, char *b);
int main(void) {
    if (strcmp("hello", "hello") != 0) return 1;
    if (strcmp("abc", "abd") == 0) return 2;
    return 0;
}
SMOKE_EOF

compile_gen1 "$WORKDIR/smoke.c" "$WORKDIR/smoke.s" "$WORKDIR/smoke.cc.log"
assemble "$WORKDIR/smoke.s" "$WORKDIR/smoke.s32o" "$WORKDIR/smoke.as.log"
SMOKE_B64=""
if [[ -f "$LIBC_OUT_DIR/builtins64.s32o" ]]; then
    SMOKE_B64="$LIBC_OUT_DIR/builtins64.s32o"
fi
link_exe "$WORKDIR/smoke.link.log" -o "$WORKDIR/smoke.s32x" --mmio 64K \
    "$LIBC_OUT_DIR/crt0.s32o" "$WORKDIR/smoke.s32o" "$LIBC_OUT_DIR/start.s32o" \
    "$LIBC_OUT_DIR/mmio_no_start.s32o" \
    $SMOKE_B64 \
    "$LIBC_OUT_DIR/string_extra.s32o" "$LIBC_OUT_DIR/string_more.s32o" \
    "$LIBC_OUT_DIR/ctype.s32o" "$LIBC_OUT_DIR/convert.s32o" \
    "$LIBC_OUT_DIR/stdio.s32o" "$LIBC_OUT_DIR/malloc.s32o" \
    "$LIBC_OUT_DIR/printf_varargs.s32o"

set +e
timeout 30 "$EMU" "$WORKDIR/smoke.s32x" > "$WORKDIR/smoke.out" 2>&1
SMOKE_RC=$?
set -e

if [[ "$SMOKE_RC" -eq 0 || "$SMOKE_RC" -eq 96 ]]; then
    echo "  smoke test: PASS"
else
    echo "  smoke test: FAIL (rc=$SMOKE_RC)" >&2
    cat "$WORKDIR/smoke.out" >&2
    exit 1
fi

echo "OK: $OUT_EXE ($(wc -c < "$OUT_EXE") bytes)"
echo "    lib/ contains gen1-compiled libc objects (HIR/SSA ABI)"
