#!/bin/bash
# EXEC: a dash-named guest must not be getopt, and an unqualified argv0
# must still find the emulator (GitHub issue 102). No clang required.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
EMU_DIR="$(dirname "$SCRIPT_DIR")"
ROOT="$(cd "$EMU_DIR/../.." && pwd)"
ASM="${ASM:-$ROOT/tools/assembler/slow32asm}"
LD="${LD:-$ROOT/tools/linker/s32-ld}"

echo "Building assembler, linker, emulators..."
make -C "$ROOT/tools/assembler" -s
make -C "$ROOT/tools/linker" -s
make -C "$ROOT/tools/emulator" -s
if [ -d "$ROOT/tools/dbt" ]; then
    make -C "$ROOT/tools/dbt" -s || true
fi

if [ ! -x "$ASM" ] || [ ! -x "$LD" ]; then
    echo "assembler/linker not found"
    exit 1
fi

work="$(mktemp -d "${TMPDIR:-/tmp}/s32-exec.XXXXXX")"
cleanup() { rm -rf "$work"; }
trap cleanup EXIT

echo "Assembling child and parent..."
"$ASM" "$SCRIPT_DIR/exec_child.s" "$work/child.s32o"
"$LD" -o "$work/child.s32x" "$work/child.s32o"
"$ASM" "$SCRIPT_DIR/exec_parent.s" "$work/parent.s32o"
"$LD" --mmio 64K --stack-size 64K --data-size 2M --heap-size 64K \
    -o "$work/parent.s32x" "$work/parent.s32o"
cp "$work/child.s32x" "$work/-t.s32x"

run_one() {
    local label="$1"
    local emu="$2"
    shift 2
    local out="$work/out.$label"
    local err="$work/err.$label"
    echo "=== $label ==="
    (cd "$work" && "$@" "$emu" -q --deny nosuch "$work/parent.s32x") \
        >"$out" 2>"$err" || true
    if grep -q 'x' "$out"; then
        echo "  OK  child printed x"
    else
        echo "  FAIL child did not print x"
        echo "=== stdout ---"
        cat "$out"
        echo "=== stderr ---"
        cat "$err"
        return 1
    fi
}

fail=0

if [ -x "$EMU_DIR/slow32" ]; then
    run_one slow32 "$EMU_DIR/slow32" env -u S32_EMU || fail=1
fi
if [ -x "$EMU_DIR/slow32-fast" ]; then
    run_one slow32-fast "$EMU_DIR/slow32-fast" env -u S32_EMU || fail=1
fi
if [ -x "$ROOT/tools/dbt/slow32-dbt" ]; then
    run_one slow32-dbt "$ROOT/tools/dbt/slow32-dbt" env -u S32_EMU || fail=1
fi

# Unqualified argv0: cwd has no binary, realpath(argv0) fails.
# /proc/self/exe or PATH should resolve it; S32_EMU is the last resort.
if [ -x "$EMU_DIR/slow32" ]; then
    echo "=== unqualified argv0 (slow32) ==="
    out="$work/out.unqual"
    err="$work/err.unqual"
    (cd "$work" && env -u S32_EMU PATH="$EMU_DIR:/usr/bin:/bin" \
        slow32 -q "$work/parent.s32x") >"$out" 2>"$err" || true
    if grep -q 'x' "$out"; then
        echo "  OK  nested EXEC with unqualified argv0"
    else
        echo "  FAIL nested EXEC with unqualified argv0"
        cat "$out" "$err"
        fail=1
    fi

    echo "=== S32_EMU fallback (slow32) ==="
    # Same layout, but also set S32_EMU to the absolute emulator. If
    # resolution failed, EXEC must still start the child.
    out="$work/out.s32emu"
    err="$work/err.s32emu"
    (cd "$work" && env S32_EMU="$EMU_DIR/slow32" PATH="$EMU_DIR:/usr/bin:/bin" \
        slow32 -q "$work/parent.s32x") >"$out" 2>"$err" || true
    if grep -q 'x' "$out"; then
        echo "  OK  nested EXEC with S32_EMU set"
    else
        echo "  FAIL nested EXEC with S32_EMU set"
        cat "$out" "$err"
        fail=1
    fi
fi

if [ "$fail" -ne 0 ]; then
    echo "=== TESTS FAILED ==="
    exit 1
fi
echo "=== All EXEC tests passed ==="
