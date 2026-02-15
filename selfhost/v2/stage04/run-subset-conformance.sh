#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="${STAGE4_ROOT:-$(cd "$SCRIPT_DIR/../.." && pwd)}"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

EMU="${STAGE4_EMU:-$ROOT_DIR/tools/emulator/slow32}"
KERNEL="${STAGE4_KERNEL:-$ROOT_DIR/forth/kernel.s32x}"
PRELUDE="${STAGE4_PRELUDE:-$ROOT_DIR/forth/prelude.fth}"
CC_FTH="${STAGE4_CC:-$SCRIPT_DIR/cc.fth}"
ASM_FTH="${STAGE4_ASM:-$ROOT_DIR/selfhost/v2/stage01/asm.fth}"
LINK_FTH="${STAGE4_LINK:-$ROOT_DIR/selfhost/v2/stage03/link.fth}"
MANIFEST="${STAGE4_SUBSET_MANIFEST:-$SCRIPT_DIR/tests/manifests/subset.lst}"
RUNTIME_CRT0="${STAGE4_RUNTIME_CRT0:-$ROOT_DIR/runtime/crt0.s32o}"
RUNTIME_LIBC_MMIO="${STAGE4_RUNTIME_LIBC_MMIO:-$ROOT_DIR/runtime/libc_mmio.s32a}"
RUNTIME_LIBS32="${STAGE4_RUNTIME_LIBS32:-$ROOT_DIR/runtime/libs32.s32a}"

KEEP_ARTIFACTS=0

usage() {
    cat <<USAGE
Usage: $0 [--emu <path>] [--manifest <path>] [--keep-artifacts]

Runs Stage4 Subset-C conformance checks:
  - compile with stage4 cc.fth
  - assemble with stage01 asm.fth
  - link with stage03 link.fth
  - execute and require clean exit (0 or 96 HALT mapping)
USAGE
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --emu)
            shift
            [[ $# -gt 0 ]] || { echo "--emu requires a path" >&2; exit 2; }
            EMU="$1"
            ;;
        --manifest)
            shift
            [[ $# -gt 0 ]] || { echo "--manifest requires a path" >&2; exit 2; }
            MANIFEST="$1"
            ;;
        --keep-artifacts) KEEP_ARTIFACTS=1 ;;
        -h|--help) usage; exit 0 ;;
        *)
            echo "Unknown option: $1" >&2
            usage
            exit 2
            ;;
    esac
    shift
done

if [[ "$EMU" != /* ]]; then
    EMU="$ROOT_DIR/$EMU"
fi
if [[ "$MANIFEST" != /* ]]; then
    MANIFEST="$ROOT_DIR/$MANIFEST"
fi

for f in "$EMU" "$KERNEL" "$PRELUDE" "$CC_FTH" "$ASM_FTH" "$LINK_FTH" "$MANIFEST" \
         "$RUNTIME_CRT0" "$RUNTIME_LIBC_MMIO" "$RUNTIME_LIBS32"; do
    [[ -f "$f" ]] || { echo "Missing required file: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/stage4-subset.XXXXXX)"
if [[ "$KEEP_ARTIFACTS" -eq 0 ]]; then
    trap 'rm -rf "$WORKDIR"' EXIT
fi

run_forth() {
    local script_a="$1"
    local script_b="$2"
    local cmd_text="$3"
    local log_file="$4"

    set +e
    cat "$PRELUDE" "$script_a" "$script_b" - <<FTH | timeout 180 "$EMU" "$KERNEL" >"$log_file" 2>&1
$cmd_text
FTH
    local rc=$?
    set -e
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        echo "forth pipeline failed (rc=$rc)" >&2
        tail -n 50 "$log_file" >&2
        return 1
    fi
}

compile_c() {
    local src="$1"
    local asm="$2"
    local log="$3"

    run_forth "$CC_FTH" /dev/null "S\" $src\" S\" $asm\" COMPILE-FILE
BYE" "$log"
    [[ -s "$asm" ]] || { echo "compile produced no output: $src" >&2; return 1; }
    grep -q "Compilation successful" "$log" || {
        echo "compile failed: $src" >&2
        tail -n 50 "$log" >&2
        return 1
    }
}

assemble_s() {
    local asm="$1"
    local obj="$2"
    local log="$3"

    run_forth "$ASM_FTH" /dev/null "S\" $asm\" S\" $obj\" ASSEMBLE
BYE" "$log"
    [[ -s "$obj" ]] || { echo "assembler produced no output: $asm" >&2; return 1; }
    if grep -q "FAILED:" "$log"; then
        echo "assembler failed: $asm" >&2
        tail -n 50 "$log" >&2
        return 1
    fi
}

link_obj() {
    local obj="$1"
    local exe="$2"
    local log="$3"

    run_forth "$LINK_FTH" /dev/null "LINK-INIT
S\" $RUNTIME_CRT0\" LINK-OBJ
S\" $obj\" LINK-OBJ
65536 LINK-MMIO
S\" $RUNTIME_LIBC_MMIO\" LINK-ARCHIVE
S\" $RUNTIME_LIBS32\" LINK-ARCHIVE
S\" $exe\" LINK-EMIT
BYE" "$log"
    [[ -s "$exe" ]] || { echo "linker produced no output: $obj" >&2; return 1; }
}

run_exe() {
    local exe="$1"
    local log="$2"

    set +e
    timeout 30 "$EMU" "$exe" >"$log" 2>&1
    local rc=$?
    set -e
    if [[ "$rc" -eq 124 ]]; then
        echo "execution timed out: $exe" >&2
        tail -n 50 "$log" >&2
        return 1
    fi
    if grep -Eq "Execute fault|Memory fault|Write out of bounds or to protected memory|Unknown opcode|Unknown instruction|Load fault|Store fault" "$log"; then
        echo "execution faulted: $exe" >&2
        tail -n 50 "$log" >&2
        return 1
    fi
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        echo "execution did not halt cleanly: $exe (rc=$rc)" >&2
        tail -n 50 "$log" >&2
        return 1
    fi
}

echo "[subset] Stage4 conformance"
count=0
while IFS= read -r src || [[ -n "$src" ]]; do
    [[ -z "$src" ]] && continue
    [[ "$src" =~ ^# ]] && continue
    if [[ "$src" != /* ]]; then
        src="$ROOT_DIR/$src"
    fi
    [[ -f "$src" ]] || { echo "Missing test source: $src" >&2; exit 1; }
    base="$(basename "$src" .c)"
    asm="$WORKDIR/$base.s"
    obj="$WORKDIR/$base.s32o"
    exe="$WORKDIR/$base.s32x"

    echo "  - $base"
    compile_c "$src" "$asm" "$WORKDIR/$base.cc.log"
    assemble_s "$asm" "$obj" "$WORKDIR/$base.as.log"
    link_obj "$obj" "$exe" "$WORKDIR/$base.ld.log"
    run_exe "$exe" "$WORKDIR/$base.run.log"
    count=$((count + 1))
done < "$MANIFEST"

echo "OK: Stage4 subset conformance passed ($count tests)"
echo "Artifacts: $WORKDIR"
