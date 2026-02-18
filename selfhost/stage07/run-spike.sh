#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$SELFHOST_DIR/.." && pwd)"

EMU="${STAGE7_EMU:-$SELFHOST_DIR/stage00/s32-emu}"
KERNEL="${STAGE7_KERNEL:-$ROOT_DIR/forth/kernel.s32x}"
PRELUDE="${STAGE7_PRELUDE:-$ROOT_DIR/forth/prelude.fth}"
CC_FTH="${STAGE7_CC:-$SELFHOST_DIR/stage04/cc.fth}"
ASM_FTH="${STAGE7_ASM:-$SELFHOST_DIR/stage01/asm.fth}"
LINK_FTH="${STAGE7_LINK:-$SELFHOST_DIR/stage03/link.fth}"
SRC="${STAGE7_SRC:-$SCRIPT_DIR/s32-ld.c}"
KEEP_ARTIFACTS=0
WITH_RELOC_SPIKE=0

usage() {
    cat <<USAGE
Usage: $0 [--emu <path>] [--keep-artifacts] [--with-reloc-spike]

Builds and spikes Stage07 linker candidate with current selfhost pipeline:
  1) Bootstrap stage05 assembler (and stage06 archiver)
  2) stage04 cc.fth -> s32-ld.s
  3) stage05 s32-as -> s32-ld.s32o
  4) stage03 link.fth -> s32-ld.s32x
Then validates:
  - Old-style CLI: input.s32o output.s32x (backward compat)
  - New-style CLI: -o output input.s32o (single object)
  - Multi-object:  -o output --mmio 64K crt0 prog start mmio archive
USAGE
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --emu)
            shift
            [[ $# -gt 0 ]] || { echo "--emu requires a path" >&2; exit 2; }
            EMU="$1"
            ;;
        --keep-artifacts)
            KEEP_ARTIFACTS=1
            ;;
        --with-reloc-spike)
            WITH_RELOC_SPIKE=1
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            usage
            exit 2
            ;;
    esac
    shift
done

for f in "$EMU" "$KERNEL" "$PRELUDE" "$CC_FTH" "$ASM_FTH" "$LINK_FTH"; do
    [[ -f "$f" ]] || { echo "Missing required file: $f" >&2; exit 1; }
done

if [[ ! -f "$SRC" ]]; then
    # Backward-compatible fallback during path transition.
    SRC="$SCRIPT_DIR/validation/s32-ld.c"
fi
[[ -f "$SRC" ]] || { echo "Missing required file: $SRC" >&2; exit 1; }

WORKDIR="$(mktemp -d /tmp/selfhost-v2-stage07.XXXXXX)"
if [[ "$KEEP_ARTIFACTS" -eq 0 ]]; then
    trap 'rm -rf "$WORKDIR"' EXIT
fi

# cc.fth uses relative include path "selfhost/stage04/include/" — run from repo root
cd "$ROOT_DIR"

# Bootstrap stage05/06 tools first
PIPE_LOG="$WORKDIR/stage5-build.log"
"$SELFHOST_DIR/stage05/run-pipeline.sh" --mode stage6-ar-smoke --emu "$EMU" --keep-artifacts >"$PIPE_LOG"
PIPE_ART="$(awk -F': ' '/^Artifacts:/{print $2}' "$PIPE_LOG" | tail -n 1)"
[[ -n "$PIPE_ART" && -d "$PIPE_ART" ]] || { echo "failed to locate stage05 artifacts dir" >&2; exit 1; }
AS_EXE="$PIPE_ART/s32-as.s32x"
[[ -f "$AS_EXE" ]] || { echo "missing stage05 assembler exe: $AS_EXE" >&2; exit 1; }

# Runtime objects from stage05 bootstrap
RUNTIME_CRT0="$PIPE_ART/crt0_minimal.s32o"
RUNTIME_MMIO_OBJ="$PIPE_ART/mmio_minimal.s32o"
RUNTIME_MMIO_NO_START_OBJ="$PIPE_ART/mmio_no_start.s32o"
LIBC_ARCHIVE="$PIPE_ART/libc_selfhost.s32a"
LIBC_START_OBJ="$PIPE_ART/libc_start.s32o"

[[ -s "$RUNTIME_CRT0" ]] || { echo "missing runtime crt0: $RUNTIME_CRT0" >&2; exit 1; }
[[ -s "$RUNTIME_MMIO_OBJ" ]] || { echo "missing runtime mmio: $RUNTIME_MMIO_OBJ" >&2; exit 1; }
[[ -s "$RUNTIME_MMIO_NO_START_OBJ" ]] || { echo "missing runtime mmio (no start): $RUNTIME_MMIO_NO_START_OBJ" >&2; exit 1; }
[[ -s "$LIBC_ARCHIVE" ]] || { echo "missing libc archive: $LIBC_ARCHIVE" >&2; exit 1; }
[[ -s "$LIBC_START_OBJ" ]] || { echo "missing libc start: $LIBC_START_OBJ" >&2; exit 1; }

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
        tail -n 60 "$log_file" >&2
        return 1
    fi
}

run_exe() {
    local exe="$1"
    local log="$2"
    shift 2

    set +e
    timeout "${EXEC_TIMEOUT:-60}" "$EMU" "$exe" "$@" >"$log" 2>&1
    local rc=$?
    set -e
    if [[ "$rc" -eq 124 ]]; then
        echo "execution timed out: $exe" >&2
        tail -n 60 "$log" >&2
        return 1
    fi
    if grep -Eq "Execute fault|Memory fault|Write out of bounds or to protected memory|Unknown opcode|Unknown instruction|Load fault|Store fault" "$log"; then
        echo "execution faulted: $exe" >&2
        tail -n 60 "$log" >&2
        return 1
    fi
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        echo "execution did not halt cleanly: $exe" >&2
        tail -n 60 "$log" >&2
        return 1
    fi
}

run_exe_any_rc() {
    local exe="$1"
    local log="$2"
    shift 2

    set +e
    timeout "${EXEC_TIMEOUT:-180}" "$EMU" "$exe" "$@" >"$log" 2>&1
    local rc=$?
    set -e
    if [[ "$rc" -eq 124 ]]; then
        echo "execution timed out: $exe" >&2
        tail -n 60 "$log" >&2
        return 124
    fi
    if grep -Eq "Execute fault|Memory fault|Write out of bounds or to protected memory|Unknown opcode|Unknown instruction|Load fault|Store fault" "$log"; then
        echo "execution faulted: $exe" >&2
        tail -n 60 "$log" >&2
        return 125
    fi
    return "$rc"
}

run_exe_nofault() {
    local exe="$1"
    local log="$2"
    shift 2

    set +e
    timeout "${EXEC_TIMEOUT:-60}" "$EMU" "$exe" "$@" >"$log" 2>&1
    local rc=$?
    set -e
    if [[ "$rc" -eq 124 ]]; then
        echo "execution timed out: $exe" >&2
        tail -n 60 "$log" >&2
        return 1
    fi
    if grep -Eq "Execute fault|Memory fault|Write out of bounds or to protected memory|Unknown opcode|Unknown instruction|Load fault" "$log"; then
        echo "execution faulted: $exe" >&2
        tail -n 60 "$log" >&2
        return 1
    fi
}

compile_c_stage4() {
    local src="$1"
    local asm="$2"
    local log="$3"

    run_forth "$CC_FTH" /dev/null "S\" $src\" S\" $asm\" COMPILE-FILE
BYE" "$log"
    [[ -s "$asm" ]] || { echo "compile produced no output: $src" >&2; return 1; }
    grep -q "Compilation successful" "$log" || {
        echo "compile failed: $src" >&2
        tail -n 60 "$log" >&2
        return 1
    }
}

assemble_with_stage5() {
    local asm="$1"
    local obj="$2"
    local log="$3"

    run_exe "$AS_EXE" "$log" "$asm" "$obj"
    [[ -s "$obj" ]] || { echo "stage05 assembler produced no output: $asm" >&2; return 1; }
}

link_forth_with_libc() {
    local obj="$1"
    local exe="$2"
    local log="$3"

    run_forth "$LINK_FTH" /dev/null "LINK-INIT
S\" $RUNTIME_CRT0\" LINK-OBJ
S\" $obj\" LINK-OBJ
S\" $LIBC_START_OBJ\" LINK-OBJ
S\" $RUNTIME_MMIO_NO_START_OBJ\" LINK-OBJ
65536 LINK-MMIO
S\" $LIBC_ARCHIVE\" LINK-ARCHIVE
S\" $exe\" LINK-EMIT
BYE" "$log"
    [[ -s "$exe" ]] || { echo "linker produced no output: $obj" >&2; return 1; }
}

# ============================================================
# Build linker
# ============================================================
echo "=== Building stage07 linker ==="

STAGE7_ASM="$WORKDIR/s32-ld.s"
STAGE7_OBJ="$WORKDIR/s32-ld.s32o"
STAGE7_EXE="$WORKDIR/s32-ld.s32x"

compile_c_stage4 "$SRC" "$STAGE7_ASM" "$WORKDIR/s32-ld.cc.log"
assemble_with_stage5 "$STAGE7_ASM" "$STAGE7_OBJ" "$WORKDIR/s32-ld.as.log"
link_forth_with_libc "$STAGE7_OBJ" "$STAGE7_EXE" "$WORKDIR/s32-ld.ld.log"
echo "Linker built: $STAGE7_EXE"

# ============================================================
# Test 1: Old-style CLI (backward compat for stage08)
# ============================================================
echo ""
echo "=== Test 1: Old-style CLI ==="

cat > "$WORKDIR/main_halt.s" <<'ASM'
.text
.global main
main:
    halt
ASM

MINI_OBJ="$WORKDIR/main_halt.s32o"
MINI_EXE="$WORKDIR/main_halt.s32x"

assemble_with_stage5 "$WORKDIR/main_halt.s" "$MINI_OBJ" "$WORKDIR/main_halt.as.log"
run_exe "$STAGE7_EXE" "$WORKDIR/s32-ld.run.log" "$MINI_OBJ" "$MINI_EXE"
[[ -s "$MINI_EXE" ]] || { echo "FAIL: old-style linker produced no output" >&2; exit 1; }
run_exe "$MINI_EXE" "$WORKDIR/main_halt.run.log"
echo "  Old-style single object: PASS"

# ============================================================
# Test 2: New-style CLI (single object, -o flag)
# ============================================================
echo ""
echo "=== Test 2: New-style CLI ==="

NEW_EXE="$WORKDIR/main_halt_new.s32x"
run_exe "$STAGE7_EXE" "$WORKDIR/s32-ld.new.run.log" -o "$NEW_EXE" "$MINI_OBJ"
[[ -s "$NEW_EXE" ]] || { echo "FAIL: new-style linker produced no output" >&2; exit 1; }
run_exe "$NEW_EXE" "$WORKDIR/main_halt_new.run.log"
echo "  New-style single object: PASS"

# ============================================================
# Test 3: Multi-object with libc + MMIO (new-style CLI)
# ============================================================
echo ""
echo "=== Test 3: Multi-object with libc + MMIO ==="

# Compile test_smoke.c (returns 42) if available, else create a simple test
SMOKE_SRC="$SELFHOST_DIR/stage09/test_smoke.c"
if [[ -f "$SMOKE_SRC" ]]; then
    compile_c_stage4 "$SMOKE_SRC" "$WORKDIR/smoke.s" "$WORKDIR/smoke.cc.log"
    assemble_with_stage5 "$WORKDIR/smoke.s" "$WORKDIR/smoke.s32o" "$WORKDIR/smoke.as.log"
    SMOKE_OBJ="$WORKDIR/smoke.s32o"
else
    # Simple smoke test: main returns 42
    cat > "$WORKDIR/smoke_main.s" <<'ASM'
.text
.global main
main:
    addi r1, r0, 42
    jalr r0, r31, 0
ASM
    assemble_with_stage5 "$WORKDIR/smoke_main.s" "$WORKDIR/smoke_main.s32o" "$WORKDIR/smoke_main.as.log"
    SMOKE_OBJ="$WORKDIR/smoke_main.s32o"
fi

MULTI_EXE="$WORKDIR/multi_test.s32x"
run_exe "$STAGE7_EXE" "$WORKDIR/s32-ld.multi.run.log" \
    -o "$MULTI_EXE" --mmio 64K \
    "$RUNTIME_CRT0" "$SMOKE_OBJ" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
    "$LIBC_ARCHIVE"
[[ -s "$MULTI_EXE" ]] || { echo "FAIL: multi-object linker produced no output" >&2; exit 1; }

set +e
timeout "${EXEC_TIMEOUT:-60}" "$EMU" "$MULTI_EXE" >"$WORKDIR/multi_test.run.log" 2>&1
MULTI_RC=$?
set -e
if [[ "$MULTI_RC" -eq 42 ]]; then
    echo "  Multi-object with libc: PASS (rc=42)"
else
    echo "  Multi-object with libc: FAIL (expected rc=42, got rc=$MULTI_RC)" >&2
    tail -n 20 "$WORKDIR/multi_test.run.log" >&2
    exit 1
fi

# ============================================================
# Optional: Relocation spike
# ============================================================
if [[ "$WITH_RELOC_SPIKE" -eq 1 ]]; then
    echo ""
    echo "=== Relocation spike ==="
    cat > "$WORKDIR/reloc_spike.s" <<'ASM'
.data
.global value
value:
    .word 7

.text
.global main
main:
    jal r1, helper
    halt

helper:
    la r2, value
    ldw r3, r2, 0
    beq r3, r3, done
    halt
done:
    halt
ASM

    RELOC_OBJ="$WORKDIR/reloc_spike.s32o"
    RELOC_EXE="$WORKDIR/reloc_spike.s32x"

    assemble_with_stage5 "$WORKDIR/reloc_spike.s" "$RELOC_OBJ" "$WORKDIR/reloc_spike.as.log"
    run_exe "$STAGE7_EXE" "$WORKDIR/s32-ld.reloc.run.log" "$RELOC_OBJ" "$RELOC_EXE"
    [[ -s "$RELOC_EXE" ]] || { echo "FAIL: relocation-spike produced no output" >&2; exit 1; }
    run_exe_nofault "$RELOC_EXE" "$WORKDIR/reloc_spike.run.log"
    echo "  Relocation spike: PASS"
fi

echo ""
echo "OK: stage07 linker spike"
echo "Linker source: $SRC"
echo "Linker exe: $STAGE7_EXE"
echo "Emulator: $EMU"
echo "Artifacts: $WORKDIR"
