#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="${SELFHOST_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

EMU="${STAGE7_EMU:-$ROOT_DIR/tools/emulator/slow32-fast}"
KERNEL="${STAGE7_KERNEL:-$ROOT_DIR/forth/kernel.s32x}"
PRELUDE="${STAGE7_PRELUDE:-$ROOT_DIR/forth/prelude.fth}"
ASM_FTH="${STAGE7_ASM:-$ROOT_DIR/selfhost/v2/stage01/asm.fth}"
EXPECT_PASS=0
EXPECT_SET=0
KEEP_ARTIFACTS=0
MODE="direct"

usage() {
    cat <<USAGE
Usage: $0 [--emu <path>] [--mode direct|extract] [--expect-pass] [--expect-fail] [--keep-artifacts]

Builds the current Stage07 linker via run-spike, then constructs an archive
resolution scenario:
  - main object has unresolved call to helper
  - helper is packaged into a .s32a archive member
  - invokes stage07 linker with: <main.s32o> <lib.s32a> <out.s32x>

Modes:
  direct   Invoke linker with archive argument directly (currently expected-fail).
  extract  Extract first archive member first, then link object-only (expected-pass).
USAGE
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --emu)
            shift
            [[ $# -gt 0 ]] || { echo "--emu requires a path" >&2; exit 2; }
            EMU="$1"
            ;;
        --expect-pass)
            EXPECT_PASS=1
            EXPECT_SET=1
            ;;
        --expect-fail)
            EXPECT_PASS=0
            EXPECT_SET=1
            ;;
        --mode)
            shift
            [[ $# -gt 0 ]] || { echo "--mode requires direct|extract" >&2; exit 2; }
            MODE="$1"
            ;;
        --keep-artifacts)
            KEEP_ARTIFACTS=1
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

case "$MODE" in
    direct|extract) ;;
    *) echo "invalid --mode: $MODE (expected direct|extract)" >&2; exit 2 ;;
esac

if [[ "$EXPECT_SET" -eq 0 ]]; then
    if [[ "$MODE" == "extract" ]]; then
        EXPECT_PASS=1
    else
        EXPECT_PASS=0
    fi
fi

if [[ "$EMU" != /* ]]; then
    EMU="$ROOT_DIR/$EMU"
fi

for f in "$EMU" "$KERNEL" "$PRELUDE" "$ASM_FTH"; do
    [[ -f "$f" ]] || { echo "Missing required file: $f" >&2; exit 1; }
done

TMP_LOG="$(mktemp /tmp/stage07-archive-spike.XXXXXX.log)"
"$ROOT_DIR/selfhost/v2/stage07/run-spike.sh" --emu "$EMU" --keep-artifacts >"$TMP_LOG"
LINKER_EXE="$(awk -F': ' '/^Linker exe:/{print $2}' "$TMP_LOG" | tail -n 1)"
WORKDIR="$(awk -F': ' '/^Artifacts:/{print $2}' "$TMP_LOG" | tail -n 1)"
[[ -n "$LINKER_EXE" && -f "$LINKER_EXE" ]] || { echo "failed to locate linker exe from run-spike output" >&2; exit 1; }
[[ -n "$WORKDIR" && -d "$WORKDIR" ]] || { echo "failed to locate run-spike artifacts dir" >&2; exit 1; }

if [[ "$KEEP_ARTIFACTS" -eq 0 ]]; then
    trap 'rm -f "$TMP_LOG"; rm -rf "$WORKDIR"' EXIT
fi

run_forth() {
    local script_a="$1"
    local cmd_text="$2"
    local log_file="$3"
    set +e
    cat "$PRELUDE" "$script_a" - <<FTH | timeout 180 "$EMU" "$KERNEL" >"$log_file" 2>&1
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

assemble_forth() {
    local asm="$1"
    local obj="$2"
    local log="$3"
    run_forth "$ASM_FTH" "S\" $asm\" S\" $obj\" ASSEMBLE
BYE" "$log"
    [[ -s "$obj" ]] || { echo "assembler produced no output: $asm" >&2; return 1; }
    if grep -q "FAILED:" "$log"; then
        echo "assembler failed: $asm" >&2
        tail -n 60 "$log" >&2
        return 1
    fi
}

cat > "$WORKDIR/main_call.s" <<'ASM'
.text
.global main
main:
    jal r1, helper
    halt
ASM

cat > "$WORKDIR/helper.s" <<'ASM'
.text
.global helper
helper:
    halt
ASM

MAIN_OBJ="$WORKDIR/main_call.s32o"
HELPER_OBJ="$WORKDIR/helper.s32o"
ARCHIVE="$WORKDIR/libhelper.s32a"
OUT_EXE="$WORKDIR/archive_spike.s32x"

assemble_forth "$WORKDIR/main_call.s" "$MAIN_OBJ" "$WORKDIR/main_call.as.log"
assemble_forth "$WORKDIR/helper.s" "$HELPER_OBJ" "$WORKDIR/helper.as.log"

cc -O2 -Wall -Wextra -std=c11 -pedantic "$ROOT_DIR/selfhost/v2/stage04/validation/s32-ar.c" -o "$WORKDIR/s32-ar-host"
"$WORKDIR/s32-ar-host" c "$ARCHIVE" "$HELPER_OBJ"

if [[ "$MODE" == "extract" ]]; then
    mkdir -p "$WORKDIR/extract"
    (
        cd "$WORKDIR/extract"
        "$WORKDIR/s32-ar-host" x "$ARCHIVE" helper.s32o
    )
    [[ -f "$WORKDIR/extract/helper.s32o" ]] || { echo "extract mode failed to recover helper.s32o" >&2; exit 1; }
    set +e
    timeout 60 "$EMU" "$LINKER_EXE" "$MAIN_OBJ" "$WORKDIR/extract/helper.s32o" "$OUT_EXE" >"$WORKDIR/archive_spike.link.log" 2>&1
    RC=$?
    set -e
else
    set +e
    timeout 60 "$EMU" "$LINKER_EXE" "$MAIN_OBJ" "$ARCHIVE" "$OUT_EXE" >"$WORKDIR/archive_spike.link.log" 2>&1
    RC=$?
    set -e
fi

if [[ "$EXPECT_PASS" -eq 1 ]]; then
    if [[ "$RC" -ne 0 && "$RC" -ne 96 ]]; then
        echo "archive spike failed (expected pass), rc=$RC" >&2
        tail -n 60 "$WORKDIR/archive_spike.link.log" >&2
        exit 1
    fi
    [[ -s "$OUT_EXE" ]] || { echo "archive spike expected output executable, got none" >&2; exit 1; }
else
    if [[ "$RC" -eq 0 || "$RC" -eq 96 ]]; then
        echo "archive spike unexpectedly passed (expected fail)" >&2
        exit 1
    fi
fi

echo "OK: stage07 archive spike mode=$MODE ($( [[ "$EXPECT_PASS" -eq 1 ]] && echo "pass" || echo "expected-fail" ))"
echo "Linker exe: $LINKER_EXE"
echo "Main object: $MAIN_OBJ"
echo "Archive: $ARCHIVE"
echo "Output: $OUT_EXE"
echo "Artifacts: $WORKDIR"
