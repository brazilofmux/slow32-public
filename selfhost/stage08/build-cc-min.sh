#!/usr/bin/env bash
set -euo pipefail

# Build cc-min.s32x (the C compiler) from source.
# Uses: Stage 04 cc.fth (compiler — last Forth dependency, one final time),
#       Stage 02 s32-as.s32x (C assembler), Stage 07 s32-ld.s32x (C linker).
# After this stage, ALL Forth tools (stage01-04) are no longer needed.
# Deposits the artifact in the script's directory.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$SELFHOST_DIR/../.." && pwd 2>/dev/null || cd "$SELFHOST_DIR/.." && pwd)"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

EMU="${SELFHOST_EMU:-$SELFHOST_DIR/stage00/s32-emu}"
KERNEL="${SELFHOST_KERNEL:-$ROOT_DIR/forth/kernel.s32x}"
PRELUDE="${SELFHOST_PRELUDE:-$ROOT_DIR/forth/prelude.fth}"
CC_FTH="$SELFHOST_DIR/stage01/cc.fth"
STAGE2_AS="$SELFHOST_DIR/stage02/s32-as.s32x"
STAGE7_LD="$SELFHOST_DIR/stage07/s32-ld.s32x"

LIBC_DIR="$SELFHOST_DIR/stage02/libc"
CRT0_SRC="$SELFHOST_DIR/stage02/crt0.s"
MMIO_NO_START_SRC="$SELFHOST_DIR/stage02/mmio_no_start.s"
CCMIN_SRC="$SCRIPT_DIR/cc-min.c"
CCMIN_PASS1="$SCRIPT_DIR/cc-min-pass1.c"
CCMIN_PASS2="$SCRIPT_DIR/cc-min-pass2.c"
CCMIN_PASS3="$SCRIPT_DIR/cc-min-pass3.c"
OUT_EXE="$SCRIPT_DIR/cc-min.s32x"

for f in "$EMU" "$KERNEL" "$PRELUDE" "$CC_FTH" "$STAGE2_AS" "$STAGE7_LD" \
         "$CRT0_SRC" "$MMIO_NO_START_SRC" \
         "$CCMIN_SRC" "$CCMIN_PASS1" "$CCMIN_PASS2" "$CCMIN_PASS3"; do
    [[ -f "$f" ]] || { echo "Missing: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/stage08-build.XXXXXX)"
trap 'rm -rf "$WORKDIR"' EXIT

# cc.fth uses relative include paths — run from repo root
cd "$ROOT_DIR"

run_forth() {
    local script="$1"
    local body="$2"
    local log="$3"
    set +e
    cat "$PRELUDE" "$script" - <<FTH | timeout 300 "$EMU" "$KERNEL" >"$log" 2>&1
$body
FTH
    local rc=$?
    set -e
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        echo "forth failed (rc=$rc)" >&2
        tail -n 40 "$log" >&2
        return 1
    fi
}

compile() {
    local src="$1" asm="$2" log="$3"
    run_forth "$CC_FTH" "S\" $src\" S\" $asm\" COMPILE-FILE
BYE" "$log"
    [[ -s "$asm" ]] || { echo "compile failed: $src" >&2; return 1; }
}

assemble() {
    local src="$1" obj="$2" log="$3"
    set +e
    timeout 120 "$EMU" "$STAGE2_AS" "$src" "$obj" >"$log" 2>&1
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

# --- Build libc ---
echo "[2/4] Build libc"
LIBC_OBJS=""
for name in string_extra convert stdio; do
    compile "$LIBC_DIR/${name}.c" "$WORKDIR/${name}.s" "$WORKDIR/${name}.cc.log"
    assemble "$WORKDIR/${name}.s" "$WORKDIR/${name}.s32o" "$WORKDIR/${name}.as.log"
    LIBC_OBJS="$LIBC_OBJS $WORKDIR/${name}.s32o"
done
compile "$LIBC_DIR/start.c" "$WORKDIR/start.s" "$WORKDIR/start.cc.log"
assemble "$WORKDIR/start.s" "$WORKDIR/start.s32o" "$WORKDIR/start.as.log"

# --- Compile and assemble cc-min ---
echo "[3/4] Compile cc-min"
# Concatenate pass files into single compilation unit (cc.fth compiles one file at a time)
cat "$CCMIN_PASS1" "$CCMIN_PASS2" "$CCMIN_PASS3" "$CCMIN_SRC" > "$WORKDIR/cc-min.merged.c"
compile "$WORKDIR/cc-min.merged.c" "$WORKDIR/cc-min.s" "$WORKDIR/cc-min.cc.log"
assemble "$WORKDIR/cc-min.s" "$WORKDIR/cc-min.s32o" "$WORKDIR/cc-min.as.log"

# --- Link using Stage 07 C linker (no more Forth!) ---
echo "[4/4] Link cc-min.s32x"
link_exe "$WORKDIR/link.log" -o "$OUT_EXE" --mmio 64K \
    "$WORKDIR/crt0.s32o" "$WORKDIR/cc-min.s32o" "$WORKDIR/start.s32o" \
    "$WORKDIR/mmio_no_start.s32o" \
    $LIBC_OBJS
[[ -s "$OUT_EXE" ]] || { echo "link failed" >&2; exit 1; }

echo "OK: $OUT_EXE ($(wc -c < "$OUT_EXE") bytes)"
