#!/usr/bin/env bash
set -euo pipefail

# Build s32-ar.s32x (the C archiver) from source.
# Uses: Stage 04 cc.fth (compiler), Stage 02 s32-as.s32x (assembler),
#       Stage 01 ar.fth (archive libc), Stage 01 link.fth (linker).
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
AR_FTH="$SELFHOST_DIR/stage01/ar.fth"
LINK_FTH="$SELFHOST_DIR/stage01/link.fth"
STAGE2_AS="$SELFHOST_DIR/stage02/s32-as.s32x"

LIBC_DIR="$SELFHOST_DIR/stage02/libc"
CRT0_SRC="$SELFHOST_DIR/stage02/crt0.s"
MMIO_NO_START_SRC="$SELFHOST_DIR/stage02/mmio_no_start.s"
AR_SRC="$SCRIPT_DIR/s32-ar.c"
OUT_EXE="$SCRIPT_DIR/s32-ar.s32x"

for f in "$EMU" "$KERNEL" "$PRELUDE" "$CC_FTH" "$AR_FTH" "$LINK_FTH" "$STAGE2_AS" \
         "$CRT0_SRC" "$MMIO_NO_START_SRC" "$AR_SRC"; do
    [[ -f "$f" ]] || { echo "Missing: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/stage06-build.XXXXXX)"
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

# Archive libc (excluding start.s32o which is linked directly)
AR_CMD="S\" $WORKDIR/libc.s32a\" AR-C-BEGIN"
for obj in $LIBC_OBJS; do
    AR_CMD="${AR_CMD}
S\" $obj\" AR-ADD"
done
AR_CMD="${AR_CMD}
AR-C-END
BYE"
run_forth "$AR_FTH" "$AR_CMD" "$WORKDIR/libc.ar.log"
[[ -s "$WORKDIR/libc.s32a" ]] || { echo "libc archive failed" >&2; exit 1; }

# --- Compile and assemble the archiver ---
echo "[3/4] Compile s32-ar.c"
compile "$AR_SRC" "$WORKDIR/s32-ar.s" "$WORKDIR/s32-ar.cc.log"
assemble "$WORKDIR/s32-ar.s" "$WORKDIR/s32-ar.s32o" "$WORKDIR/s32-ar.as.log"

# --- Link ---
echo "[4/4] Link s32-ar.s32x"
run_forth "$LINK_FTH" "LINK-INIT
S\" $WORKDIR/crt0.s32o\" LINK-OBJ
S\" $WORKDIR/s32-ar.s32o\" LINK-OBJ
S\" $WORKDIR/start.s32o\" LINK-OBJ
S\" $WORKDIR/mmio_no_start.s32o\" LINK-OBJ
65536 LINK-MMIO
S\" $WORKDIR/libc.s32a\" LINK-ARCHIVE
S\" $OUT_EXE\" LINK-EMIT
BYE" "$WORKDIR/link.log"
[[ -s "$OUT_EXE" ]] || { echo "link failed" >&2; exit 1; }

echo "OK: $OUT_EXE ($(wc -c < "$OUT_EXE") bytes)"
