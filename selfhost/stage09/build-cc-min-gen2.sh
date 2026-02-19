#!/usr/bin/env bash
set -euo pipefail

# Build gen2 cc-min.s32x: cc-min compiles itself.
# Uses: Stage 08 cc-min.s32x (Gen1 compiler), Stage 05 s32-as.s32x (assembler),
#       Stage 07 s32-ld.s32x (linker).
# Libc is compiled by cc.fth (stage01) since cc-min doesn't yet support
# all features used in the libc sources (e.g. postfix ++).
# Deposits the self-compiled artifact in the script's directory.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$SELFHOST_DIR/../.." && pwd 2>/dev/null || cd "$SELFHOST_DIR/.." && pwd)"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

EMU="${SELFHOST_EMU:-$SELFHOST_DIR/stage00/s32-emu}"
STAGE5_AS="$SELFHOST_DIR/stage05/s32-as.s32x"
STAGE7_LD="$SELFHOST_DIR/stage07/s32-ld.s32x"
GEN1_CC="$SELFHOST_DIR/stage08/cc-min.s32x"

LIBC_DIR="$SELFHOST_DIR/stage05/libc"
CRT0_SRC="$SELFHOST_DIR/stage05/crt0.s"
MMIO_NO_START_SRC="$SELFHOST_DIR/stage05/mmio_no_start.s"
STAGE08_DIR="$SELFHOST_DIR/stage08"
CCMIN_PASS1="$STAGE08_DIR/cc-min-pass1.c"
CCMIN_PASS2="$STAGE08_DIR/cc-min-pass2.c"
CCMIN_PASS3="$STAGE08_DIR/cc-min-pass3.c"
CCMIN_MAIN="$STAGE08_DIR/cc-min.c"
OUT_EXE="$SCRIPT_DIR/cc-min.s32x"

for f in "$EMU" "$STAGE5_AS" "$STAGE7_LD" "$GEN1_CC" \
         "$CRT0_SRC" "$MMIO_NO_START_SRC" \
         "$CCMIN_PASS1" "$CCMIN_PASS2" "$CCMIN_PASS3" "$CCMIN_MAIN"; do
    [[ -f "$f" ]] || { echo "Missing: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/stage09-build.XXXXXX)"
trap 'rm -rf "$WORKDIR"' EXIT

cd "$ROOT_DIR"

compile() {
    local src="$1" asm="$2" log="$3"
    set +e
    timeout "${EXEC_TIMEOUT:-180}" "$EMU" "$GEN1_CC" "$src" "$asm" >"$log" 2>&1
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

# --- Build libc (compiled by cc-min, not cc.fth!) ---
echo "[2/4] Build libc"
LIBC_OBJS=""
for name in string_extra convert stdio; do
    compile "$LIBC_DIR/${name}.c" "$WORKDIR/${name}.s" "$WORKDIR/${name}.cc.log"
    assemble "$WORKDIR/${name}.s" "$WORKDIR/${name}.s32o" "$WORKDIR/${name}.as.log"
    LIBC_OBJS="$LIBC_OBJS $WORKDIR/${name}.s32o"
done
compile "$LIBC_DIR/start.c" "$WORKDIR/start.s" "$WORKDIR/start.cc.log"
assemble "$WORKDIR/start.s" "$WORKDIR/start.s32o" "$WORKDIR/start.as.log"

# --- Gen1 compiles cc-min → Gen2 ---
echo "[3/4] Compile cc-min (Gen1 → Gen2)"
cat "$CCMIN_PASS1" "$CCMIN_PASS2" "$CCMIN_PASS3" "$CCMIN_MAIN" > "$WORKDIR/cc-min.merged.c"
compile "$WORKDIR/cc-min.merged.c" "$WORKDIR/cc-min.s" "$WORKDIR/cc-min.cc.log"
assemble "$WORKDIR/cc-min.s" "$WORKDIR/cc-min.s32o" "$WORKDIR/cc-min.as.log"

# --- Link Gen2 using C linker ---
echo "[4/4] Link cc-min.s32x (Gen2)"
link_exe "$WORKDIR/link.log" -o "$OUT_EXE" --mmio 64K \
    "$WORKDIR/crt0.s32o" "$WORKDIR/cc-min.s32o" "$WORKDIR/start.s32o" \
    "$WORKDIR/mmio_no_start.s32o" \
    $LIBC_OBJS
[[ -s "$OUT_EXE" ]] || { echo "link failed" >&2; exit 1; }

echo "OK: $OUT_EXE ($(wc -c < "$OUT_EXE") bytes)"
