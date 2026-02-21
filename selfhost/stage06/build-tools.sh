#!/usr/bin/env bash
set -euo pipefail

# Build stage06 tools: s32-as.s32x, s32-ar.s32x, s32-ld.s32x
# All compiled by stage05's cc.s32x, assembled by stage05 assembler,
# linked by stage05 linker. Uses stage06's own libc/runtime.

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
TOOLS_DIR="$SCRIPT_DIR/tools"

for f in "$EMU" "$STAGE5_CC" "$STAGE5_AS" "$STAGE5_LD" \
         "$CRT0_SRC" "$MMIO_NO_START_SRC" \
         "$TOOLS_DIR/s32-as.c" "$TOOLS_DIR/s32_formats_min.h" \
         "$TOOLS_DIR/s32-ar.c" "$TOOLS_DIR/s32ar_min.h" \
         "$TOOLS_DIR/s32-ld.c"; do
    [[ -f "$f" ]] || { echo "Missing: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/stage06-tools.XXXXXX)"
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
echo "[1/5] Assemble runtime"
assemble "$CRT0_SRC" "$WORKDIR/crt0.s32o" "$WORKDIR/crt0.log"
assemble "$MMIO_NO_START_SRC" "$WORKDIR/mmio_no_start.s32o" "$WORKDIR/mmio_no_start.log"

# --- Build libc (compiled by stage05 s12cc) ---
echo "[2/5] Build libc"
LIBC_OBJS=""
for name in string_extra string_more ctype convert stdio malloc; do
    compile "$LIBC_DIR/${name}.c" "$WORKDIR/${name}.s" "$WORKDIR/${name}.cc.log"
    assemble "$WORKDIR/${name}.s" "$WORKDIR/${name}.s32o" "$WORKDIR/${name}.as.log"
    LIBC_OBJS="$LIBC_OBJS $WORKDIR/${name}.s32o"
done
compile "$LIBC_DIR/start.c" "$WORKDIR/start.s" "$WORKDIR/start.cc.log"
assemble "$WORKDIR/start.s" "$WORKDIR/start.s32o" "$WORKDIR/start.as.log"

# --- Build assembler ---
echo "[3/5] Build s32-as.s32x"
compile "$TOOLS_DIR/s32-as.c" "$WORKDIR/s32-as.s" "$WORKDIR/s32-as.cc.log"
assemble "$WORKDIR/s32-as.s" "$WORKDIR/s32-as.s32o" "$WORKDIR/s32-as.as.log"
link_exe "$WORKDIR/s32-as.link.log" -o "$SCRIPT_DIR/s32-as.s32x" --mmio 64K \
    "$WORKDIR/crt0.s32o" "$WORKDIR/s32-as.s32o" "$WORKDIR/start.s32o" \
    "$WORKDIR/mmio_no_start.s32o" \
    $LIBC_OBJS
[[ -s "$SCRIPT_DIR/s32-as.s32x" ]] || { echo "s32-as link failed" >&2; exit 1; }
echo "  OK: s32-as.s32x ($(wc -c < "$SCRIPT_DIR/s32-as.s32x") bytes)"

# --- Build archiver ---
echo "[4/5] Build s32-ar.s32x"
compile "$TOOLS_DIR/s32-ar.c" "$WORKDIR/s32-ar.s" "$WORKDIR/s32-ar.cc.log"
assemble "$WORKDIR/s32-ar.s" "$WORKDIR/s32-ar.s32o" "$WORKDIR/s32-ar.as.log"
link_exe "$WORKDIR/s32-ar.link.log" -o "$SCRIPT_DIR/s32-ar.s32x" --mmio 64K \
    "$WORKDIR/crt0.s32o" "$WORKDIR/s32-ar.s32o" "$WORKDIR/start.s32o" \
    "$WORKDIR/mmio_no_start.s32o" \
    $LIBC_OBJS
[[ -s "$SCRIPT_DIR/s32-ar.s32x" ]] || { echo "s32-ar link failed" >&2; exit 1; }
echo "  OK: s32-ar.s32x ($(wc -c < "$SCRIPT_DIR/s32-ar.s32x") bytes)"

# --- Build linker ---
echo "[5/5] Build s32-ld.s32x"
compile "$TOOLS_DIR/s32-ld.c" "$WORKDIR/s32-ld.s" "$WORKDIR/s32-ld.cc.log"
assemble "$WORKDIR/s32-ld.s" "$WORKDIR/s32-ld.s32o" "$WORKDIR/s32-ld.as.log"
link_exe "$WORKDIR/s32-ld.link.log" -o "$SCRIPT_DIR/s32-ld.s32x" --mmio 64K \
    "$WORKDIR/crt0.s32o" "$WORKDIR/s32-ld.s32o" "$WORKDIR/start.s32o" \
    "$WORKDIR/mmio_no_start.s32o" \
    $LIBC_OBJS
[[ -s "$SCRIPT_DIR/s32-ld.s32x" ]] || { echo "s32-ld link failed" >&2; exit 1; }
echo "  OK: s32-ld.s32x ($(wc -c < "$SCRIPT_DIR/s32-ld.s32x") bytes)"

echo ""
echo "All tools built successfully."
