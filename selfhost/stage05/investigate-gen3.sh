#!/usr/bin/env bash
set -euo pipefail

# Investigate the gen3 hang: build gen1 and gen2 with dbt,
# then run gen2→gen3 with slow32-fast -p to probe where it hangs.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$SELFHOST_DIR/.." && pwd)"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

DBT="$ROOT_DIR/tools/dbt/slow32-dbt"
FAST="$ROOT_DIR/tools/emulator/slow32-fast"

[[ -x "$DBT" ]] || { echo "Missing: $DBT" >&2; exit 1; }
[[ -x "$FAST" ]] || { echo "Missing: $FAST" >&2; exit 1; }

STAGE4_CC="$SELFHOST_DIR/stage04/cc.s32x"
STAGE4_AS="$SELFHOST_DIR/stage04/s32-as.s32x"
STAGE4_LD="$SELFHOST_DIR/stage04/s32-ld.s32x"

for f in "$STAGE4_CC" "$STAGE4_AS" "$STAGE4_LD"; do
    [[ -f "$f" ]] || { echo "Missing: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/stage05-gen3-investigate.XXXXXX)"
echo "Workdir: $WORKDIR"
# Don't auto-clean — we want to inspect artifacts

cd "$ROOT_DIR"

LIBC_DIR="$SCRIPT_DIR/libc"
CRT0_SRC="$SCRIPT_DIR/crt0.s"
MMIO_NO_START_SRC="$SCRIPT_DIR/mmio_no_start.s"

# ============================================================
# Step 1: Build runtime + libc (all with dbt)
# ============================================================
echo "=== Step 1: Build runtime + libc ==="

"$DBT" "$STAGE4_AS" "$CRT0_SRC" "$WORKDIR/crt0.s32o" >/dev/null 2>&1
"$DBT" "$STAGE4_AS" "$MMIO_NO_START_SRC" "$WORKDIR/mmio_no_start.s32o" >/dev/null 2>&1

LIBC_OBJS=""
for name in string_extra string_more ctype convert stdio malloc; do
    "$DBT" "$STAGE4_CC" "$LIBC_DIR/${name}.c" "$WORKDIR/${name}.s" >/dev/null 2>&1
    "$DBT" "$STAGE4_AS" "$WORKDIR/${name}.s" "$WORKDIR/${name}.s32o" >/dev/null 2>&1
    LIBC_OBJS="$LIBC_OBJS $WORKDIR/${name}.s32o"
done
"$DBT" "$STAGE4_CC" "$LIBC_DIR/start.c" "$WORKDIR/start.s" >/dev/null 2>&1
"$DBT" "$STAGE4_AS" "$WORKDIR/start.s" "$WORKDIR/start.s32o" >/dev/null 2>&1

LINK_OBJS="$WORKDIR/crt0.s32o @CC_OBJ@ $WORKDIR/start.s32o $WORKDIR/mmio_no_start.s32o $LIBC_OBJS"

echo "  runtime built OK"

# ============================================================
# Step 2: Build gen1 (stage04 compiles s12cc.c)
# ============================================================
echo "=== Step 2: Build gen1 (stage04 → s12cc.c) ==="

timeout 600 "$DBT" "$STAGE4_CC" "$SCRIPT_DIR/s12cc.c" "$WORKDIR/gen1.s" >/dev/null 2>&1
echo "  gen1.s: $(wc -l < "$WORKDIR/gen1.s") lines"

"$DBT" "$STAGE4_AS" "$WORKDIR/gen1.s" "$WORKDIR/gen1.s32o" >/dev/null 2>&1
"$DBT" "$STAGE4_LD" -o "$WORKDIR/gen1.s32x" --mmio 64K \
    "$WORKDIR/crt0.s32o" "$WORKDIR/gen1.s32o" "$WORKDIR/start.s32o" "$WORKDIR/mmio_no_start.s32o" \
    $LIBC_OBJS >/dev/null 2>&1

echo "  gen1.s32x: $(wc -c < "$WORKDIR/gen1.s32x") bytes"

# ============================================================
# Step 3: Build gen2 (gen1 compiles s12cc.c) — dbt
# ============================================================
echo "=== Step 3: Build gen2 (gen1 → s12cc.c) ==="

timeout 600 "$DBT" "$WORKDIR/gen1.s32x" "$SCRIPT_DIR/s12cc.c" "$WORKDIR/gen2.s" >/dev/null 2>&1
echo "  gen2.s: $(wc -l < "$WORKDIR/gen2.s") lines"

"$DBT" "$STAGE4_AS" "$WORKDIR/gen2.s" "$WORKDIR/gen2.s32o" >/dev/null 2>&1
"$DBT" "$STAGE4_LD" -o "$WORKDIR/gen2.s32x" --mmio 64K \
    "$WORKDIR/crt0.s32o" "$WORKDIR/gen2.s32o" "$WORKDIR/start.s32o" "$WORKDIR/mmio_no_start.s32o" \
    $LIBC_OBJS >/dev/null 2>&1

echo "  gen2.s32x: $(wc -c < "$WORKDIR/gen2.s32x") bytes"

# ============================================================
# Step 4: Run gen2 → gen3 with slow32-fast -p (THE HANG)
# ============================================================
echo ""
echo "=== Step 4: gen2 → gen3 (slow32-fast -p 1) ==="
echo "  This is where the hang occurs. Ctrl+C to get register dump."
echo "  gen2.s32x: $WORKDIR/gen2.s32x"
echo "  Output:    $WORKDIR/gen3.s"
echo ""

"$FAST" -p 1 "$WORKDIR/gen2.s32x" "$SCRIPT_DIR/s12cc.c" "$WORKDIR/gen3.s"
