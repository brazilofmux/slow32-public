#!/bin/bash
# GitHub issue 74: a JAL past ±1MB must link via a veneer and still run.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
AS="$ROOT/tools/assembler/slow32asm"
LD="$ROOT/tools/linker/s32-ld"
EMU="${SELFHOST_EMU:-$ROOT/tools/dbt/slow32-dbt}"
W="$(mktemp -d /tmp/s32-veneer.XXXXXX)"
trap 'rm -rf "$W"' EXIT

cat > "$W/near.s" <<'EOF'
.global _start
_start:
    jal r31, near
    halt
near:
    addi r1, r0, 7
    jalr r0, r31, 0
EOF
"$AS" "$W/near.s" "$W/near.s32o" >/dev/null
"$LD" -o "$W/near.s32x" "$W/near.s32o"
rc="$("$EMU" "$W/near.s32x" >/dev/null; echo $?)"
[ "$rc" = 7 ] || { echo "near JAL: want rc=7 got $rc" >&2; exit 1; }

cat > "$W/far.s" <<'EOF'
.global _start
_start:
    jal r31, far
    halt
.space 0x100010
far:
    addi r1, r0, 42
    jalr r0, r31, 0
EOF
"$AS" "$W/far.s" "$W/far.s32o" >/dev/null
"$LD" --code-size 2M -o "$W/far.s32x" "$W/far.s32o" 2>"$W/ld.err"
grep -q 'JAL veneers' "$W/ld.err" || { echo "far JAL: expected veneer note" >&2; cat "$W/ld.err" >&2; exit 1; }
rc="$("$EMU" "$W/far.s32x" >/dev/null; echo $?)"
[ "$rc" = 42 ] || { echo "far JAL: want rc=42 got $rc" >&2; exit 1; }
echo "OK: JAL veneers (near rc=7, far rc=42)"
