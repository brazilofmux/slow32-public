#!/bin/sh
# pgo-profile.sh MAP HIST > PROFILE
#
# Block frequencies for stage08's register allocator (memory model step
# 5b.6, docs/plans/memory-model.md): turn a `slow32 -p` histogram into
# the `function block count` lines HIR_PGO=<file> reads.
#
#   1. compile with HIR_PGO_LABELS=1 -- a label __pgo_<function>_<block>
#      at every emitted block, and nothing else changes;
#   2. make the labels global (sed -E 's/^([A-Za-z_][A-Za-z0-9_]*):$/
#      .globl \1\n\1:/'), assemble, and link with s32-ld --print-map;
#   3. run the training workload: slow32 -p HIST program.s32x ...;
#   4. pgo-profile.sh program.s32x.map HIST > profile;
#   5. compile again with HIR_PGO=profile.
#
# A block's count is how often its first instruction ran.  Blocks that
# never ran are left out (the allocator weighs them 1), and a function
# with no lines never ran in training and keeps the default weights.
# Block numbers are the HIR's, fixed before allocation, so a profile
# fits builds that allocate differently -- not ones that lower
# differently: re-profile after a front-end or optimizer change.
set -eu
MAP="${1:?usage: pgo-profile.sh MAP HIST}"
HIST="${2:?usage: pgo-profile.sh MAP HIST}"
awk '
    FNR == NR { cnt[toupper($1)] = $2; next }       # the histogram: hex address, count
    $3 ~ /^__pgo_/ {
        a = toupper($1); sub(/^0X/, "", a)
        c = cnt[a]
        if (c + 0 == 0) next
        name = substr($3, 7)                        # past __pgo_
        n = split(name, part, "_")
        blk = part[n]
        fn = substr(name, 1, length(name) - length(blk) - 1)
        print fn, blk, c
    }
' "$HIST" "$MAP" | LC_ALL=C sort -k1,1 -k2,2n
