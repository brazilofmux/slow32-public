#!/bin/bash

# The kit's BASIC compilers, as shipped: docker/bin/s32kbasic (s32-bc.s32x,
# s32-sbc.s32x, cc, as, ld, all on the emulator) over sbc/tests/native, each
# program compiled both ways and required to print what the kit's own
# interpreter, sbasic.s32x, prints.  sbc/tests/run-kit.sh is the full gate
# (the corpus, against the host compilers too); this needs only the kit and
# bash, so it runs inside slow32:kit.
#
# Usage: ./run-kit-basic.sh [test-name ...]
# Env:   KIT (default ~/s32x), SLOW32_DBT, SLOW32_FAST, TIMEOUT (secs, default 60)
# Exit:  0 all agree, 1 a mismatch, 2 setup.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
export KIT="${KIT:-$HOME/s32x}"
export EMU="${SLOW32_DBT:-$ROOT/tools/dbt/slow32-dbt}"
RUN="${SLOW32_FAST:-$ROOT/tools/emulator/slow32-fast}"
DRIVER="$ROOT/docker/bin/s32kbasic"
TESTS="$ROOT/sbc/tests/native"
TIMEOUT="${TIMEOUT:-60}"
[ -f "$KIT/s32-bc.s32x" ] && [ -f "$KIT/s32-sbc.s32x" ] || { echo "no BASIC compilers in $KIT"; exit 2; }

W="$(mktemp -d "${TMPDIR:-/tmp}/kitbasic.XXXXXX")" || exit 2
trap 'rm -rf "${W:?}"' EXIT
cd "$TESTS" || exit 2

agree=0 differ=0 refused=0
if [ $# -gt 0 ]; then names=("$@"); else names=(); for f in *.bas; do names+=("${f%.bas}"); done; fi
for t in "${names[@]}"; do
    keys=/dev/null
    [ -f "$t.in" ] && keys="$t.in"
    timeout "$TIMEOUT" "$RUN" -q "$KIT/sbasic.s32x" "$t.bas" <"$keys" 2>&1 | sed '/^Loaded [0-9]* lines from /d' >"$W/want" || true
    for how in --qb --any; do
        if ! "$DRIVER" "$how" "$t.bas" "$W/p.s32x" >"$W/log" 2>&1; then
            if [ "$how" = --any ] && ! grep -q '^Error in line' "$W/want"; then
                echo "$t $how: did not compile: $(tail -1 "$W/log")"; differ=$((differ + 1))
            else
                refused=$((refused + 1))
            fi
            continue
        fi
        timeout "$TIMEOUT" "$RUN" -q "$W/p.s32x" <"$keys" >"$W/got" 2>&1 || true
        if cmp -s "$W/want" "$W/got"; then
            agree=$((agree + 1))
        else
            echo "$t $how: differs"; diff "$W/want" "$W/got" | head -4
            differ=$((differ + 1))
        fi
    done
done
echo "kit BASIC: $agree agree, $refused refused, $differ differ"
[ "$differ" = 0 ] && [ "$agree" -gt 0 ]
