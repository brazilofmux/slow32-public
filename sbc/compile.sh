#!/bin/bash
# compile.sh [--qb | --any] prog.bas [prog.s32x] -- a BASIC program to a
# .s32x, by whichever of the two compilers takes it (README.md):
#
#   s32-bc   the QuickBASIC compiler: a program QBasic 1.1 would load
#            (the checker passes it), compiled up front to native code
#            -- tried first;
#   s32-sbc  the SLOW BASIC compiler: anything the interpreter runs
#            (GW-BASIC's line numbers, ECMA-55, the interpreter's own
#            statements), the executor beside the compiled C -- what a
#            program s32-bc refuses gets, with the refusal said.
#
#   --qb     s32-bc only (a refusal is the failure)
#   --any    s32-sbc only
#
# Either way the program prints what the interpreter prints: that is
# each compiler's gate (tests/run-oracle.py, and --bc).
set -euo pipefail
SBC_DIR="$(cd "$(dirname "$0")" && pwd)"
MODE=auto
case "${1:-}" in
    --qb) MODE=qb; shift ;;
    --any) MODE=any; shift ;;
esac
SRC="${1:?usage: compile.sh [--qb | --any] prog.bas [prog.s32x]}"
OUT="${2:-${SRC%.*}.s32x}"

if [ "$MODE" != any ]; then
    ERR="$(mktemp "${TMPDIR:-/tmp}/compile.XXXXXX")"
    trap 'rm -f "${ERR:?}"' EXIT
    if bash "$SBC_DIR/compile-bc.sh" "$SRC" "$OUT" 2>"$ERR"; then
        echo "compile.sh: $OUT by s32-bc (QuickBASIC)" >&2
        exit 0
    fi
    if [ "$MODE" = qb ]; then
        cat "$ERR" >&2
        exit 1
    fi
    echo "compile.sh: s32-bc refused it ($(head -1 "$ERR" | sed 's/^s32-bc: //')); s32-sbc (SLOW BASIC) instead" >&2
fi
bash "$SBC_DIR/compile-bas.sh" "$SRC" "$OUT" >/dev/null
echo "compile.sh: $OUT by s32-sbc (SLOW BASIC)" >&2
