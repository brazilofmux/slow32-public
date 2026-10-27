#!/bin/bash
# run-stage08-fuzz.sh [N] [FIRST] -- random-program differential for the
# stage08 C compiler: N generated programs (default 2000) from seed FIRST
# (default 1) in each of two profiles -- wide (globals, pointers,
# structs, 64-bit, signed, floating point, while/do, goto) and narrow
# (the scalar subset, where the shapes found first stay dense) -- each
# built with clang and with selfhost/stage08/cc.s32x, outputs compared.
# Failures land in regression/results-fuzz/{wide,narrow}/ as seed-N.c
# and seed-N.txt; `fuzz/cfuzz.py reduce N --profile P` shrinks one.  See
# fuzz/cfuzz.py for the generator, the host-compiler tie-break and the
# reducer.  KIT (default ~/s32x) supplies crt0, libc and headers.
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
rc=0
for p in wide narrow; do
    python3 "$HERE/fuzz/cfuzz.py" run --profile $p -n "${1:-2000}" -s "${2:-1}" --keep "$HERE/results-fuzz/$p" || rc=1
done
exit $rc
