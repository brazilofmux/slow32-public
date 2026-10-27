#!/bin/bash
# run-stage08-fuzz.sh [N] [FIRST] -- random-program differential for the
# stage08 C compiler: N generated programs (default 2000) from seed FIRST
# (default 1), each built with clang and with selfhost/stage08/cc.s32x,
# outputs compared.  Failures land in regression/results-fuzz/ as
# seed-N.c and seed-N.txt; `fuzz/cfuzz.py reduce N` shrinks one.  See
# fuzz/cfuzz.py for the generator, the host-compiler tie-break and the
# reducer.  KIT (default ~/s32x) supplies crt0, libc and headers.
set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"
exec python3 "$HERE/fuzz/cfuzz.py" run -n "${1:-2000}" -s "${2:-1}" --keep "$HERE/results-fuzz"
