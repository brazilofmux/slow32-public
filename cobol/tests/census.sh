#!/bin/bash
# census.sh [DIR] -- the census over every program we have: which
# elementary items stand alone (docs/plans/census.md).  Compiles the
# corpora asm-snapshot.sh knows (the harness's programs, Open Systems,
# majesty, CCVS-85, X-COBOL) with S32_CENSUS_DIR set, then reads what the
# compiler left with census.py.  DIR keeps the census files and the
# assembly; without it a temporary directory is used and removed.
#
#   census.sh /tmp/census            every corpus, the report on stdout
#   SNAP_SKIP="ccvs xcobol" census.sh      leave the slow corpora out
#   tests/census.py DIR/census --by-file   afterwards: program by program
#   tests/census.py DIR/census --items alone --corpus majesty
set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"
if [ $# -ge 1 ]; then D="$1"; mkdir -p "$D"; else D="$(mktemp -d "${TMPDIR:-/tmp}/census.XXXXXX")"; fi
mkdir -p "$D/census" "$D/asm"
S32_CENSUS_DIR="$D/census" "$HERE/asm-snapshot.sh" "$D/asm" >"$D/snapshot.log" 2>&1
tail -1 "$D/snapshot.log" >&2
python3 "$HERE/census.py" "$D/census"
if [ $# -lt 1 ]; then rm -r "$D/census" "$D/asm"; rm "$D/snapshot.log"; rmdir "$D"; fi
