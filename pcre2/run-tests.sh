#!/bin/bash
# PCRE2's own test suite against the SLOW-32 build: PCRE2's RunTest, over
# its testdata/, with out/pcre2test.s32x behind a wrapper that runs it on an
# emulator.  RunTest diffs every output against the release's expected
# output, so a pass is the release's own verdict.
#
# The test data is the release's, read from where it was unpacked (PCRE2_SRC,
# default ~/refs/pcre2-10.49 -- pcre2-10.49.tar.bz2, sha256
# 53c156e1ba416a20da8e65395daa132da0d80e76910424caca3fcdae7831d384); it is
# not in this tree.  Skips when it is absent.
#
# Expected (2026-10-20, slow32-dbt): "All tests passed."  Tests 11-13 are
# 16/32-bit, 17 JIT, 23 \C-disabled and 28-29 EBCDIC: not this build.
# Test 3 skips: none of the fr_FR locales can be set.
#
# Usage: run-tests.sh [test numbers...]   env: PCRE2_SRC, EMU (slow32-dbt;
# an engine that prints statistics on stdout, as slow32-fast does, fails
# every test on them)
set -u
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
PCRE2_SRC="${PCRE2_SRC:-$HOME/refs/pcre2-10.49}"
EMU="${EMU:-$ROOT/tools/dbt/slow32-dbt}"
BIN="$SCRIPT_DIR/out/pcre2test.s32x"
[ -f "$PCRE2_SRC/RunTest" ] && [ -d "$PCRE2_SRC/testdata" ] || {
    echo "SKIP: no PCRE2 release at $PCRE2_SRC (set PCRE2_SRC)"; exit 0; }
[ -f "$BIN" ] || { echo "missing $BIN -- run build.sh" >&2; exit 1; }
W="$(mktemp -d "${TMPDIR:-/tmp}/pcre2-tests.XXXXXX")"
trap 'rm -rf "${W:?}"' EXIT
printf '#!/bin/sh\nexec "%s" "%s" "$@"\n' "$EMU" "$BIN" > "$W/pcre2test"
chmod +x "$W/pcre2test"
cd "$W" && srcdir="$PCRE2_SRC" bash "$PCRE2_SRC/RunTest" -8 "$@"
