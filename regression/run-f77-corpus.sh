#!/usr/bin/env bash
# gfortran's own execute tests through fortran/'s f77 on SLOW-32, native gfortran
# as the oracle, held to fortran/tests/gfortran-verdicts.txt.  Fails if a test
# that agreed stops agreeing, and names any that got better: record those with
# --update and commit the file.  After a change to fortran/ or the HIR backends.
#
# Most of the corpus is past Fortran 95, and f77 refuses it by name: of 3,222
# tests it takes about 750.  What the gate holds is those -- and that a test f77
# refused is not now compiled to another answer (a refusal that becomes
# `exit-differs` is reported as a NOTE: read it).  Its first run (2026-11-10)
# found some fifty defects in a compiler whose own suite, FCVS, the fuzzer and
# LAPACK were all green: docs/plans/fortran90.md has the list, and what is left
# and why.
#
# The corpus is flang's (regression/run-flang-corpus.sh says how to fetch it; GPL,
# never in git).  Anything missing is exit 2, not a pass.  The verdicts are
# macOS's: gfortran, and so what it says of host-dependent tests, varies by host.
# ~6 min on an M5.
set -u
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"; ROOT="$(dirname "$HERE")"
REFS="${LLVM_TEST_SUITE:-$HOME/refs/llvm-test-suite}"
CORPUS_COMMIT=d3f64b96fe6176716771a2e172f799caf1239b5b
VERDICTS="${VERDICTS:-$ROOT/fortran/tests/gfortran-verdicts.txt}"
export SUITE="$REFS/Fortran/gfortran/regression"; export GFORTRAN="${GFORTRAN:-gfortran}"
die() { echo "run-f77-corpus: $1" >&2; exit 2; }

[ -d "$SUITE" ] || die "no corpus at $SUITE"
have="$(git -C "$REFS" rev-parse HEAD 2>/dev/null)"
[ "$have" = "$CORPUS_COMMIT" ] || die "corpus is at ${have:-nothing}, not $CORPUS_COMMIT"
command -v "$GFORTRAN" > /dev/null || die "no $GFORTRAN"
[ -x "$ROOT/fortran/out/f77" ] && [ -f "$ROOT/fortran/runtime/libf77.s32o" ] || die "no f77: run fortran/build.sh"
[ -x "$ROOT/tools/dbt/slow32-dbt" ] || die "no slow32-dbt"

WORK="$(mktemp -d "${TMPDIR:-/tmp}/f77-corpus.XXXXXX")" || exit 2
echo "f77 corpus: results in $WORK"
if [ "${1:-}" = --update ]; then
    python3 "$ROOT/flang/tests/gfortran-suite.py" "$WORK/run" --f77 --update "$VERDICTS" | sed -n '/agree where/p;/recorded/p'
    exit "${PIPESTATUS[0]}"
fi
[ -f "$VERDICTS" ] || die "no $VERDICTS: run with --update"
python3 "$ROOT/flang/tests/gfortran-suite.py" "$WORK/run" --f77 --check "$VERDICTS" | grep -E '^(MISSING|NEW|NOTE|BETTER|WORSE|f77 corpus|agree where)'
exit "${PIPESTATUS[0]}"
