#!/usr/bin/env bash
# gfortran's own execute tests through flang on SLOW-32, native gfortran as the
# oracle, held to flang/tests/gfortran-verdicts.txt.  Fails if a test that agreed
# stops agreeing, and names any that got better: record those with --update and
# commit the file.  After a change to flang, flang-rt, the SLOW-32 backend, the C
# library or the assemblers and linkers: its first day found wrong code in all of
# those that FCVS had passed (docs/plans/flang.md).
#
# Needs flang/build.sh run since flang was built, and the corpus, not in git
# (GPL), at the commit below:
#   git clone --filter=blob:none --sparse https://github.com/llvm/llvm-test-suite ~/refs/llvm-test-suite
#   git -C ~/refs/llvm-test-suite sparse-checkout set Fortran
#   git -C ~/refs/llvm-test-suite checkout CORPUS_COMMIT
# Anything missing is exit 2, not a pass.  The verdicts are macOS's: gfortran, and
# so what it says of host-dependent tests, varies by host.  ~5 min on an M5.
# Mutation-tested: NINT back to lround fails nint_2; tampered records are named.
set -u
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"; ROOT="$(dirname "$HERE")"
L="${LLVM_PROJECT:-$HOME/llvm-project}"; REFS="${LLVM_TEST_SUITE:-$HOME/refs/llvm-test-suite}"
CORPUS_COMMIT=d3f64b96fe6176716771a2e172f799caf1239b5b
VERDICTS="${VERDICTS:-$ROOT/flang/tests/gfortran-verdicts.txt}"
FLANG="$L/build-flang/bin/flang"; RTLIB="$ROOT/flang/out/libflangrt.s32a"
export SUITE="$REFS/Fortran/gfortran/regression"; export GFORTRAN="${GFORTRAN:-gfortran}"
die() { echo "run-flang-corpus: $1" >&2; exit 2; }

[ -d "$SUITE" ] || die "no corpus at $SUITE"
have="$(git -C "$REFS" rev-parse HEAD 2>/dev/null)"
[ "$have" = "$CORPUS_COMMIT" ] || die "corpus is at ${have:-nothing}, not $CORPUS_COMMIT"
command -v "$GFORTRAN" > /dev/null || die "no $GFORTRAN"
[ -x "$FLANG" ] || die "no flang at $FLANG"
[ -f "$RTLIB" ] || die "no $RTLIB: run flang/build.sh"
[ "$RTLIB" -nt "$FLANG" ] || die "flang is newer than its runtime and modules: run flang/build.sh"

"$ROOT/flang/tests/run-tests.sh" || exit 1

WORK="$(mktemp -d "${TMPDIR:-/tmp}/flang-corpus.XXXXXX")" || exit 2
echo "flang corpus: results in $WORK"
if [ "${1:-}" = --update ]; then
    python3 "$ROOT/flang/tests/gfortran-suite.py" "$WORK/run" --update "$VERDICTS" | sed -n '/agree where/p;/recorded/p'
    exit "${PIPESTATUS[0]}"
fi
[ -f "$VERDICTS" ] || die "no $VERDICTS: run with --update"
python3 "$ROOT/flang/tests/gfortran-suite.py" "$WORK/run" --check "$VERDICTS" | grep -E '^(MISSING|NEW|NOTE|BETTER|WORSE|flang corpus|agree where)'
exit "${PIPESTATUS[0]}"
