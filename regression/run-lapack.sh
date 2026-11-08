#!/usr/bin/env bash
# Reference LAPACK and its own test suite on SLOW-32, built fresh by its own Makefiles,
# gfortran beside it with the arithmetic this machine has (flang/tests/lapack/run.py).
# Fails if a suite doesn't finish, runs another number of tests, or has more over its
# threshold than the host.  The corpus asks whether a program passes; this looks at
# numbers: x**n by pow, a last place off, held the corpus steady and failed csep here.
# After a change to flang, flang-rt, the SLOW-32 backend or the C library.
# --f77: SLOW-32's side built by fortran/'s f77 instead (fortran/tests/lapack/s32fc),
# the DMD routines and their four suites left out (USE ISO_FORTRAN_ENV, F2003);
# after a change to fortran/.
#
# Needs flang/build.sh run since flang was built, and LAPACK, not in git, at the commit
# run.py pins:
#   git clone https://github.com/Reference-LAPACK/lapack ~/refs/lapack
#   git -C ~/refs/lapack checkout v3.12.1
# Anything missing is exit 2, not a pass.  A fail keeps its directory, about 640 MB, to
# read; a pass removes it.  ~5 min on an M5.
# Mutation-tested: without the powi libcall csep fails; with the DBT's libm stubs, ssep;
# an engine that dies is a suite that didn't finish.
set -u
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"; ROOT="$(dirname "$HERE")"
L="${LLVM_PROJECT:-$HOME/llvm-project}"; LAPACK="${LAPACK:-$HOME/refs/lapack}"
FLANG="$L/build-flang/bin/flang"; RTLIB="$ROOT/flang/out/libflangrt.s32a"
die() { echo "run-lapack: $1" >&2; exit 2; }

F77=0
for a in "$@"; do [ "$a" = --f77 ] && F77=1; done
[ -d "$LAPACK" ] || die "no LAPACK at $LAPACK"
command -v gfortran > /dev/null || die "no gfortran"
if [ "$F77" = 1 ]; then
    [ -x "$ROOT/fortran/out/f77" ] && [ -f "$ROOT/fortran/runtime/libf77.s32o" ] || die "no f77: run fortran/build.sh"
else
    [ -x "$FLANG" ] || die "no flang at $FLANG"
    [ -f "$RTLIB" ] || die "no $RTLIB: run flang/build.sh"
    [ "$RTLIB" -nt "$FLANG" ] || die "flang is newer than its runtime and modules: run flang/build.sh"
fi

TOP="${TMPDIR:-/tmp}"; TOP="${TOP%/}"
WORK="$(mktemp -d "$TOP/lapack.XXXXXX")" || exit 2
LOG="$WORK/log"
LAPACK="$LAPACK" python3 "$ROOT/flang/tests/lapack/run.py" "$WORK/run" "$@" > "$LOG" 2>&1
rc=$?
grep -E 'WORSE|DID NOT FINISH|print what the host|tests; failing|make failed|is at ' "$LOG"
if [ "$rc" -eq 0 ]; then
    case "$WORK" in "$TOP"/lapack.*) rm -rf "$WORK" ;; esac
else
    echo "run-lapack: failed, rc=$rc; outputs in $WORK ($LOG)" >&2
    [ "$rc" -eq 2 ] || rc=1
fi
exit "$rc"
