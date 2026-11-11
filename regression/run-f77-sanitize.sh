#!/usr/bin/env bash
# fortran/'s f77 built with the address and undefined-behaviour sanitizers,
# over every Fortran source at hand: f77's own tests, FCVS, and gfortran's
# corpus where they are (~/refs; neither is needed).  Fails if a sanitizer
# speaks or the compiler dies of a signal.  After a change to fortran/src or
# the HIR backend's files there.
#
# The output-comparing gates see a compiler's bad read only when it changes an
# answer.  This sees it where it happens, and most of all on the paths no test
# compares: what f77 does after it has refused a statement.  Its first run
# (2026-11-10) found the instruction selector reading before its table on
# ordinary Fortran 77 (a dead ADDI's operand of -1), a DATA substring out of
# range written past the compiler's own image, message buffers overrun by long
# names, constants folded by conversions C leaves undefined (one program on an
# ARM host, another on x86), an INTEGER literal that wrapped, and the backend
# handed the remains of refused statements.
#
# Signed overflow and shifts are left out: the HIR optimizer's hashes and folds
# wrap on purpose, as SLOW-32's own arithmetic does.  ~1 min.
set -u
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"; ROOT="$(dirname "$HERE")"
CC="${CC:-cc}"
W="$(mktemp -d "${TMPDIR:-/tmp}/f77-san.XXXXXX")" || exit 2
trap 'rm -rf "${W:?}"' EXIT
"$CC" -O1 -g -w -fsanitize=address,undefined -fno-sanitize=signed-integer-overflow,shift \
      -fno-sanitize-recover=undefined -fno-omit-frame-pointer \
      -o "$W/f77san" "$ROOT/fortran/src/f77.c" -lm || { echo "run-f77-sanitize: no sanitizer build with $CC" >&2; exit 2; }

{
    ls "$ROOT"/fortran/tests/f77/*.f "$ROOT"/fortran/tests/f77/*.f90 "$ROOT"/fortran/tests/multi/*/*.f* \
       "$ROOT"/fortran/tests/stop/*.f "$ROOT"/fortran/tests/torture.f 2>/dev/null
    ls "$HOME"/refs/fcvs/fcvs21/*.FOR 2>/dev/null
    [ -d "$HOME/refs/llvm-test-suite/Fortran/gfortran/regression" ] &&
        find "$HOME/refs/llvm-test-suite/Fortran/gfortran/regression" \
             \( -name '*.f' -o -name '*.f90' -o -name '*.f95' -o -name '*.f03' -o -name '*.f08' -o -name '*.F90' -o -name '*.for' \)
} > "$W/files"

cat > "$W/one.sh" <<'ONE'
#!/bin/bash
# one.sh W FILE: the sanitizer build on FILE, in a directory of its own (a module's .mod lands there)
d="$(mktemp -d "$1/w.XXXXXX")"; cd "$d" || exit
UBSAN_OPTIONS=print_stacktrace=1 ASAN_OPTIONS=detect_leaks=0 perl -e 'alarm 120; exec @ARGV' "$1/f77san" "$2" out.s > err 2>&1; rc=$?
if grep -q "AddressSanitizer\|runtime error:" err; then
    echo "SANITIZER $2: $(grep -m1 "runtime error:\|ERROR: AddressSanitizer" err | cut -c1-160)"
elif [ $rc -gt 1 ]; then echo "DIED($rc) $2: $(head -1 err | cut -c1-120)"; fi
cd "$1" && rm -rf "$d"
ONE
chmod +x "$W/one.sh"
tr '\n' '\0' < "$W/files" | xargs -0 -P "${JOBS:-6}" -n 1 "$W/one.sh" "$W" > "$W/report" 2>&1
n="$(wc -l < "$W/files" | tr -d ' ')"; bad="$(wc -l < "$W/report" | tr -d ' ')"
head -20 "$W/report"
echo "f77 sanitize: $n sources, $bad with a report"
[ "$bad" = 0 ]
