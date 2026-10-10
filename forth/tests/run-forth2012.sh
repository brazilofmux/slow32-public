#!/bin/bash
# Run the Forth 2012 test suite (John Hayes' core tests and the gforth
# additions) against the SLOW-32 kernel + prelude.  The suite is GPL and is
# not vendored: it is read from a gforth installation (brew's, or
# FORTH2012_DIR).  Skipped, exit 0, when none is present.
#
# Each file is fed as: prelude, float stubs, ttester.fs, the shim below,
# then the test file(s).  The shim defines what the gforth copies of the
# files assume of their host: { } for T{ }T, MAX-N/MAX-U (coremore reaches
# them through gforth's environment-wordlist), STR= (string.fs), the
# D<> D> D>= D<= comparisons dbltest.fs uses, and four floating-point words
# ttester's no-FP-stack branch still names (it assumes floats exist).
# `require` lines are dropped: the files they name are concatenated instead.
#
# Known, accepted residue (not failures of ours):
#   search.fs: `root` is gforth's ROOT wordlist; 5e is a float.
set -u
cd "$(dirname "$0")/.."
EMU="${EMU:-../tools/emulator/slow32-fast}"
KERNEL="${S32_FORTH_KERNEL:-kernel.s32x}"
PRELUDE="${S32_FORTH_PRELUDE:-prelude.fth}"
DIR="${FORTH2012_DIR:-}"
if [ -z "$DIR" ]; then
    for d in /opt/homebrew/Cellar/gforth/*/share/gforth/*/test /usr/local/share/gforth/*/test /usr/share/gforth/*/test; do
        [ -f "$d/coretest.fs" ] && DIR="$d" && break
    done
fi
if [ -z "$DIR" ] || [ ! -f "$DIR/ttester.fs" ]; then
    echo "run-forth2012: no gforth test directory found (set FORTH2012_DIR); skipping"
    exit 0
fi
SHIM=': 0E 0 ; : FDROP DROP ; : F@ @ ; : FCONF= = ;
'
POST=': { T{ ;
: } }T ;
S" MAX-N" ENVIRONMENT? DROP CONSTANT MAX-N  S" MAX-U" ENVIRONMENT? DROP CONSTANT MAX-U
: STR= COMPARE 0= ;
: D<> D= 0= ;  : D> 2SWAP D< ;  : D>= D< 0= ;  : D<= 2SWAP D< 0= ;
HEX
'
# each entry: files to concatenate; later files may need the earlier ones
RUNS=("coretest" "coreext" "coretest dbltest" "coremore" "string" "search" "coretest postpone" "deferred")
ACCEPT_WRONG='root forth'           # grep -E pattern of accepted INCORRECT/WRONG lines
ACCEPT_UNDEF='^(5E|ROOT) \?$'       # grep -E pattern of accepted "name ?" lines
fail=0
for run in "${RUNS[@]}"; do
    out=$( { cat "$PRELUDE"; printf '%s' "$SHIM"; cat "$DIR/ttester.fs"; printf '%s' "$POST"
             for f in $run; do grep -v -i '^require \|^environment-wordlist >order' "$DIR/$f.fs"; done
             printf '\nDECIMAL CR .( ERRORS-END) CR\nBYE\n'; } | timeout 60 "$EMU" "$KERNEL" 2>&1 | sed 's/ok> //g')
    wrong=$(printf '%s\n' "$out" | grep 'INCORRECT RESULT\|WRONG NUMBER' | grep -Evc "$ACCEPT_WRONG")
    undef=$(printf '%s\n' "$out" | grep ' ?$' | grep -Evc "$ACCEPT_UNDEF")
    faults=$(printf '%s\n' "$out" | grep -c 'fault\|^Error:')
    ended=$(printf '%s\n' "$out" | grep -c 'ERRORS-END')
    status=ok
    if [ "$wrong" != 0 ] || [ "$undef" != 0 ] || [ "$faults" != 0 ] || [ "$ended" != 1 ]; then status=FAIL; fail=1; fi
    printf '%-20s %-4s wrong=%s undefined=%s faults=%s ended=%s\n' "${run// /+}" "$status" "$wrong" "$undef" "$faults" "$ended"
    if [ "$status" = FAIL ]; then
        printf '%s\n' "$out" | grep 'INCORRECT RESULT\|WRONG NUMBER\| ?$\|fault\|^Error:' | grep -Ev "$ACCEPT_WRONG|$ACCEPT_UNDEF" | head -8 | sed 's/^/    /'
    fi
done
exit $fail
