#!/bin/bash

# s32-cobc under AddressSanitizer, over a real corpus.
#
# WHY THIS EXISTS.  cobol/ISSUES.md 27: lit_label returned a pointer into a
# realloc'd table, and parse_inspect held one across a call that grew it.  The
# result was an empty %hi() in the emitted assembly, so an INSPECT REPLACING
# searched for a literal at address 0 and silently replaced nothing.
#
# Nothing caught it.  The 97-test harness passed, the majesty corpus stayed
# byte-identical, and CCVS caught it only by luck of which literal straddled
# the table's growth boundary -- shift the literal count and the same bug
# hides again.  It is also ALLOCATOR-DEPENDENT: macOS malloc returned an
# empty string, glibc returned the old bytes, so the same compiler built from
# the same source was wrong on one host and correct on the other.
#
# A test pinned to a literal count would therefore pass for the wrong reason
# the moment anything shifted it.  This checks the INVARIANT instead -- no
# held pointer into a growable table -- by building the compiler under ASan
# and compiling enough real source to exercise the growth.  Plain ASan is
# sufficient; no forced-realloc build is needed, because ASan poisons the old
# block on every realloc that moves.
#
# SELF-VALIDATING.  COBC_SRC points the build at any s32-cobc.c, so the check
# can be proved to work rather than assumed to:
#
#   git show 18fcb42c:cobol/src/s32-cobc.c > /tmp/buggy.c
#   COBC_SRC=/tmp/buggy.c ./run-cobc-asan.sh NC     # must FAIL
#   ./run-cobc-asan.sh NC                           # must PASS
#
# Usage: ./run-cobc-asan.sh [module ...]        default: NC
#   Modules are CCVS-85 directories (NC SQ RL IX ST SM IC RW IF).  The
#   cobol/tests fixed and free sources are always compiled as well.
#
# Env: CCVS85 (default ~/gnucobol-svn/tests/cobol85), COBC_SRC, CC, KEEP=1.
#
# Exit: 0 no sanitizer report, 1 at least one, 2 setup error.

set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"
CDIR="$ROOT/cobol"
CCVS="${CCVS85:-$HOME/gnucobol-svn/tests/cobol85}"
COBC_SRC="${COBC_SRC:-$CDIR/src/s32-cobc.c}"
CC="${CC:-cc}"
MODULES=${*:-NC}

RED=$'\033[0;31m'; GREEN=$'\033[0;32m'; YELLOW=$'\033[1;33m'; NC=$'\033[0m'

[ -f "$COBC_SRC" ] || { echo "no compiler source at $COBC_SRC" >&2; exit 2; }

W="$(mktemp -d "$CDIR/out/asan.XXXXXX")" || exit 2
[ -n "${KEEP:-}" ] || trap 'rm -rf "$W"' EXIT

echo "Building s32-cobc with -fsanitize=address from $COBC_SRC"
if ! "$CC" -std=c99 -O1 -g -w -fsanitize=address -I "$CDIR/src" \
        -o "$W/s32-cobc-asan" "$COBC_SRC" \
        "$CDIR/src/picture.c" "$CDIR/src/picture_scan.c" 2>"$W/build.err"; then
    echo "${RED}compiler build failed${NC}" >&2; sed -n '1,20p' "$W/build.err" >&2; exit 2
fi

# halt_on_error=0 so one report does not hide the rest of the corpus.
export ASAN_OPTIONS="halt_on_error=0:detect_leaks=0:log_path=$W/asan"

total=0; reports=0; skipped=0; failed=()

compile_one() {   # compile_one <format> <label> <source> [extra args...]
    fmt=$1; label=$2; src=$3; shift 3
    total=$((total + 1))
    rm -f "$W"/asan.*
    "$W/s32-cobc-asan" "$fmt" "$@" -o "$W/out.s" "$src" >"$W/cobc.log" 2>&1
    # ASan writes to log_path.<pid>; a compile error of its own is not our business
    if ls "$W"/asan.* >/dev/null 2>&1 || grep -q 'ERROR: AddressSanitizer' "$W/cobc.log"; then
        reports=$((reports + 1)); failed+=("$label")
        printf '  %-28s %sSANITIZER%s\n' "$label" "$RED" "$NC"
        { cat "$W"/asan.* 2>/dev/null; grep -A6 'ERROR: AddressSanitizer' "$W/cobc.log"; } \
            | grep -E 'ERROR: AddressSanitizer|SUMMARY|#[0-9]+ ' | head -8 | sed 's/^/      /'
    fi
}

for m in $MODULES; do
    d="$CCVS/$m"
    if [ ! -d "$d" ]; then
        printf '  %-28s %sSKIP%s (no %s -- set CCVS85)\n' "$m" "$YELLOW" "$NC" "$d"
        skipped=$((skipped + 1))
        continue
    fi
    echo "CCVS-85 $m:"
    for f in "$d"/lib/*.CBL "$d"/*.CBL; do
        [ -e "$f" ] || continue
        compile_one -fixed "$(basename "$f")" "$f" -I "$CCVS/copy"
    done
done

echo "cobol/tests:"
for f in "$CDIR"/tests/fixed/*.cbl; do
    [ -e "$f" ] || continue
    compile_one -fixed "fixed/$(basename "$f")" "$f" -I "$CDIR/tests/copy"
done
for f in "$CDIR"/tests/free/*.cbl; do
    [ -e "$f" ] || continue
    compile_one -free "free/$(basename "$f")" "$f" -I "$CDIR/tests/copy"
done

echo
echo "================================================="
if [ "$reports" -eq 0 ]; then
    # A clean run is only worth what the corpus was.  Measured on kagura
    # 2026-09-02: with the CCVS modules skipped, cobol/tests alone (83
    # compiles) does NOT reach lit_label's growth boundary, and this script
    # returns green on the very compiler it was written to catch --
    # COBC_SRC=<18fcb42c's s32-cobc.c> passes.  So say what was covered
    # rather than let the colour speak.
    if [ "$skipped" -gt 0 ]; then
        echo "${YELLOW}$total compiles, no sanitizer reports"
        echo "  -- but $skipped module(s) were skipped, and cobol/tests alone is"
        echo "     NOT enough to reach the growth boundary this check exists for."
        echo "     Set CCVS85 to a tree with the modules extracted before"
        echo "     reading this as a pass.${NC}"
        exit 0
    fi
    echo "${GREEN}$total compiles, no sanitizer reports${NC}"
    exit 0
fi
echo "${RED}$total compiles, $reports with sanitizer reports${NC}"
printf '  %s\n' "${failed[@]}"
exit 1
