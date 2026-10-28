#!/bin/bash

# The LLVM machine verifier over every C library clang builds for SLOW-32.
#
# WHY THIS EXISTS.  On 2026-10-27 a random-program differential found a
# wrong-code bug in the SLOW32 backend's GPRPair spill code: the reload left
# the low word of a double in the wrong register.  `llc -verify-machineinstrs`
# rejects that code outright, and had all along -- nothing ever ran it.  Run
# over the runtime, libcob, libf77 and SQLite, it then found two more backend
# defects the moment it was turned on (an indirect call's r31, the optsize
# select's live-ins).  Output-comparing gates see a backend defect only when a
# program's answer happens to change; the verifier sees it in every function
# llc builds.  Run this after any change under llvm/lib/Target/SLOW32.
#
# Each source is compiled to IR the way its own build does (the runtime and
# libf77 at -O2, SQLite at -Os, libcob at -O2), then through llc with the
# verifier.  libc++ is not here: rebuild it with
#   BUILD=<dir> CXXFLAGS="-mllvm -verify-machineinstrs" \
#     CFLAGS="-mllvm -verify-machineinstrs" scripts/build-libcxx.sh
# (2026-10-27: clean).
#
# SELF-VALIDATING: LLC=<an llc from before 4ae5cc47e157> fails here (the
# runtime's indirect calls), so a green run is a verifier that ran.
#
# Env: LLVM_BIN (default ~/llvm-project/build/bin), LLC (default
# $LLVM_BIN/llc), JOBS (default: cores).  ~7 seconds on an M5.

set -u
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
CLANG="$LLVM_BIN/clang"
LLC="${LLC:-$LLVM_BIN/llc}"
JOBS="${JOBS:-$(sysctl -n hw.ncpu 2>/dev/null || nproc)}"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/llvm-verify.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT

RT="$ROOT/runtime"
SQ="$ROOT/sqlite"
SQOPTS="$(sed -n '/^OPTS="/,/"$/p' "$SQ/build.sh" | tr '\n' ' ' | sed 's/^OPTS="//; s/" *$//')"

# one line per unit: source|clang flags
{
    for f in "$RT"/*.c; do
        case "$(basename "$f")" in
            dtoa.c)    echo "$f|-O2 -I$RT/include -funwind-tables -DIEEE_8087 -DUSE_LOCALE -DNO_LOCALE_CACHE" ;;
            locales.c) echo "$f|-O2 -I$RT/include -funwind-tables -I$RT/utf/include" ;;
            *)         echo "$f|-O2 -I$RT/include -funwind-tables" ;;
        esac
    done
    for f in "$ROOT"/cobol/libcob/*.c; do
        echo "$f|-O2 -I$RT/include -I$ROOT/cobol/libcob -I$RT/utf/include -I$SQ"
    done
    for f in "$ROOT"/fortran/runtime/*.c; do
        echo "$f|-O2 -I$RT/include"
    done
    for f in sqlite3.c slow32_vfs.c main.c; do
        echo "$SQ/$f|-Os -I$RT/include -I$SQ $SQOPTS"
    done
    echo "$SQ/shell_slow32.c|-Os -I$RT/include -I$SQ -include $SQ/compat/shell_slow32.h -I$SQ/compat $SQOPTS -DSHELL_LIBC_HAS_POSIX_FILES -DSHELL_LIBC_HAS_GETPID"
} > "$WORK/units"

one() {   # one N: the Nth unit (by number -- SQLite's lines are too long for xargs -I)
    local u; u="$(sed -n "${1}p" "$WORK/units")"
    local src="${u%%|*}" flags="${u#*|}"
    local tag; tag="$(echo "${src#$ROOT/}" | tr / _)"
    # shellcheck disable=SC2086
    if ! "$CLANG" -target slow32-unknown-none -S -emit-llvm -w $flags "$src" \
            -o "$WORK/$tag.ll" 2> "$WORK/$tag.cerr"; then
        echo "CLANG ${src#$ROOT/}: $(grep -m1 error "$WORK/$tag.cerr")"
        return
    fi
    # in a subshell whose own stderr goes nowhere: llc aborts on a verifier
    # failure, and bash would report the signal
    if ( exec "$LLC" -mtriple=slow32-unknown-none -verify-machineinstrs "$WORK/$tag.ll" \
            -o /dev/null 2> "$WORK/$tag.verr" ) 2>/dev/null; then
        echo "OK ${src#$ROOT/}"
    else
        echo "BAD ${src#$ROOT/}: $(grep -c 'Bad machine code' "$WORK/$tag.verr") error(s), first: $(grep -m1 -A2 'Bad machine code' "$WORK/$tag.verr" | tr '\n' ' ')"
    fi
    rm -f "$WORK/$tag.ll"
}
export -f one
export ROOT CLANG LLC WORK

seq 1 "$(wc -l < "$WORK/units")" | xargs -P "$JOBS" -n1 bash -c 'one "$1"' _ 2>/dev/null | sort > "$WORK/results"
grep -v '^OK ' "$WORK/results"
ok=$(grep -c '^OK ' "$WORK/results"); bad=$(grep -c '^BAD ' "$WORK/results"); cl=$(grep -c '^CLANG ' "$WORK/results")
echo "llvm verify: $ok verified, $bad rejected, $cl did not compile (of $(wc -l < "$WORK/units"))"
[ "$bad" -eq 0 ] && [ "$cl" -eq 0 ]
