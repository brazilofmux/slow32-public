#!/bin/bash
# The COBOL runtime built by the other C compiler.
#
# cobol/cctool.sh has two ways to turn libcob.c into an object: the LLVM
# backend, and -- on a machine with the tree and no LLVM build -- the
# self-hosted stage08 cc run under the emulator.  Every other gate here
# runs the first.  The second was found not to build at all on
# 2026-10-02, twice over: libcob.c had grown a file-scope asm, which that
# compiler does not have, and esql.c a local variable named for a typedef
# of its own file, which that compiler misread (selfhost ISSUES-78) -- the
# one a day old, the other two.  Nothing had looked.
#
# So: build libcob and the SQL runtime with the self-hosted compiler into
# a directory of their own (the runtime in use is not touched), and run
# the suite's programs against that runtime -- every test under fixed/,
# free/ and 2002/ compiled, run in a fresh copy of tests/data and compared
# with its .expected, as run-tests.sh does, less the oracle and the
# -fno-hot-arith pass (they are the compiler's checks, and the compiler
# is the same one).  Programs with .c beside them are compiled by the
# self-hosted cc too.
#
#   tests/selfhost-libcob.sh            the kit's compiler (S32_KIT, ~/s32x)
#   S32_KIT=dir tests/selfhost-libcob.sh
#
# The compiler is the kit's cc.s32x, as on the machines this is for; a
# kit older than a repair this needs fails here, and that is the answer
# such a machine would get.
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
CDIR="$(cd "$HERE/.." && pwd)"
ROOT="$(cd "$CDIR/.." && pwd)"
EMU="${EMU:-$ROOT/tools/emulator/slow32}"
export LLVM_BIN=/nonexistent        # cctool.sh: no clang, so the self-hosted cc
export S32_KIT="${S32_KIT:-$HOME/s32x}"
[ -f "$S32_KIT/cc.s32x" ] || { echo "selfhost-libcob: no $S32_KIT/cc.s32x (set S32_KIT)"; exit 1; }

mkdir -p "$CDIR/out"
W="$(mktemp -d "$CDIR/out/selfhost.XXXXXX")"
trap 'rm -rf "$W"' EXIT

if ! LIBCOB_OUT="$W" "$CDIR/libcob/build.sh" > "$W/build.log" 2>&1; then
    echo "selfhost-libcob: the runtime does not build with $S32_KIT/cc.s32x"
    grep -a -m3 -A2 "error" "$W/build.log"
    exit 1
fi
export S32_LIBCOB="$W/libcob.s32o" S32_ESQL="$W/esql.s32o"

pass=0; fail=0
for fmt in fixed free 2002; do
    for src in "$HERE/$fmt"/*.cbl; do
        name="$(basename "$src" .cbl)"
        exp="${src%.cbl}.expected"
        [ -f "$exp" ] || continue
        flag="-$fmt"; std=""
        [ "$fmt" = 2002 ] && { flag="-free"; std="-std=2002"; }
        case "$name" in mf-*) std="$std -dialect=mf" ;; esac
        extra=()
        if [ -f "${src%.cbl}.link" ]; then for e in $(cat "${src%.cbl}.link"); do extra+=("$HERE/$e"); done; fi
        if ! "$CDIR/compile.sh" $flag $std -I "$HERE/copy" "$src" "${extra[@]+"${extra[@]}"}" -o "$W/p.s32x" > "$W/p.log" 2>&1; then
            echo "  FAIL $fmt/$name (does not build)"; fail=$((fail+1)); continue
        fi
        rm -rf "$W/run"; mkdir -p "$W/run/tmp"
        [ -d "$HERE/data" ] && cp -R "$HERE/data/." "$W/run/"
        keys=/dev/null; [ -f "${src%.cbl}.keys" ] && keys="${src%.cbl}.keys"
        args=""; [ -f "${src%.cbl}.args" ] && args="$(cat "${src%.cbl}.args")"
        penv=(); if [ -f "${src%.cbl}.env" ]; then while IFS= read -r l || [ -n "$l" ]; do [ -n "$l" ] && penv+=("$l"); done < "${src%.cbl}.env"; fi
        (cd "$W/run" && env ${penv[@]+"${penv[@]}"} timeout 120 "$EMU" "$W/p.s32x" $args 2>/dev/null < "$keys") | awk '
            /^Starting execution/ { capture = 1; held = 0; next }
            /^HALT at|^Program halted|^Exit code/ { if (held && prev != "") print prev; capture = 0; held = 0 }
            capture { if (held) print prev; prev = $0; held = 1 }
            END { if (held) print prev }' > "$W/p.out"
        if diff -q "$W/p.out" "$exp" > /dev/null; then pass=$((pass+1))
        else echo "  FAIL $fmt/$name"; diff "$exp" "$W/p.out" | head -4 | sed 's/^/      /'; fail=$((fail+1)); fi
    done
done
echo "selfhost-libcob: $pass programs agree, $fail differ (runtime built by $S32_KIT/cc.s32x)"
[ "$fail" = 0 ]
