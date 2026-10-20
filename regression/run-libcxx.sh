#!/bin/bash

# C++ programs on libc++ and libc++abi, on every engine.
#
# Each regression/libcxx/<name>.cpp is built for SLOW-32 against the libc++
# that scripts/build-libcxx.sh installs (outside the tree), at -O0 and -O2,
# and run on slow32, slow32-fast, slow32-dbt and qemu; every run must print
# exactly <name>.expected, which is what a host build of the same file
# prints -- or, for a test with a <name>.locales file, values pinned from
# CLDR: that test is linked with the named locales (runtime/locales.s32o,
# ahead of libc, and the rodata their collation wants).  Without that
# libc++ install the gate says SKIP and exits 0.
#
# The link: crt0, the program, libc++.a, libc++abi.a, libc, libs32 -- not
# runtime/libs32cxx.s32a, the runtime's own C++ ABI, which libc++abi
# replaces.
#
# Usage: ./run-libcxx.sh [name ...]
# Env:   LIBCXX (default ~/llvm-project/build-slow32-libcxx/install),
#        LLVM_BIN, SLOW32, SLOW32_FAST, SLOW32_DBT, QEMU_S32, TIMEOUT;
#        S32_AS, S32_LD (default the tree's slow32asm and s32-ld),
#        S32_RT (crt0, the C library archives and locales.s32o; default
#        runtime/) and S32_RT_INCLUDE (default runtime/include) -- in
#        slow32:toolchain, /opt/slow32/{bin,lib,include} and LIBCXX
#        /opt/slow32/libcxx test the image's own runtime and libc++.
# Exit:  0 all agree (or skipped), 1 a mismatch, 2 setup error.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
R="$(cd "$SCRIPT_DIR/.." && pwd)"
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
LIBCXX="${LIBCXX:-$HOME/llvm-project/build-slow32-libcxx/install}"
SLOW32="${SLOW32:-$R/tools/emulator/slow32}"
SLOW32_FAST="${SLOW32_FAST:-$R/tools/emulator/slow32-fast}"
SLOW32_DBT="${SLOW32_DBT:-$R/tools/dbt/slow32-dbt}"
QEMU_S32="${QEMU_S32:-$HOME/qemu/build/qemu-system-slow32}"
TIMEOUT="${TIMEOUT:-120}"
S32_AS="${S32_AS:-$R/tools/assembler/slow32asm}"
S32_LD="${S32_LD:-$R/tools/linker/s32-ld}"
S32_RT="${S32_RT:-$R/runtime}"
S32_RT_INCLUDE="${S32_RT_INCLUDE:-$R/runtime/include}"
TESTS="$SCRIPT_DIR/libcxx"

if [ ! -f "$LIBCXX/lib/libc++.a" ] || [ ! -f "$LIBCXX/lib/libc++abi.a" ]; then
    echo "libcxx: SKIP (no libc++ at $LIBCXX; scripts/build-libcxx.sh builds it)"
    exit 0
fi
[ -x "$LLVM_BIN/clang++" ] || { echo "ERROR: no clang++ in $LLVM_BIN" >&2; exit 2; }

# Engine banners and statistics, as the other differentials strip them
normalize() {
    sed -e 's/\r$//' -e '/^Starting execution/d' -e '/^MMIO enabled/d' \
        -e '/^HALT at/d' -e '/^Program halted/d' -e '/^Exit code:/d' \
        -e '/^Instructions executed/d' -e '/^Simulated cycles:/d' \
        -e '/^Wall time:/d' -e '/^Performance:/d' -e '/^Cycles:/d' \
        -e '/instructions\/second/d' \
        -e '/^slow32: native intrinsics detected:/d' -e '/^slow32: math intercepts:/d' \
        -e '/^  mem[a-z]*:  *0x[0-9a-f]*$/d' -e '/^  strlen:  *0x[0-9a-f]*$/d' |
    awk '{ l[NR] = $0; if ($0 ~ /[^[:space:]]/) last = NR } END { for (i = 1; i <= last; i++) print l[i] }'
}

ENGINES=()
for spec in "slow32|$SLOW32" "slow32-fast|$SLOW32_FAST" "slow32-dbt|$SLOW32_DBT" "qemu|$QEMU_S32"; do
    [ -x "${spec#*|}" ] && ENGINES+=("$spec") || echo "  SKIPPED ENGINE: ${spec%%|*} (${spec#*|})"
done

W="$(mktemp -d "${TMPDIR:-/tmp}/libcxx.XXXXXX")"
trap 'rm -rf "${W:?}"' EXIT
# the clang driver is not used, but slow32asm is called by name if it is
export PATH="$R/tools/assembler:$R/tools/linker:$R/tools/utilities:$PATH"

if [ $# -gt 0 ]; then NAMES=("$@")
else NAMES=(); for f in "$TESTS"/*.cpp; do NAMES+=("$(basename "$f" .cpp)"); done; fi

PASS=0
FAIL=0
for b in "${NAMES[@]}"; do
    [ -f "$TESTS/$b.cpp" ] && [ -f "$TESTS/$b.expected" ] ||
        { echo "ERROR: $b needs $b.cpp and $b.expected" >&2; exit 2; }
    extra=()
    [ -f "$TESTS/$b.locales" ] && extra=("$S32_RT/locales.s32o" --rodata-size 2M)
    for opt in -O0 -O2; do
        o="$W/$b$opt"
        if ! "$LLVM_BIN/clang++" -target slow32-unknown-none -std=c++20 $opt -S -emit-llvm \
                -nostdinc++ -isystem "$LIBCXX/include/c++/v1" -isystem "$S32_RT_INCLUDE" \
                -fexceptions -frtti -funwind-tables "$TESTS/$b.cpp" -o "$o.ll" 2>"$o.err" ||
           ! "$LLVM_BIN/llc" -mtriple=slow32-unknown-none "$o.ll" -o "$o.s" 2>>"$o.err" ||
           ! "$S32_AS" "$o.s" "$o.s32o" >/dev/null 2>>"$o.err" ||
           ! "$S32_LD" -o "$o.s32x" --mmio 64K "$S32_RT/crt0.s32o" "$o.s32o" ${extra[@]+"${extra[@]}"} \
                "$LIBCXX/lib/libc++.a" "$LIBCXX/lib/libc++abi.a" \
                "$S32_RT/libc_mmio.s32a" "$S32_RT/libs32.s32a" >/dev/null 2>>"$o.err"; then
            printf "  %-30s FAIL (build)\n" "$b $opt"; head -3 "$o.err"; FAIL=$((FAIL+1)); continue
        fi
        for spec in "${ENGINES[@]}"; do
            name="${spec%%|*}"; path="${spec#*|}"; rc=0
            # each run in a fresh directory of its own: a program may make
            # files (files.cpp does), and a run must not see the last one's
            run_dir="$(mktemp -d "$W/run.XXXXXX")"
            if [ "$name" = qemu ]; then
                (cd "$run_dir" && timeout "$TIMEOUT" "$path" -machine slow32-tcg -nographic -monitor none \
                    -kernel "$o.s32x" </dev/null >"$o.raw" 2>&1) || rc=$?
            else
                (cd "$run_dir" && timeout "$TIMEOUT" "$path" "$o.s32x" </dev/null >"$o.raw" 2>&1) || rc=$?
            fi
            normalize < "$o.raw" > "$o.got"
            if [ "$rc" -ne 0 ]; then
                printf "  %-30s FAIL (exit %s)\n" "$b $opt $name" "$rc"; FAIL=$((FAIL+1))
            elif ! cmp -s "$TESTS/$b.expected" "$o.got"; then
                printf "  %-30s FAIL (output differs)\n" "$b $opt $name"
                diff "$TESTS/$b.expected" "$o.got" | head -6; FAIL=$((FAIL+1))
            else
                printf "  %-30s PASS\n" "$b $opt $name"; PASS=$((PASS+1))
            fi
        done
    done
done
echo "libcxx: $PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ]
