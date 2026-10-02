#!/usr/bin/env bash
# Differential harness: the HOST assembler vs the SELF-HOSTED one.
#
# The tree has two independent assemblers for one ISA.  That is
# deliberate -- the host one (tools/assembler/slow32asm.c, ~3.7k lines)
# has to eat everything LLVM emits (.cfi_*, .loc/.file/.stabs, .weak,
# .addrsig, .eh_frame) and may use stdio, getopt and a hash table; the
# self-hosted one (selfhost/src/tools/s32-as.c, ~1.9k lines) only has to
# eat stage08 cc's output, and must compile in the frozen bootstrap
# dialect and run on SLOW-32.  Merging them would drag DWARF parsing
# into the bootstrap path for no benefit.
#
# But they MUST agree on the overlap -- instruction encoding, section
# layout, symbol values and relocations -- and until this script,
# nothing checked that.  Every other harness picks one assembler or the
# other.
#
# Comparison is SEMANTIC, not byte-for-byte, because the two order the
# symbol table differently (host: definition order; selfhost: first-
# reference order).  That difference is benign -- the linker resolves by
# name -- so this normalises it away rather than failing on it, and
# relocations are matched by symbol NAME since their indices move with
# the ordering.
#
# Usage: ./run-as-differential.sh [file.s ...]
#        (no args = the built-in corpus)
set -uo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"
HOST_AS="$ROOT/tools/assembler/slow32asm"
SELF_AS="$ROOT/selfhost/stage08/s32-as.s32x"
EMU="${SELFHOST_EMU:-$ROOT/selfhost/stage00/s32-emu}"
DUMP="$ROOT/tools/utilities/slow32dump"
W="$(mktemp -d)"
trap 'rm -rf "$W"' EXIT

for t in "$HOST_AS" "$DUMP" "$EMU"; do
    [ -x "$t" ] || { echo "missing tool: $t" >&2; exit 1; }
done
[ -f "$SELF_AS" ] || { echo "missing $SELF_AS (run selfhost/stage08/build-tools.sh)" >&2; exit 1; }

# Canonical form: everything that must match, nothing that need not.
# Section OFFSETS are excluded (pure file layout); symbol and
# relocation lines are sorted so ordering cannot cause a false failure.
canon() {
    local obj="$1" out="$2"
    {
        echo "--sections--"
        "$DUMP" "$obj" 2>/dev/null | awk '
            /^Idx Name/{on=1;next} /^---/{next}
            on && NF>=7 {print $2, $3, $4, $6, $7, $8}
            on && NF==0{exit}'
        echo "--symbols--"
        "$DUMP" "$obj" 2>/dev/null | awk '
            /^Num Value/{on=1;next} /^---/{next}
            on && NF>=7 {print $NF, $2, $3, $4, $5, $6}
            on && NF==0{exit}' | sort
        echo "--relocs--"
        "$DUMP" "$obj" 2>/dev/null | awk '
            /^Offset +Type/{on=1;next} /^---/{next}
            on && NF>=5 {print $1, $2, $4, $5}
            on && NF==0{on=0}' | sort
        echo "--contents--"
        "$DUMP" "$obj" 2>/dev/null | sed -n '/^Contents of section/,$p'
    } > "$out"
}

corpus=("$@")
if [ ${#corpus[@]} -eq 0 ]; then
    while IFS= read -r f; do corpus+=("$f"); done < <(
        ls "$ROOT"/selfhost/stage08/*.s 2>/dev/null
        ls "$ROOT"/runtime/*.s 2>/dev/null
    )
    # The interesting corpus is COMPILER-GENERATED assembly: that is
    # what both assemblers actually have to agree on.  Hand-written .s
    # in runtime/ uses a base+offset store syntax (`stw sp+0, lr`) that
    # only the host assembler accepts, so those skip.
    CLANG="${CLANG:-$HOME/llvm-project/build/bin/clang}"
    LLC="${LLC:-$HOME/llvm-project/build/bin/llc}"
    if [ -x "$CLANG" ] && [ -x "$LLC" ]; then
        n=0
        for c in "$ROOT"/regression/tests/*/test.c; do
            [ -f "$c" ] || continue
            [ "$n" -ge 25 ] && break
            tag="cl_$(basename "$(dirname "$c")")"
            if "$CLANG" -target slow32-unknown-none -S -emit-llvm -O2 \
                  -I"$ROOT/runtime/include" "$c" -o "$W/$tag.ll" 2>/dev/null &&
               "$LLC" -mtriple=slow32-unknown-none -O2 "$W/$tag.ll" -o "$W/$tag.s" 2>/dev/null; then
                corpus+=("$W/$tag.s"); n=$((n+1))
            fi
        done
    fi
    # A generated CAPACITY case: more labels than any fixed ceiling the
    # assembler used to have.  This is not hypothetical -- removing
    # MAX_LBL left g_lbl_to_sym[32768] indexed by an unbounded label
    # count, and 40k labels overflowed it (found by ASan, fixed by
    # growing it with the label tables).
    {
        echo ".text"
        echo ".globl big"
        echo "big:"
        i=0
        while [ $i -lt 40000 ]; do
            echo ".L$i:"
            echo "    addi r1, r1, 1"
            i=$((i+1))
        done
        echo "    jalr r0, r31, 0"
    } > "$W/gen_manylabels.s"
    corpus+=("$W/gen_manylabels.s")

    # A generated LITERAL case: a 32-bit word in every spelling -- signed
    # and unsigned decimal, hexadecimal in both cases, octal, at and
    # around 2^31 and 2^32-1 -- as data and as immediates.  Compiler
    # output has almost none of these (stage08 cc writes decimal, and
    # one test in ten has a word past 2^31), and the self-hosted
    # assembler reads its literals with a reader of its own (as_word,
    # selfhost ISSUES-75), which the corpus above exercised through one
    # path only: three faults put into it by hand all passed.
    {
        echo ".data"
        echo ".globl lits"
        echo "lits:"
        echo "    .word 0, 1, -1, 9, -9, 2147483647, 2147483648, 2147483649, 4294967295"
        echo "    .word -2147483647, -2147483648, 4294967294, 1000000000, -1000000000"
        echo "    .word 0x0, 0x1, 0x7fffffff, 0x80000000, 0x80000001, 0xFFFFFFFF, 0xdeadBEEF, 0XabcDEF01"
        echo "    .word 00, 07, 017, 0777, 037777777777"
        echo "    .word -0x10, -017, +5, +0x20"
        echo "    .byte 0, 1, 127, 128, 255, -1, -128, 0x7f, 0xff, 0x80, 010, 0377"
        echo ".text"
        echo ".globl litcode"
        echo "litcode:"
        echo "    addi r1, r0, -16"
        echo "    addi r1, r0, 16"
        echo "    addi r1, r0, 0x10"
        echo "    addi r1, r0, -0x10"
        echo "    addi r1, r0, 017"
        echo "    addi r1, r0, 2047"
        echo "    addi r1, r0, -2048"
        echo "    lui r2, 524288"
        echo "    lui r2, 0x80000"
        echo "    lui r2, 0xFFFFF"
        echo "    ori r3, r3, 0xfff"
        echo "    ldw r4, r29, -8"
        echo "    stw r29, r4, 0x7fc"
        echo "    jalr r0, r31, 0"
    } > "$W/gen_literals.s"
    corpus+=("$W/gen_literals.s")

    # ...and stage08 cc's own output, the other producer.
    if [ -f "$ROOT/selfhost/stage08/cc.s32x" ]; then
        n=0
        for c in "$ROOT"/selfhost/stage08/tests/*.c; do
            [ -f "$c" ] || continue
            [ "$n" -ge 10 ] && break
            tag="s8_$(basename "$c" .c)"
            if timeout 300 "$EMU" "$ROOT/selfhost/stage08/cc.s32x" "$c" "$W/$tag.s" >/dev/null 2>&1 &&
               [ -s "$W/$tag.s" ]; then
                corpus+=("$W/$tag.s"); n=$((n+1))
            fi
        done
    fi
fi

pass=0; fail=0; skip=0
for f in "${corpus[@]}"; do
    [ -f "$f" ] || continue
    b="$(basename "$f")"
    # a file this script wrote for both assemblers is one both must take:
    # for those a rejection is a failure, not a skip
    must=0; case "$b" in gen_*) must=1;; esac
    if ! "$HOST_AS" "$f" "$W/h.o" >/dev/null 2>&1 || [ ! -s "$W/h.o" ]; then
        if [ $must = 1 ]; then printf "  %-28s FAIL (host assembler rejects)\n" "$b"; fail=$((fail+1)); continue; fi
        printf "  %-28s SKIP (host assembler rejects)\n" "$b"; skip=$((skip+1)); continue
    fi
    rm -f "$W/s.o"
    if ! timeout 600 "$EMU" "$SELF_AS" "$f" "$W/s.o" >/dev/null 2>&1 || [ ! -s "$W/s.o" ]; then
        if [ $must = 1 ]; then printf "  %-28s FAIL (selfhost assembler rejects)\n" "$b"; fail=$((fail+1)); continue; fi
        printf "  %-28s SKIP (selfhost assembler rejects)\n" "$b"; skip=$((skip+1)); continue
    fi
    canon "$W/h.o" "$W/h.txt"
    canon "$W/s.o" "$W/s.txt"
    if diff -q "$W/h.txt" "$W/s.txt" >/dev/null; then
        printf "  %-28s AGREE\n" "$b"; pass=$((pass+1))
    else
        printf "  %-28s DIFFER\n" "$b"; fail=$((fail+1))
        diff "$W/h.txt" "$W/s.txt" | head -20 | sed 's/^/      /'
    fi
done

echo ""
echo "assembler differential: $pass agree, $fail differ, $skip skipped"
[ "$fail" -eq 0 ]
