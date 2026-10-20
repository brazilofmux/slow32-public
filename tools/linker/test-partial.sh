#!/bin/bash
# s32-ld -r: a partial link that keeps a module's internals to itself.
#
# Two modules, each of two files, and a main program all define a function
# named helper and a counter named tally; in the modules they are hidden.
# Each module is partially linked with --localize-hidden and its entry
# point renamed (--redefine-sym), and the three link into one program in
# which every caller still reaches its own helper and its own tally --
# what TinyMUX's engine and modules need to share one static link.  Within
# a module the two files share one tally, a weak hidden definition in each
# (an inline function's static, in C++), unified by the partial link before
# it is made local; the modules' tallies stay apart.  And static functions
# of the same name in two files of a module (name clashes among locals) must
# keep their own bodies: each takes its static's address, so the partial
# link sees two local symbols of one name.
#
# Failure modes this was written against: hidden symbols left global (the
# final link stops at a duplicate helper, or a module calls main's); a weak
# tally unified across modules (the counts run together); a local name
# collision in the partial-link output (one file's static called from the
# other); a relocation pointed at the wrong symbol after renumbering.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
LLVM="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
AS="$ROOT/tools/assembler/slow32asm"
LD="$ROOT/tools/linker/s32-ld"
RT="$ROOT/runtime"
EMU="${EMU:-$ROOT/tools/emulator/slow32-fast}"
W="$(mktemp -d "${TMPDIR:-/tmp}/s32-partial.XXXXXX")"
trap 'rm -rf "${W:?}"' EXIT

cc() {  # cc NAME: $W/NAME.c -> $W/NAME.s32o
    "$LLVM/clang" --target=slow32-unknown-none -O2 -I"$RT/include" -S "$W/$1.c" -o "$W/$1.s"
    "$AS" "$W/$1.s" "$W/$1.s32o" >/dev/null
}

for m in a b; do
    n=$([ $m = a ] && echo 10 || echo 20)
    cat > "$W/${m}1.c" <<EOF
#define HID __attribute__((visibility("hidden")))
HID __attribute__((weak)) int tally;
HID int helper(int x) { tally++; return x + $n; }
static int scale(int x) { return x * 3; }
static int (*volatile pick)(int) = scale;  /* a local symbol, by address */
int ${m}_second(int x);
int entry(int x) { return ${m}_second(helper(pick(x))); }
EOF
    cat > "$W/${m}2.c" <<EOF
#define HID __attribute__((visibility("hidden")))
HID __attribute__((weak)) int tally;
HID int helper(int x);
static int scale(int x) { return x * 5; }
static int (*volatile pick)(int) = scale;  /* a local symbol, by address */
HID int ${m}_second(int x) { tally++; return pick(x) + tally * 1000; }
int ${m}_count(void) { return tally; }
EOF
    cc "${m}1"; cc "${m}2"
    "$LD" -r --localize-hidden \
        --redefine-sym entry=${m}_entry \
        -o "$W/mod_$m.s32o" "$W/${m}1.s32o" "$W/${m}2.s32o"
done

cat > "$W/main.c" <<'EOF'
#include <stdio.h>
int tally = 500;
int helper(int x) { return x - 1; }
int a_entry(int), b_entry(int), a_count(void), b_count(void);
int main(void) {
    int ra = a_entry(1), rb = b_entry(1), ra2 = a_entry(2);
    printf("a=%d b=%d a2=%d acount=%d bcount=%d helper=%d tally=%d\n",
           ra, rb, ra2, a_count(), b_count(), helper(7), tally);
    return 0;
}
EOF
cc main
"$LD" -o "$W/prog.s32x" "$RT/crt0.s32o" "$W/main.s32o" "$W/mod_a.s32o" "$W/mod_b.s32o" \
    "$RT/libc_debug.s32a" "$RT/libs32.s32a"

# a: scale1(1)=3, helper 3+10=13 (tally 1), a_second: scale2(13)=65, tally 2
#    -> 2065; second call: 6+10=16 (tally 3), 80 + 4000 -> 4080.
# b: 3+20=23 (tally 1), 115 + 2000 -> 2115.  main's helper(7)=6, tally 500.
want="a=2065 b=2115 a2=4080 acount=4 bcount=2 helper=6 tally=500"
got="$("$EMU" "$W/prog.s32x" 2>/dev/null | grep '^a=' || true)"
if [ "$got" != "$want" ]; then
    echo "FAIL: want '$want'"
    echo "       got '$got'"
    exit 1
fi

# The partial-link output's own shape: only the entry points (renamed) and
# the undefined references are global; nothing hidden is left global.
dump="$("$ROOT/tools/utilities/slow32dump" -s "$W/mod_a.s32o")"
globals="$(echo "$dump" | awk '$5 == "GLOBAL" || $5 == "WEAK" {print $NF}' | sort | tr '\n' ' ')"
want_globals="a_count a_entry "
if [ "$globals" != "$want_globals" ]; then
    echo "FAIL: mod_a globals: want '$want_globals' got '$globals'"
    exit 1
fi
echo "partial link: PASS ($got)"
