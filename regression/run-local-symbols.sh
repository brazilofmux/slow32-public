#!/usr/bin/env bash
# Local symbols: a static function or table has its name in the object and
# in the executable, from both toolchains, so that a map or a profile can
# tell it from the global before it.  (slow32asm left them out, and s32-ld
# wrote globals only: TinyMUX's profile credited the engine's statics to
# their neighbours.)
#
#   1. objects from each assembler name their statics, LOCAL, and no .L label;
#   2. executables from each linker name them as the source did -- no
#      "name@file" -- two files' statics of one name both, and -s drops them;
#   3. a program's own static memcpy is not the library's: no engine's hook
#      binds to it, and every engine prints what the reference prints;
#   4. capacity: an object of more symbols than the self-hosted tools' old
#      fixed tables (8,192 per object in s32-ld, 32,768 in s32-as) links and runs.
set -uo pipefail
# (a negative check counts with grep -c: under pipefail, `! a | grep -q` passes
# whenever grep's early exit kills a with SIGPIPE, whatever a printed)

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"
LLVM="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
AS="$ROOT/tools/assembler/slow32asm"
LD="$ROOT/tools/linker/s32-ld"
DUMP="$ROOT/tools/utilities/slow32dump"
RT="$ROOT/runtime"
DBT="$ROOT/tools/dbt/slow32-dbt"
S8="$ROOT/selfhost/stage08"
QEMU="${QEMU:-$HOME/qemu/build/qemu-system-slow32}"
W="$(mktemp -d "${TMPDIR:-/tmp}/s32-localsym.XXXXXX")"
trap 'rm -rf "${W:?}"' EXIT
pass=0 fail=0

check() { # what, status
    if [ "$2" -eq 0 ]; then pass=$((pass + 1)); echo "  ok    $1"
    else fail=$((fail + 1)); echo "  FAIL  $1"; fi
}
# the name column of a file's symbol table, one binding
names() { "$DUMP" "$1" 2>/dev/null | awk -v b="$2" '/^Num Value/{on=1;next} /^---/{next} on&&NF>=6{if($5==b)print $NF} on&&NF==0{on=0}' | sort; }

cat > "$W/a.c" <<'E'
int printf(const char *, ...);
static int helper(int x) { volatile int y = x; return y * 3; }
static const int table_a[3] = { 2, 3, 5 };
int from_a(int x) { return helper(x) + table_a[x & 1]; }
E
cat > "$W/b.c" <<'E'
static int helper(int x) { volatile int y = x; return y + 11; }
int from_b(int x) { return helper(x); }
E
cat > "$W/m.c" <<'E'
int printf(const char *, ...);
int from_a(int), from_b(int);
int main(void) { printf("%d %d\n", from_a(4), from_b(4)); return 0; }
E

echo "=== clang, slow32asm, s32-ld"
for u in a b m; do
    "$LLVM/clang" -target slow32-unknown-none -S -O1 -fno-inline -I"$RT/include" "$W/$u.c" -o "$W/$u.s" &&
    "$AS" "$W/$u.s" "$W/$u.s32o" > /dev/null || { echo "  cannot build $u"; exit 2; }
done
names "$W/a.s32o" LOCAL | grep -qx helper;   check "object: a static function, LOCAL" $?
names "$W/a.s32o" LOCAL | grep -qx table_a;  check "object: a static table, LOCAL" $?
[ "$(names "$W/a.s32o" LOCAL | grep -c '^\.L')" = 0 ];  check "object: no assembler-private label" $?
names "$W/a.s32o" GLOBAL | grep -qx from_a;  check "object: the global as before" $?

"$LD" -o "$W/h.s32x" --print-map --mmio 64K "$RT/crt0.s32o" "$W/a.s32o" "$W/b.s32o" "$W/m.s32o" \
    "$RT/libc_mmio.s32a" "$RT/libs32.s32a" > /dev/null 2>&1
check "links" $?
[ "$("$DBT" "$W/h.s32x")" = "14 15" ]; check "runs, each file its own helper" $?
[ "$(names "$W/h.s32x" LOCAL | grep -cx helper)" = 2 ]; check "executable: both files' helper" $?
[ "$("$DUMP" "$W/h.s32x" 2>/dev/null | grep -c 'helper@')" = 0 ]; check "executable: the source's name, no @file" $?
[ "$(names "$W/h.s32x" LOCAL | grep -c '^\.L')" = 0 ]; check "executable: no assembler-private label" $?
"$LD" -s -o "$W/hs.s32x" --mmio 64K "$RT/crt0.s32o" "$W/a.s32o" "$W/b.s32o" "$W/m.s32o" \
    "$RT/libc_mmio.s32a" "$RT/libs32.s32a" > /dev/null 2>&1
[ -z "$(names "$W/hs.s32x" LOCAL)" ]; check "-s leaves no local name" $?
[ "$("$DBT" "$W/hs.s32x")" = "14 15" ]; check "-s runs" $?

echo "=== stage08 cc, s32-as, s32-ld"
if [ -x "$DBT" ] && [ -f "$S8/s32-ld.s32x" ]; then
    S8L="$S8/lib"
    for u in a b; do
        "$DBT" "$S8/cc.s32x" -O1 "$W/$u.c" "$W/s$u.s" > /dev/null 2>&1 || "$DBT" "$S8/cc.s32x" "$W/$u.c" "$W/s$u.s" > /dev/null 2>&1
        "$DBT" "$S8/s32-as.s32x" "$W/s$u.s" "$W/s$u.s32o" > /dev/null 2>&1 || check "s32-as $u" 1
    done
    # stage08 may inline a static away; a table it must keep
    names "$W/sa.s32o" LOCAL | grep -qx table_a; check "s32-as: a static table, LOCAL" $?
    [ "$(names "$W/sa.s32o" LOCAL | grep -c '^\.L')" = 0 ]; check "s32-as: no label that is referenced only" $?

    # both assemblers keep one list of names over the same source
    cat > "$W/same.s" <<'E'
.text
.globl pub
pub:
    jal r0, priv
priv:
.L1:
    jalr r0, r31, 0
.data
tab:
    .word 1
.set konst, 5
.rodata
.bss
E
    "$AS" "$W/same.s" "$W/same_h.s32o" > /dev/null
    "$DBT" "$S8/s32-as.s32x" "$W/same.s" "$W/same_s.s32o" > /dev/null 2>&1
    diff <(names "$W/same_h.s32o" LOCAL | grep -v '^\.') <(names "$W/same_s.s32o" LOCAL | grep -v '^\.') > "$W/same.diff"
    check "the two assemblers name the same locals" $?
fi

echo "=== a program's own memcpy"
cat > "$W/own.c" <<'E'
int printf(const char *, ...);
/* not the library's: it fills.  A hook bound by name would copy instead. */
__attribute__((noinline)) static void *memcpy(void *d, const void *s, unsigned n) {
    char *p = d; (void)s;
    while (n--) *p++ = 'X';
    return d;
}
void *(*volatile call)(void *, const void *, unsigned) = memcpy;
int main(void) {
    char a[6] = "hello", b[6] = "abcde";
    call(b, a, 5);
    printf("%s\n", b);
    return 0;
}
E
"$LLVM/clang" -target slow32-unknown-none -S -O1 -fno-builtin -fno-inline -I"$RT/include" "$W/own.c" -o "$W/own.s" &&
"$AS" "$W/own.s" "$W/own.s32o" > /dev/null &&
"$LD" -o "$W/own.s32x" --mmio 64K "$RT/crt0.s32o" "$W/own.s32o" "$RT/libc_mmio.s32a" "$RT/libs32.s32a" > /dev/null 2>&1
check "builds" $?
names "$W/own.s32x" LOCAL | grep -qx memcpy; check "its name is in the table, LOCAL" $?
[ "$("$DBT" "$W/own.s32x")" = "XXXXX" ]; check "slow32-dbt calls it" $?
[ "$("$ROOT/tools/emulator/slow32-fast" "$W/own.s32x" 2>/dev/null | grep -c XXXXX)" = 1 ]; check "slow32-fast calls it" $?
if [ -x "$QEMU" ]; then
    timeout 20 "$QEMU" -M slow32-tcg -nographic -kernel "$W/own.s32x" 2>/dev/null | grep -q XXXXX; check "qemu calls it" $?
else
    echo "  skip  qemu (no $QEMU)"
fi

echo "=== capacity"
python3 - "$W/big.s" <<'E'
import sys
n = 40000
with open(sys.argv[1], 'w') as f:
    f.write('.text\n.globl main\nmain:\n    addi r1, r0, 0\n    jalr r0, r31, 0\n')
    for i in range(n):
        f.write('st%d:\n    addi r1, r1, 1\n' % i)
    f.write('.data\n')
    for i in range(n):
        f.write('dt%d:\n    .word %d\n' % (i, i))
E
"$AS" "$W/big.s" "$W/big_h.s32o" > /dev/null; check "slow32asm: 80,000 names" $?
[ "$(names "$W/big_h.s32o" LOCAL | wc -l | tr -d ' ')" = 80000 ]; check "slow32asm: all of them" $?
if [ -f "$S8/s32-as.s32x" ]; then
    "$DBT" "$S8/s32-as.s32x" "$W/big.s" "$W/big_s.s32o" > "$W/as.log" 2>&1; check "s32-as: 80,000 names" $?
    [ "$(names "$W/big_s.s32o" LOCAL | wc -l | tr -d ' ')" = 80000 ]; check "s32-as: all of them" $?
    "$DBT" "$S8/s32-ld.s32x" -o "$W/big.s32x" "$S8/crt0.s32o" "$W/big_s.s32o" "$S8/libc/libc.s32a" > "$W/ld.log" 2>&1 ||
    "$DBT" "$S8/s32-ld.s32x" -o "$W/big.s32x" "$RT/crt0.s32o" "$W/big_s.s32o" "$RT/libc_mmio.s32a" "$RT/libs32.s32a" > "$W/ld.log" 2>&1
    st=$?; check "s32-ld: an object of 80,000" $st
    [ $st -ne 0 ] && head -3 "$W/ld.log"
    [ "$(names "$W/big.s32x" LOCAL | wc -l | tr -d ' ')" -ge 80000 ]; check "s32-ld: writes them" $?
fi

echo "local symbols: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
