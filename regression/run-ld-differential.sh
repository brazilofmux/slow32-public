#!/usr/bin/env bash
# Differential harness: the HOST linker vs the SELF-HOSTED one.
#
# Companion to run-as-differential.sh, and deliberately a different KIND
# of comparison.  Two independent assemblers can be held to producing
# the same object bytes; two independent linkers cannot reasonably be
# held to the same executable bytes, because file layout (section file
# offsets, string-table placement, padding) is a free choice.  What must
# match is what the program DOES.
#
# So this links the same objects with both linkers and compares:
#   - the loaded image: section virtual addresses, sizes and flags, and
#     the entry point -- if these differ, addresses differ, which is a
#     real divergence
#   - BEHAVIOUR: run both binaries on the same input under the reference
#     interpreter and require identical stdout and exit status
#
# That framing is what caught the first real bug here: the self-hosted
# linker set `bss_va = data_va + data_sz` with no alignment, so a real
# link put .bss at 0x2384D.  Contributions are placed at offsets
# RELATIVE to bss_va, so an unaligned base carries through to every
# object in BSS.  It survived because nothing enforces alignment --
# every engine permits unaligned word access, and the emulator's
# S32_TRAP_ON_UNALIGNED is defined but never referenced.
set -uo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"
HOST_LD="$ROOT/tools/linker/s32-ld"
SELF_LD="$ROOT/selfhost/stage08/s32-ld.s32x"
EMU="${SELFHOST_EMU:-$ROOT/selfhost/stage00/s32-emu}"
RUN="$ROOT/tools/emulator/slow32"
DUMP="$ROOT/tools/utilities/slow32dump"
W="$(mktemp -d)"
trap 'rm -rf "$W"' EXIT

for t in "$HOST_LD" "$DUMP" "$EMU" "$RUN"; do
    [ -x "$t" ] || { echo "missing tool: $t" >&2; exit 1; }
done
[ -f "$SELF_LD" ] || { echo "missing $SELF_LD" >&2; exit 1; }

L="$ROOT/selfhost/stage08/lib"

# The loaded image: addresses and sizes, never file offsets.
image() {
    "$DUMP" "$1" 2>/dev/null | awk '
        /^  Entry Point:/ {print "entry", $3}
        /^Idx Name|^Name  /{on=1;next}
        /^---/{next}
        on && NF>=6 && $1 ~ /^\./ {print $1, $3, $4, $NF}
        on && NF==0{exit}'
}

pass=0; fail=0
# IMAGE=0: behaviour only.  Clang's objects carry .eh_frame, which the
# host linker lays out among the read-only sections and the selfhost one
# drops (it builds no C++ unwinding), so their addresses differ by design.
IMAGE=1
SELF_REJECT=SKIP   # FAIL where the selfhost linker must accept the input
check() {
    local tag="$1"; shift
    if ! "$HOST_LD" -o "$W/h.s32x" "$@" >/dev/null 2>&1 || [ ! -s "$W/h.s32x" ]; then
        printf "  %-26s SKIP (host linker rejects)\n" "$tag"; return
    fi
    if ! timeout 600 "$EMU" "$SELF_LD" -o "$W/s.s32x" "$@" >/dev/null 2>&1 || [ ! -s "$W/s.s32x" ]; then
        if [ "$SELF_REJECT" = FAIL ]; then
            printf "  %-26s DIFFER (selfhost linker rejects)\n" "$tag"; fail=$((fail+1))
        else
            printf "  %-26s SKIP (selfhost linker rejects)\n" "$tag"
        fi
        return
    fi
    # Sorted: section-table ORDER is not semantic (each section carries
    # its own vaddr), and the two linkers emit them in different orders.
    image "$W/h.s32x" | sort > "$W/h.img"
    image "$W/s.s32x" | sort > "$W/s.img"
    if [ "$IMAGE" = 1 ] && ! diff -q "$W/h.img" "$W/s.img" >/dev/null; then
        printf "  %-26s DIFFER (image layout)\n" "$tag"; fail=$((fail+1))
        diff "$W/h.img" "$W/s.img" | head -8 | sed 's/^/      /'
        return
    fi
    # Behaviour: same stdout, same exit status.
    "$RUN" -q "$W/h.s32x" >"$W/h.out" 2>&1; local hr=$?
    "$RUN" -q "$W/s.s32x" >"$W/s.out" 2>&1; local sr=$?
    if [ "$hr" != "$sr" ] || ! diff -q "$W/h.out" "$W/s.out" >/dev/null; then
        printf "  %-26s DIFFER (behaviour: rc %s vs %s)\n" "$tag" "$hr" "$sr"; fail=$((fail+1))
        return
    fi
    printf "  %-26s AGREE\n" "$tag"; pass=$((pass+1))
}

# Corpus: the SELFHOST world's objects, image and behaviour, and then
# clang's (below), behaviour only.  The selfhost linker used to refuse
# clang's objects ("reloc target out of range": relocations into the
# .eh_frame it does not lay out) and the runtime's weak references.
LIBC_OBJS=""
for o in "$L"/*.s32o; do
    case "$(basename "$o")" in crt0.s32o) ;; *) LIBC_OBJS="$LIBC_OBJS $o" ;; esac
done

CC="$ROOT/selfhost/stage08/cc.s32x"
AS="$ROOT/selfhost/stage08/s32-as.s32x"
n=0
if [ -f "$CC" ] && [ -f "$AS" ] && [ -f "$L/crt0.s32o" ]; then
    for c in "$ROOT"/selfhost/stage08/tests/*.c; do
        [ -f "$c" ] || continue
        [ "$n" -ge 8 ] && break
        tag="$(basename "$c" .c)"
        timeout 300 "$EMU" "$CC" "$c" "$W/t.s" >/dev/null 2>&1 || continue
        [ -s "$W/t.s" ] || continue
        timeout 300 "$EMU" "$AS" "$W/t.s" "$W/t.s32o" >/dev/null 2>&1 || continue
        [ -s "$W/t.s32o" ] || continue
        check "$tag" --mmio 64K "$L/crt0.s32o" "$W/t.s32o" $LIBC_OBJS
        n=$((n+1))
    done
fi

# Clang's world: regression objects with the runtime's archives, sizes
# given (the two linkers' default stack differs, 64KB and 1MB); and a
# weak reference left undefined, which must resolve to 0 -- the runtime's
# locale hooks are such references, and this linker rejected them.
IMAGE=0
SELF_REJECT=FAIL
RT="$ROOT/runtime"
n=0
for o in "$HERE"/results/feature-*/test.s32o; do
    [ -f "$o" ] || continue
    [ "$n" -ge 8 ] && break
    check "clang:$(basename "$(dirname "$o")")" --mmio 64K --stack-size 64K --heap-size 1M \
        "$RT/crt0.s32o" "$o" "$RT/libc_mmio.s32a" "$RT/libs32.s32a"
    n=$((n+1))
done
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
if [ -x "$LLVM_BIN/clang" ]; then
    printf '%s\n' '#include <stdio.h>' \
        'extern int absent(void) __attribute__((weak));' \
        'int main(void) { printf("%s\n", absent ? "present" : "absent"); return absent ? 1 : 0; }' > "$W/weak.c"
    if "$LLVM_BIN/clang" -target slow32-unknown-none -S -emit-llvm -O2 -I"$RT/include" "$W/weak.c" -o "$W/weak.ll" 2>/dev/null &&
       "$LLVM_BIN/llc" -mtriple=slow32-unknown-none "$W/weak.ll" -o "$W/weak.s" 2>/dev/null &&
       "$ROOT/tools/assembler/slow32asm" "$W/weak.s" "$W/weak.s32o" >/dev/null 2>&1; then
        check "clang:weak-undefined" --mmio 64K --stack-size 64K --heap-size 1M \
            "$RT/crt0.s32o" "$W/weak.s32o" "$RT/libc_mmio.s32a" "$RT/libs32.s32a"
    else
        printf "  %-26s SKIP (clang cannot build it)\n" "clang:weak-undefined"
    fi
fi

echo ""
echo "linker differential: $pass agree, $fail differ"
[ "$fail" -eq 0 ]
