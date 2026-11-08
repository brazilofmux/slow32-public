#!/usr/bin/env bash
# build-kit.sh OUT [part ...] -- the kit (what ~/s32x holds) built from this
# tree.  The Mac and the image build run the same script
# (docs/plans/containers.md):
#   scripts/build-kit.sh ~/s32x                  on a machine with the tree's tools and LLVM
#   Dockerfile.selfhost   runs the stage08 part
#   Dockerfile.kit        runs the apps and basic parts, over selfhost's /kit
#
# Parts (all three, in order, when none is named):
#   stage08  selfhost/stage08 built from the checked-in stage07 on the emulator,
#            laid out as RUNTIME_KIT.md "Regenerating" says: cc.s32x and the
#            tools at OUT/, selfhost/{include,crt0.s32o,libc.s32a,README.md}
#   apps     the programs the kit ships, each by its own build script, with
#            clang and llc from LLVM_BIN (default ~/llvm-project/build/bin,
#            which every script assumes)
#   basic    the two BASIC compilers and their run time, by the kit's own
#            tools (sbc/build-kit.sh)
#
# Never in the kit: regal.s32x (the user's; see build-kit-image.sh).
# Environment: SELFHOST_EMU (stage08's engine), KIT_EMU (basic's engine).
set -euo pipefail
# One collation everywhere: the build scripts glob their sources, and a glob
# sorts by locale -- sbasic's src/builtin_names.c is third on a Mac in
# en_US.UTF-8 and fourth in the image's C locale, which reorders the link and
# changes every byte after it.  With this the Mac and the image build the
# same kit.
export LC_ALL=C
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
[ $# -ge 1 ] || { echo "usage: build-kit.sh OUT [stage08|apps|basic ...]" >&2; exit 2; }
mkdir -p "$1"; OUT="$(cd "$1" && pwd)"; shift
PARTS=("$@"); [ ${#PARTS[@]} -gt 0 ] || PARTS=(stage08 apps basic)
say() { echo "kit: $*"; }

part_stage08() {
    local S="$ROOT/selfhost/stage08"
    ( cd "$S" && make clean >/dev/null && make >/dev/null )
    mkdir -p "$OUT/selfhost"
    cp "$S"/cc.s32x "$S"/s32-as.s32x "$S"/s32-ld.s32x "$S"/s32-ar.s32x \
       "$S"/slow32dis.s32x "$S"/slow32dump.s32x "$OUT/"
    rm -rf "$OUT/selfhost/include"; cp -r "$S/include" "$OUT/selfhost/include"
    cp "$S/lib/crt0.s32o" "$OUT/selfhost/"
    cp "$S/RUNTIME_KIT.md" "$OUT/selfhost/README.md"
    rm -f "$OUT/selfhost/libc.s32a"
    local objs=(); for o in "$S"/lib/*.s32o; do [ "$(basename "$o")" = crt0.s32o ] || objs+=("$o"); done
    "${S32_AR:-$ROOT/tools/utilities/s32-ar}" rc "$OUT/selfhost/libc.s32a" "${objs[@]}" >/dev/null
    say "stage08: cc.s32x $(shasum -a 256 "$OUT/cc.s32x" | cut -c1-16)"
}

part_apps() {
    local cc="$ROOT/slow32cc"
    # examples: benchmark_core without MMIO (it prints through DEBUG), the
    # rest on the MMIO libc
    "$cc" -O2 "$ROOT/examples/benchmark_core.c" -o "$OUT/benchmark_core.s32x" >/dev/null
    for p in phonetic validatecsv; do
        "$cc" --libc=mmio -O2 "$ROOT/examples/$p.c" -o "$OUT/$p.s32x" >/dev/null
    done
    # each app's own build script, which leaves <app>/<app>.s32x
    for a in lua nano prolog sbasic xbase; do
        ( cd "$ROOT/$a" && bash build.sh >/dev/null )
        cp "$ROOT/$a/$a.s32x" "$OUT/"
    done
    ( cd "$ROOT/forth" && FORTH_RUN=0 bash build.sh >/dev/null )
    cp "$ROOT/forth/kernel.s32x" "$ROOT/forth/prelude.fth" "$OUT/"
    bash "$ROOT/islisp/build.sh" clang >/dev/null
    cp "$ROOT/islisp/out/islisp.s32x" "$OUT/"
    bash "$ROOT/dfsort/build.sh" >/dev/null
    cp "$ROOT/dfsort/out/s32sort.s32x" "$OUT/"
    bash "$ROOT/sqlite/build.sh" >/dev/null
    mkdir -p "$OUT/sqlite"
    cp "$ROOT/sqlite/out/sqlite3.s32x" "$OUT/"
    cp "$ROOT/sqlite/out/libsqlite3.s32a" "$ROOT/sqlite/sqlite3.h" "$OUT/sqlite/"
    make -s -C "$ROOT/tzlookup/slow32" tz.s32x >/dev/null
    cp "$ROOT/tzlookup/slow32/tz.s32x" "$OUT/"
    say "apps: built"
}

part_basic() {
    KIT_TOOLS="$OUT" KIT_RT="$OUT/selfhost" bash "$ROOT/sbc/build-kit.sh" "$OUT" >/dev/null
    say "basic: built"
}

for p in "${PARTS[@]}"; do
    case "$p" in stage08|apps|basic) "part_$p" ;; *) echo "build-kit.sh: no part $p" >&2; exit 2 ;; esac
done
