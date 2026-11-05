#!/bin/bash
# build-kit.sh [outdir] -- the two BASIC compilers and their run time, built
# by the self-hosted toolchain alone: stage08 cc, s32-as, s32-ld and s32-ar,
# SLOW-32 programs on the emulator, against stage08's libc.  No host compiler
# and no LLVM touches any of it.  What it leaves is what ~/s32x carries:
#
#   s32-bc.s32x          the QuickBASIC compiler (.bas to SLOW-32 assembly)
#   s32-sbc.s32x         the SLOW BASIC compiler (.bas to C)
#   basic/bc_rt.s32o     s32-bc's run time
#   basic/libsbasic.s32a the interpreter's objects minus main, and sbcrt
#   basic/c-include/     the headers s32-sbc's C includes
#
#   KIT_TOOLS  the directory holding cc.s32x, s32-as.s32x, s32-ld.s32x,
#              s32-ar.s32x (default selfhost/stage08)
#   KIT_RT     the directory holding include/, crt0.s32o, libc.s32a
#              (default: stage08's include and lib, archived here)
#   KIT_EMU    the engine (default tools/dbt/slow32-dbt)
set -euo pipefail
SBC_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(dirname "$SBC_DIR")"
OUT="${1:-$SBC_DIR/out/kit}"
TOOLS="${KIT_TOOLS:-$ROOT/selfhost/stage08}"
EMU="${KIT_EMU:-$ROOT/tools/dbt/slow32-dbt}"
SB="$ROOT/sbasic/src"
mkdir -p "$OUT/basic/c-include"
W="$(mktemp -d "${TMPDIR:-/tmp}/kitbasic.XXXXXX")"
trap 'rm -rf "${W:?}"' EXIT

if [ -n "${KIT_RT:-}" ]; then
    INC="$KIT_RT/include"; CRT0="$KIT_RT/crt0.s32o"; LIBC="$KIT_RT/libc.s32a"
else
    INC="$ROOT/selfhost/stage08/include"
    CRT0="$ROOT/selfhost/stage08/lib/crt0.s32o"
    LIBC="$W/libc.s32a"
    members=()
    for o in "$ROOT"/selfhost/stage08/lib/*.s32o; do
        [ "$(basename "$o")" = crt0.s32o ] || members+=("$o")
    done
    "$EMU" "$TOOLS/s32-ar.s32x" rc "$LIBC" "${members[@]}" >/dev/null
fi

# The interpreter's machine services that stage08's libc does not have:
# the runtime's own, compiled here (they need only s32_mmio_request).
services=(args_mmio term_mmio tube_mmio dpc_mmio)

# source.c -> $W/<prefix><base>.s32o; cc.s32x narrates on stderr, kept for a failure
compile() {  # compile prefix source.c
    local b; b="$1$(basename "$2" .c)"
    # (the runtime's headers last: for what only it has, term.h, tube.h, slow32_args.h)
    if ! "$EMU" "$TOOLS/cc.s32x" -I"$INC" -I"$SB" -I"$SBC_DIR/bc" -I"$SBC_DIR/runtime" \
            -I"$ROOT/runtime" -I"$ROOT/runtime/include" -DNDEBUG \
            "$2" "$W/$b.s" >/dev/null 2>"$W/$b.log" || [ ! -s "$W/$b.s" ]; then
        echo "build-kit.sh: cc failed on $2" >&2; tail -5 "$W/$b.log" >&2; exit 1
    fi
    "$EMU" "$TOOLS/s32-as.s32x" "$W/$b.s" "$W/$b.s32o" >/dev/null ||
        { echo "build-kit.sh: as failed on $b.s" >&2; exit 1; }
    echo "$W/$b.s32o"
}

front=(program lexer parser ast value error builtin_names)
link_tool() {  # link_tool out.s32x objects...
    local o="$1"; shift
    "$EMU" "$TOOLS/s32-ld.s32x" -o "$o" --mmio 64K --heap-size max --stack-size 16M --mem-size 1G \
        "$CRT0" "$@" "$LIBC" >"$W/ld.log" 2>&1 || { cat "$W/ld.log" >&2; echo "build-kit.sh: link failed: $o" >&2; exit 1; }
}

objs=(); for f in "${front[@]}" strict; do objs+=("$(compile bc_ "$SB/$f.c")"); done
objs+=("$(compile bc_ "$SBC_DIR/bc/bc.c")")
link_tool "$OUT/s32-bc.s32x" "${objs[@]}"
echo "OK  $OUT/s32-bc.s32x"

objs=(); for f in "${front[@]}" lower; do objs+=("$(compile sbc_ "$SB/$f.c")"); done
objs+=("$(compile sbc_ "$SBC_DIR/s32-sbc.c")")
link_tool "$OUT/s32-sbc.s32x" "${objs[@]}"
echo "OK  $OUT/s32-sbc.s32x"

cp "$(compile rt_ "$SBC_DIR/bc/bc_rt.c")" "$OUT/basic/bc_rt.s32o"
objs=("$(compile rt_ "$SBC_DIR/runtime/sbcrt.c")")
for src in "$SB"/*.c; do
    [ "$(basename "$src")" = main.c ] || objs+=("$(compile rt_ "$src")")
done
for f in "${services[@]}"; do objs+=("$(compile rt_ "$ROOT/runtime/$f.c")"); done
rm -f "$OUT/basic/libsbasic.s32a"
"$EMU" "$TOOLS/s32-ar.s32x" rc "$OUT/basic/libsbasic.s32a" "${objs[@]}" >/dev/null
cp "$SB"/*.h "$SBC_DIR/runtime/sbcrt.h" "$OUT/basic/c-include/"
echo "OK  $OUT/basic (${#objs[@]} objects)"
