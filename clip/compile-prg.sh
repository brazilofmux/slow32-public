#!/bin/bash
# .prg -> C -> .s32x using dBase objects minus main.c
set -euo pipefail

CLIP_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(dirname "$CLIP_DIR")"
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
CLANG="$LLVM_BIN/clang"
LLC="$LLVM_BIN/llc"
ASM="$ROOT/tools/assembler/slow32asm"
LD="$ROOT/tools/linker/s32-ld"
TARGET=slow32-unknown-none
CFLAGS="-target $TARGET -S -emit-llvm -nostdinc -fno-builtin -I$ROOT/runtime/include -I$ROOT/dbase/src -I$CLIP_DIR/runtime"
OPT="${OPT:--O1}"

if [ $# -lt 1 ]; then
    echo "Usage: $0 input.prg [output.s32x] [more.prg ...]"
    exit 1
fi

PRG="$1"
shift
if [ ! -f "$PRG" ]; then
    echo "not found: $PRG"
    exit 1
fi
BASE="$(basename "$PRG" .prg)"
BASE="$(basename "$BASE" .PRG)"
OUT=""
if [ $# -ge 1 ]; then
    case "$1" in
        *.prg|*.PRG) ;;
        *) OUT="$1"; shift ;;
    esac
fi
OUT="${OUT:-$CLIP_DIR/$BASE.s32x}"
GEN="$CLIP_DIR/out/${BASE}.c"
mkdir -p "$CLIP_DIR/out"

if [ ! -x "$CLIP_DIR/s32-clip" ]; then
    (cd "$CLIP_DIR" && gcc -Wall -Wextra -O2 -o s32-clip s32-clip.c)
else
    if [ "$CLIP_DIR/s32-clip.c" -nt "$CLIP_DIR/s32-clip" ]; then
        (cd "$CLIP_DIR" && gcc -Wall -Wextra -O2 -o s32-clip s32-clip.c)
    fi
fi

echo "  clip $PRG $*"
"$CLIP_DIR/s32-clip" "$PRG" "$@" -o "$GEN"

# dBase runtime objects (everything except the REPL)
OBJECTS=""
for src in "$ROOT/dbase/src"/*.c; do
    b="$(basename "$src" .c)"
    if [ "$b" = "main" ]; then
        continue
    fi
    if [ ! -f "$ROOT/dbase/src/$b.s32o" ] || [ "$src" -nt "$ROOT/dbase/src/$b.s32o" ]; then
        echo "  compile dbase $b.c"
        "$CLANG" $CFLAGS $OPT -DNDEBUG "$src" -o "$ROOT/dbase/src/$b.ll"
        "$LLC" -mtriple=$TARGET "$ROOT/dbase/src/$b.ll" -o "$ROOT/dbase/src/$b.s"
        "$ASM" "$ROOT/dbase/src/$b.s" "$ROOT/dbase/src/$b.s32o"
    fi
    OBJECTS="$OBJECTS $ROOT/dbase/src/$b.s32o"
done

echo "  compile cliprt + generated"
"$CLANG" $CFLAGS $OPT "$CLIP_DIR/runtime/cliprt.c" -o "$CLIP_DIR/out/cliprt.ll"
"$LLC" -mtriple=$TARGET "$CLIP_DIR/out/cliprt.ll" -o "$CLIP_DIR/out/cliprt.s"
"$ASM" "$CLIP_DIR/out/cliprt.s" "$CLIP_DIR/out/cliprt.s32o"

"$CLANG" $CFLAGS $OPT "$GEN" -o "$CLIP_DIR/out/${BASE}.ll"
"$LLC" -mtriple=$TARGET "$CLIP_DIR/out/${BASE}.ll" -o "$CLIP_DIR/out/${BASE}.s"
"$ASM" "$CLIP_DIR/out/${BASE}.s" "$CLIP_DIR/out/${BASE}.s32o"

echo "  link $OUT"
"$LD" --mmio 64K --stack-size 128K --data-size 8M --heap-size 64M -o "$OUT" \
    "$ROOT/runtime/crt0.s32o" \
    "$CLIP_DIR/out/${BASE}.s32o" \
    "$CLIP_DIR/out/cliprt.s32o" \
    $OBJECTS \
    "$ROOT/runtime/libc_mmio.s32a" \
    "$ROOT/runtime/libs32.s32a"
echo "OK"
