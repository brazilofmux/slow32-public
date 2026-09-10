#!/bin/bash
# Build SQLite for SLOW-32 with the stage08 self-hosted compiler: the same
# three artifacts as build.sh -- out/stage08/libsqlite3.s32a, the smoke test
# and the shell -- from the same pristine sources, against stage08's own
# libc and crt0.  Both programs print output byte-identical to the clang
# build's (selfhost ISSUES-67).
#
# The compiler runs under an emulator (SELFHOST_EMU, default slow32-dbt);
# the amalgamation takes about a minute.  Assembly and linking use the host
# tools.  -mlong-calls throughout: stage08 has no -Os, the library is 1.2MB
# of code, and a JAL reaches +/-1MB.
set -e
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$SCRIPT_DIR/.."
TOOLCHAIN="$PROJECT_DIR/tools"
STAGE08="$PROJECT_DIR/selfhost/stage08"
OUTDIR="$SCRIPT_DIR/out/stage08"
EMU="${SELFHOST_EMU:-$TOOLCHAIN/dbt/slow32-dbt}"
[ -x "$EMU" ] || EMU="$TOOLCHAIN/emulator/slow32-fast"
CC="$STAGE08/cc.s32x"
[ -f "$CC" ] || { echo "missing $CC (run selfhost/stage08/build-s12cc.sh)" >&2; exit 1; }
mkdir -p "$OUTDIR"

OPTS="-DSQLITE_OS_OTHER=1 -DSQLITE_THREADSAFE=0 -DSQLITE_OMIT_WAL=1 -DSQLITE_OMIT_LOAD_EXTENSION=1
      -DSQLITE_OMIT_SHARED_CACHE=1 -DSQLITE_OMIT_DEPRECATED=1 -DSQLITE_OMIT_UTF16=1
      -DSQLITE_OMIT_COMPILEOPTION_DIAGS=1 -DSQLITE_DEFAULT_MEMSTATUS=0
      -DSQLITE_DQS=0 -DSQLITE_LIKE_DOESNT_MATCH_BLOBS -DSQLITE_CORE -DSQLITE_BYTEORDER=0"

# Call-model override for measurement (GitHub issue 74): LONGCALLS= builds
# with short calls so the two models can be compared on identical sources.
LONGCALLS="${LONGCALLS--mlong-calls}"

cc8() {   # cc8 name.c [flags...] -> out/stage08/name.s32o
    local src="$1"; shift
    local b="$(basename "$src" .c)"
    echo "  $b.c"
    "$EMU" "$CC" -I "$STAGE08/include" -I "$SCRIPT_DIR" $OPTS $LONGCALLS "$@" \
        "$src" "$OUTDIR/$b.s" > "$OUTDIR/$b.cc.log" 2>&1 \
        || { tail -5 "$OUTDIR/$b.cc.log" >&2; echo "stage08 cc failed: $src" >&2; exit 1; }
    "$TOOLCHAIN/assembler/slow32asm" "$OUTDIR/$b.s" "$OUTDIR/$b.s32o" > "$OUTDIR/$b.as.log" 2>&1 \
        || { tail -5 "$OUTDIR/$b.as.log" >&2; echo "assemble failed: $b.s" >&2; exit 1; }
}
link() {  # link out.s32x objects...   (crt0, program, library, libc: long calls need this order)
    local out="$1"; shift
    "$TOOLCHAIN/linker/s32-ld" --mmio 64K --code-size 8M --stack-size 256K --heap-size 32M -o "$out" \
        "$STAGE08/lib/crt0.s32o" "$@" "$OUTDIR/libsqlite3.s32a" "$OUTDIR/libc_stage08.s32a"
}

echo "=== stage08 libc archive"
rm -f "$OUTDIR/libc_stage08.s32a"
"$TOOLCHAIN/utilities/s32-ar" rc "$OUTDIR/libc_stage08.s32a" \
    $(ls "$STAGE08"/lib/*.s32o | grep -v '/crt0\.s32o$') > /dev/null

echo "=== library"
cc8 "$SCRIPT_DIR/sqlite3.c"
cc8 "$SCRIPT_DIR/slow32_vfs.c"
rm -f "$OUTDIR/libsqlite3.s32a"
"$TOOLCHAIN/utilities/s32-ar" rc "$OUTDIR/libsqlite3.s32a" "$OUTDIR/sqlite3.s32o" "$OUTDIR/slow32_vfs.s32o"
cp "$SCRIPT_DIR/sqlite3.h" "$OUTDIR/"
echo "  -> $OUTDIR/libsqlite3.s32a, sqlite3.h"

echo "=== smoke test"
cc8 "$SCRIPT_DIR/main.c"
link "$OUTDIR/sqlite3_test.s32x" "$OUTDIR/main.s32o"
echo "  -> $OUTDIR/sqlite3_test.s32x"

if [ "${1:-}" != "--no-shell" ] && [ -f "$SCRIPT_DIR/shell.c" ]; then
    echo "=== shell"
    # stage08 has no -include: a two-line wrapper stands in for it (not
    # named shell.c: "shell.c" is searched in the includer's directory first).
    printf '#include "compat/shell_slow32.h"\n#include "shell.c"\n' > "$OUTDIR/shell_main.c"
    cc8 "$OUTDIR/shell_main.c" -I "$SCRIPT_DIR/compat"
    cc8 "$SCRIPT_DIR/shell_slow32.c" -I "$SCRIPT_DIR/compat"
    link "$OUTDIR/sqlite3.s32x" "$OUTDIR/shell_main.s32o" "$OUTDIR/shell_slow32.s32o"
    echo "  -> $OUTDIR/sqlite3.s32x"
fi
echo "=== done"
