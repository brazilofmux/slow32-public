#!/bin/bash
# Build SQLite for SLOW-32: the library (out/libsqlite3.s32a + sqlite3.h),
# the smoke test (out/sqlite3_test.s32x) and the shell (out/sqlite3.s32x).
#
# The amalgamation is pristine (3.51.0): the backend lowers the 32-bit atomic
# builtins SQLite uses to plain loads and stores, so the March 2026 patch
# that forced them is gone, and -O2 is fine (the -O1 pin predated the
# backend work).  Single process: no threads, no shared cache, no WAL (it
# would need exclusive locking mode to run without shared memory), no
# extensions.  EXPLAIN and PRAGMA integrity_check are in: they cost code
# size and nothing else, and a library wants them; so are the progress
# handler, the authorizer, trace and column decltype, which the shell uses.
#
# -Os, not -O2: a JAL reaches +/-1MB and the linker has no veneers yet
# (tools/linker/ISSUES.md), so a program's callers must all sit within 1MB of
# their callees.  The library is 697KB of code at -Os and 995KB at -O2, and
# at -O2 the libc behind it was out of main's reach.
set -e
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$SCRIPT_DIR/.."
TOOLCHAIN="$PROJECT_DIR/tools"
RUNTIME="$PROJECT_DIR/runtime"
OUTDIR="$SCRIPT_DIR/out"
LLVM="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
CLANG="$LLVM/clang"; LLC="$LLVM/llc"
mkdir -p "$OUTDIR"

CFLAGS="-target slow32-unknown-none -S -emit-llvm -I$RUNTIME/include -I$SCRIPT_DIR -Os"
OPTS="-DSQLITE_OS_OTHER=1 -DSQLITE_THREADSAFE=0 -DSQLITE_OMIT_WAL=1 -DSQLITE_OMIT_LOAD_EXTENSION=1
      -DSQLITE_OMIT_SHARED_CACHE=1 -DSQLITE_OMIT_DEPRECATED=1 -DSQLITE_OMIT_UTF16=1
      -DSQLITE_OMIT_COMPILEOPTION_DIAGS=1 -DSQLITE_DEFAULT_MEMSTATUS=0
      -DSQLITE_DQS=0 -DSQLITE_LIKE_DOESNT_MATCH_BLOBS -DSQLITE_CORE -DSQLITE_BYTEORDER=0"

cc1() {   # cc1 name.c [cflags...] -> out/name.s32o
    local src="$1"; shift
    local b="$(basename "$src" .c)"
    echo "  $b.c"
    $CLANG $CFLAGS $OPTS "$@" "$src" -o "$OUTDIR/$b.ll"
    $LLC -mtriple=slow32-unknown-none "$OUTDIR/$b.ll" -o "$OUTDIR/$b.s"
    "$TOOLCHAIN/assembler/slow32asm" "$OUTDIR/$b.s" "$OUTDIR/$b.s32o"
}
link() {  # link out.s32x objects...
    local out="$1"; shift
    "$TOOLCHAIN/linker/s32-ld" --mmio 64K --code-size 2M --stack-size 256K --heap-size 32M -o "$out" \
        "$RUNTIME/crt0.s32o" "$@" "$OUTDIR/libsqlite3.s32a" "$RUNTIME/libc_mmio.s32a" "$RUNTIME/libs32.s32a"
}

echo "=== library"
cc1 "$SCRIPT_DIR/sqlite3.c"
cc1 "$SCRIPT_DIR/slow32_vfs.c"
rm -f "$OUTDIR/libsqlite3.s32a"
"$TOOLCHAIN/utilities/s32-ar" rc "$OUTDIR/libsqlite3.s32a" "$OUTDIR/sqlite3.s32o" "$OUTDIR/slow32_vfs.s32o"
cp "$SCRIPT_DIR/sqlite3.h" "$OUTDIR/"
echo "  -> $OUTDIR/libsqlite3.s32a, sqlite3.h"

echo "=== smoke test"
cc1 "$SCRIPT_DIR/main.c"
link "$OUTDIR/sqlite3_test.s32x" "$OUTDIR/main.s32o"
echo "  -> $OUTDIR/sqlite3_test.s32x"

if [ "${1:-}" != "--no-shell" ] && [ -f "$SCRIPT_DIR/shell.c" ]; then
    echo "=== shell"
    SHELL_CFLAGS="-include $SCRIPT_DIR/compat/shell_slow32.h -I$SCRIPT_DIR/compat"
    cc1 "$SCRIPT_DIR/shell.c" $SHELL_CFLAGS
    cc1 "$SCRIPT_DIR/shell_slow32.c"
    link "$OUTDIR/sqlite3.s32x" "$OUTDIR/shell.s32o" "$OUTDIR/shell_slow32.s32o"
    echo "  -> $OUTDIR/sqlite3.s32x"
fi
echo "=== done"
