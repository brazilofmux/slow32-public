#!/bin/bash
# Build SQLite3 for SLOW-32
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$SCRIPT_DIR/.."
TOOLCHAIN="$PROJECT_DIR/tools"
RUNTIME="$PROJECT_DIR/runtime"
OUTDIR="$SCRIPT_DIR/out"
CLANG="$HOME/llvm-project/build/bin/clang"
LLC="$HOME/llvm-project/build/bin/llc"

mkdir -p "$OUTDIR"

CFLAGS="-target slow32-unknown-none -S -emit-llvm -I$RUNTIME/include -I$SCRIPT_DIR"

# SQLite compile-time options
SQLITE_OPTS=""
SQLITE_OPTS="$SQLITE_OPTS -DSQLITE_OS_OTHER=1"
SQLITE_OPTS="$SQLITE_OPTS -DSQLITE_THREADSAFE=0"
SQLITE_OPTS="$SQLITE_OPTS -DSQLITE_OMIT_WAL=1"
SQLITE_OPTS="$SQLITE_OPTS -DSQLITE_OMIT_LOAD_EXTENSION=1"
SQLITE_OPTS="$SQLITE_OPTS -DSQLITE_OMIT_SHARED_CACHE=1"
SQLITE_OPTS="$SQLITE_OPTS -DSQLITE_OMIT_AUTOINIT=1"
SQLITE_OPTS="$SQLITE_OPTS -DSQLITE_OMIT_DEPRECATED=1"
SQLITE_OPTS="$SQLITE_OPTS -DSQLITE_OMIT_UTF16=1"
SQLITE_OPTS="$SQLITE_OPTS -DSQLITE_OMIT_PROGRESS_CALLBACK=1"
SQLITE_OPTS="$SQLITE_OPTS -DSQLITE_OMIT_COMPLETE=1"
SQLITE_OPTS="$SQLITE_OPTS -DSQLITE_OMIT_DECLTYPE=1"
SQLITE_OPTS="$SQLITE_OPTS -DSQLITE_OMIT_TRACE=1"
SQLITE_OPTS="$SQLITE_OPTS -DSQLITE_OMIT_AUTHORIZATION=1"
SQLITE_OPTS="$SQLITE_OPTS -DSQLITE_OMIT_EXPLAIN=1"
SQLITE_OPTS="$SQLITE_OPTS -DSQLITE_OMIT_INTEGRITY_CHECK=1"
SQLITE_OPTS="$SQLITE_OPTS -DSQLITE_OMIT_COMPILEOPTION_DIAGS=1"
SQLITE_OPTS="$SQLITE_OPTS -DSQLITE_DEFAULT_MEMSTATUS=0"
SQLITE_OPTS="$SQLITE_OPTS -DSQLITE_DQS=0"
SQLITE_OPTS="$SQLITE_OPTS -DSQLITE_LIKE_DOESNT_MATCH_BLOBS"
SQLITE_OPTS="$SQLITE_OPTS -DSQLITE_CORE"
SQLITE_OPTS="$SQLITE_OPTS -DSQLITE_BYTEORDER=0"

# Step 1: Compile sqlite3.c (the big one — use -O1 to avoid optimizer issues)
echo "=== Compiling sqlite3.c (this may take a while) ==="
$CLANG $CFLAGS -O1 $SQLITE_OPTS "$SCRIPT_DIR/sqlite3.c" -o "$OUTDIR/sqlite3.ll"
echo "  -> sqlite3.ll"

echo "=== LLC: sqlite3.ll -> sqlite3.s ==="
$LLC -mtriple=slow32-unknown-none "$OUTDIR/sqlite3.ll" -o "$OUTDIR/sqlite3.s"
echo "  -> sqlite3.s"

# Step 2: Compile VFS
echo "=== Compiling slow32_vfs.c ==="
$CLANG $CFLAGS -O2 $SQLITE_OPTS "$SCRIPT_DIR/slow32_vfs.c" -o "$OUTDIR/slow32_vfs.ll"
$LLC -mtriple=slow32-unknown-none "$OUTDIR/slow32_vfs.ll" -o "$OUTDIR/slow32_vfs.s"
echo "  -> slow32_vfs.s"

# Step 3: Compile main.c
echo "=== Compiling main.c ==="
$CLANG $CFLAGS -O2 $SQLITE_OPTS "$SCRIPT_DIR/main.c" -o "$OUTDIR/main.ll"
$LLC -mtriple=slow32-unknown-none "$OUTDIR/main.ll" -o "$OUTDIR/main.s"
echo "  -> main.s"

# Step 4: Assemble
echo "=== Assembling ==="
for f in sqlite3 slow32_vfs main; do
    echo "  assembling $f.s ..."
    "$TOOLCHAIN/assembler/slow32asm" "$OUTDIR/$f.s" "$OUTDIR/$f.s32o"
done

# Step 5: Link
echo "=== Linking ==="
"$TOOLCHAIN/linker/s32-ld" --mmio 64K --code-size 2M \
    -o "$OUTDIR/sqlite3.s32x" \
    "$RUNTIME/crt0.s32o" \
    "$OUTDIR/main.s32o" \
    "$OUTDIR/slow32_vfs.s32o" \
    "$OUTDIR/sqlite3.s32o" \
    "$RUNTIME/libc_mmio.s32a" \
    "$RUNTIME/libs32.s32a"
echo "  -> $OUTDIR/sqlite3.s32x"

echo ""
echo "=== Build complete ==="
echo "Run with: $TOOLCHAIN/emulator/slow32-fast $OUTDIR/sqlite3.s32x"
