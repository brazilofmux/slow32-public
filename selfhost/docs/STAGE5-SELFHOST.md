# Stage 5: Compiling the Real Toolchain

## Goal

Use the stage 4 C compiler (running on SLOW-32 under emulation) to compile the real toolchain sources, then assemble and link them into working SLOW-32 executables. At the end of this stage, the full toolchain runs on SLOW-32.

## Build Sequence

### 1. Compile Runtime Libraries

The runtime provides `crt0`, libc, and compiler support routines.

```
crt0.s                      → crt0.s32o          (stage 2 assembler)
runtime/libc sources (.c)   → .s → .s32o         (stage 4 compiler + stage 2 assembler)
runtime/libs32 sources (.c) → .s → .s32o         (stage 4 compiler + stage 2 assembler)
*.s32o                      → libc.s32a          (archiver, or link directly)
*.s32o                      → libs32.s32a        (archiver, or link directly)
```

Key libc components needed:

- `malloc.c` / `free.c` — dynamic memory
- `printf.c` / `fprintf.c` — formatted output
- `string.c` — string functions (strlen, strcmp, etc.)
- `stdio.c` — file I/O (fopen, fread, etc.)
- `ctype.c` — character classification
- `stdlib.c` — atoi, strtol, qsort, exit
- `memory.c` — memcpy, memset

### 2. Compile Toolchain

```
tools/assembler/slow32asm.c  → slow32asm.s → slow32asm.s32o
common/s32_formats.h         (included by all)
common/s32_hashtable.h       (included by assembler and linker)

tools/linker/s32-ld.c        → s32-ld.s → s32-ld.s32o
tools/utilities/s32-ar.c     → s32-ar.s → s32-ar.s32o
tools/utilities/slow32dis.c  → slow32dis.s → slow32dis.s32o
tools/utilities/slow32dump.c → slow32dump.s → slow32dump.s32o
```

### 3. Link Each Tool

```
s32-ld crt0.s32o slow32asm.s32o libc.s32a libs32.s32a -o slow32asm.s32x
s32-ld crt0.s32o s32-ld.s32o    libc.s32a libs32.s32a -o s32-ld.s32x
s32-ld crt0.s32o s32-ar.s32o    libc.s32a libs32.s32a -o s32-ar.s32x
s32-ld crt0.s32o slow32dis.s32o libc.s32a libs32.s32a -o slow32dis.s32x
s32-ld crt0.s32o slow32dump.s32o libc.s32a libs32.s32a -o slow32dump.s32x
```

### 4. Build the C Compiler Itself

If the stage 4 compiler is written in C (Path B), compile it too:

```
cc.c → cc.s → cc.s32o → cc.s32x
```

If written in Forth (Path A), it runs in the Forth kernel and doesn't need compilation. However, rewriting it in C at this point enables the fixed-point proof in stage 6.

## Bootstrapping Order

Some tools must be built before others:

```
1. crt0.s32o          (assembled — no compiler needed)
2. libc objects        (compiled with stage 4 compiler)
3. libs32 objects      (compiled with stage 4 compiler)
4. slow32asm.s32x      (compiled and linked — can now self-assemble)
5. s32-ld.s32x         (compiled and linked — can now self-link)
6. s32-ar.s32x         (compiled and linked)
7. slow32dis.s32x      (compiled and linked)
8. slow32dump.s32x     (compiled and linked)
9. cc.s32x             (compiled and linked — if C-based compiler)
```

After step 5, the toolchain can rebuild itself: the SLOW-32-native assembler can assemble SLOW-32 assembly, and the SLOW-32-native linker can link the result.

## Verification Checks

### Binary Comparison

For each tool, compare the SLOW-32-compiled output against the LLVM-compiled output:

```bash
# Cross-compile with LLVM (reference)
clang -target slow32-unknown-none -S -emit-llvm -O0 slow32asm.c -o ref.ll
llc -mtriple=slow32-unknown-none ref.ll -o ref.s
slow32asm ref.s ref.s32o
s32-ld crt0.s32o ref.s32o libc.s32a libs32.s32a -o ref.s32x

# Self-compile on SLOW-32
emulator cc.s32x < slow32asm.c > self.s
emulator slow32asm.s32x self.s self.s32o
emulator s32-ld.s32x crt0.s32o self.s32o libc.s32a libs32.s32a -o self.s32x

# Both should produce identical output when run:
emulator ref.s32x < test.s > ref-output.txt
emulator self.s32x < test.s > self-output.txt
diff ref-output.txt self-output.txt
```

The binaries themselves may differ (different compilers produce different code), but their **behavior** must be identical.

### Functional Tests

Run the regression test suite using the self-hosted toolchain:

```bash
# Use self-hosted assembler + linker to build regression tests
for test in regression/tests/*.c; do
    emulator cc.s32x < "$test" > tmp.s
    emulator slow32asm.s32x tmp.s tmp.s32o
    emulator s32-ld.s32x crt0.s32o tmp.s32o libc.s32a libs32.s32a -o tmp.s32x
    emulator slow32 tmp.s32x > output.txt
    diff output.txt "regression/results/$(basename $test .c).expected"
done
```

## Potential Issues

1. **Performance**: The self-hosted toolchain runs under emulation, which is ~100x slower than native. Compiling the assembler (~2,500 lines) might take minutes. Acceptable for bootstrap but not for daily use.

2. **Memory**: The assembler and linker use dynamic arrays that grow with `realloc`. Ensure the emulator provides enough heap space (the linker's `--mmio 64K` provides MMIO; heap size depends on `mem_size` in the executable header).

3. **libc completeness**: The runtime libc must support all ~37 functions the toolchain calls. Missing functions will show up as linker errors.

4. **Preprocessor edge cases**: The toolchain uses `#include` with relative paths (`../../common/s32_formats.h`). The bootstrap compiler's preprocessor must handle path resolution.

## Dependencies

- **Stage 2**: Assembler (assembles compiler output)
- **Stage 3**: Linker (links everything together)
- **Stage 4**: C compiler (compiles the toolchain sources)
- Runtime library sources (already exist in `runtime/`)
