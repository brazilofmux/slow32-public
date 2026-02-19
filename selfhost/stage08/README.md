# Stage 08: Subset-C Compiler (cc-min)

## What Is Here

Two things: an archiver parity gate that validates Stage 02 archiver, and
`cc-min` -- the first C compiler written in C. This is where the
bootstrap turns recursive: a C compiler compiling C.

| File/Dir | Description |
|----------|-------------|
| `cc-min.c` | Main driver |
| `cc-min-pass1.c` | One-pass recursive descent compiler (tokenizer + parser + codegen) |
| `cc-min-pass2.c` | Pass 2: reserved for future validation |
| `cc-min-pass3.c` | Pass 3: write output buffer to file |
| `tests/` | 21 test programs |
| `run-regression.sh` | Runs both archiver parity and cc-min spike |
| `run-cc-spike.sh` | Builds and end-to-end tests cc-min |

## What It Needs

- Stage 00 emulator (`s32-emu`)
- Stage 04 compiler (`cc.fth`) to build cc-min itself
- Stage 01 assembler and Stage 03 linker (to build cc-min)
- Stage 02 assembler (to assemble cc-min's output)
- Stage 02 linker (to link cc-min's output)
- `libc/` from Stage 02

## What It Produces

`.s` assembly files from `.c` source files. cc-min is a one-pass
recursive descent compiler (SubC/Small-C style) that emits SLOW-32
assembly directly during parsing -- no AST, no separate IR.

The pipeline is: cc-min compiles `.c` to `.s`, Stage 02 assembles
`.s` to `.s32o`, Stage 02 links `.s32o` to `.s32x`, emulator runs it.

## How To Test

```bash
# Full regression (archiver parity + cc-min spike)
selfhost/stage08/run-regression.sh --emu ./tools/emulator/slow32-fast

# Just the compiler spike
selfhost/stage08/run-cc-spike.sh --emu ./tools/emulator/slow32-fast
```

## Roadmap to Self-Compilation

The goal: cc-min compiles cc-min. Five milestones, each building on the last.

### Milestone 1: Become a real compiler -- DONE

The foundational rewrite. cc-min is now a one-pass recursive descent compiler
with accumulator + stack codegen matching cc.fth's calling convention.

- General tokenizer (30+ token types)
- Symbol table (arbitrary variable/function names)
- Code generation (256-byte fixed frames, r3-r10 args, r29=SP, r30=FP)
- Multiple functions with arbitrary names
- Local `int` variables on the stack
- Function calls with arbitrary argument count
- `if`/`else`, `while`, `for` loops with branch codegen
- `break`/`continue`
- Full expression operators: `&&`, `||`, `!`, all comparisons, `%`, ternary `?:`
- `return` with expression

### Milestone 2: Pointers, arrays, and chars

After this, string processing works. Can compile `string_extra.c` and `convert.c`.

- `char` type (1-byte loads/stores) -- 11 files need this
- Pointer declarations and dereferencing (`*p`) -- 11 files
- Array declarations, global and local (`char buf[128]`) -- 7+ files
- Array indexing (`a[i]`) -- 11 files
- Pointer arithmetic (`p++`, `p + n`) -- 8 files
- String literals (stored in .data, address as `const char *`) -- 10 files
- Address-of operator (`&x`) -- 4 files
- Double pointers (`char **argv`) -- 8 files
- Bitwise operators: `&`, `|`, `~`, `<<`, `>>` -- 5-6 files each
- `++`/`--` as statements -- 9 files

### Milestone 3: Structs and typedefs

After this, can compile `stdio.c`, `s32-ar.c`, and `s32-ld.c`.

- `struct` definition -- 4 files
- `typedef` -- 4 files
- Member access `.` and `->` -- 4 files
- Arrays of structs -- 4 files
- `sizeof` -- 2 files
- Type casts (especially pointer casts like `(uint8_t *)buf`) -- 9 files

### Milestone 4: Preprocessor

After this, can compile the full toolchain as-is.

- `#include` (paste-include) -- 8 files
- `#define` object-like macros -- 7 files
- Anonymous `enum` -- 1 file (s32-as.c)

### Milestone 5: Self-compilation gate

cc-min compiles cc-min. The bootstrap is complete.

## Feature Audit

Comprehensive audit of all 12 selfhost toolchain source files. This tells us
exactly what cc-min needs to support.

| Feature | Files Using It |
|---------|---------------|
| `char` type | 11 |
| Pointer declarations | 11 |
| Array indexing (`a[i]`) | 11 |
| String literals | 10 |
| `++` increment | 9 |
| Type casts | 9 |
| Pointer dereferencing (`*p`) | 8 |
| Pointer arithmetic | 8 |
| Double pointer (`char **`) | 8 |
| `#include` | 8 |
| `#define` (object-like) | 7 |
| Global arrays | 7 |
| Local arrays | 5 |
| `struct` definition | 4 |
| `typedef` | 4 |
| Member access `.` | 4 |
| Address-of (`&`) | 4 |
| Member access `->` | 3 |
| `--` decrement | 3 |
| `sizeof` | 2 |
| Anonymous `enum` | 1 |

**Not used anywhere** (safe to omit): `switch`/`case`, `do...while`, `goto`,
`union`, `#ifdef`/`#ifndef`, function-like macros, varargs, function pointers,
string literal concatenation, struct initializers, array initializers, `extern`,
nested structs.

## cc.fth Constraints

These apply to all cc-min source code since cc.fth (stage01) compiles it:

- Merged source must be < 32768 bytes (INC-BUF-SZ limit)
- No expressions in array sizes: use `#define SZ 1024` then `arr[SZ]`
- No 2D arrays: use flat buffer with manual indexing
- No struct arrays with embedded char arrays: use parallel flat arrays
- Use `jal r31, <func>` not `call <func>` (stage02 linker HI20/LO12 limitation)
- No switch/case, varargs, function pointers, unions
