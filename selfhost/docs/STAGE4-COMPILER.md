# Stage 4: Minimal C Compiler

## Goal

A C compiler capable of compiling the SLOW-32 toolchain sources (~7,000 lines of C). This is the largest and most complex bootstrap stage. It reads C source and emits SLOW-32 assembly text (which the stage 2 assembler then converts to object files).

## Target: The C Subset Actually Used

Analysis of the toolchain sources (see [C-SUBSET.md](C-SUBSET.md)) reveals a manageable subset:

**Required:**

- Basic types: `int`, `char`, `uint8_t`/`uint16_t`/`uint32_t`, `int32_t`, `long`, `size_t`
- `struct` (nested, with pointers)
- `enum` (named and anonymous)
- `typedef`
- Pointers (single and double indirection, pointer arithmetic)
- Arrays (1D, with `[]` indexing)
- `for`, `while`, `do-while`, `if-else`, `switch-case`
- `break`, `continue`, `return`
- `#include`, `#define` (simple and function-like), `#ifdef`/`#ifndef`/`#if`/`#else`/`#endif`
- String literals, character constants
- Designated initializers (`{.field = value}`)
- `static` locals and functions
- `const` qualifier
- Cast expressions
- Ternary operator (`? :`)
- All arithmetic, bitwise, comparison, and logical operators
- `sizeof`

**Not required:**

- Floating point (`float`, `double`)
- `va_list` / varargs
- Function pointers
- Unions
- `goto`
- Bitfields
- Variable-length arrays (VLAs)
- Compound literals
- Complex designated initializers
- Variadic macros
- `long long` (64-bit integers) — toolchain uses `uint32_t` exclusively
- `volatile`, `register`, `restrict`

## Architecture

```
                    ┌──────────────┐
 C source ────────▶│ Preprocessor │
                    └──────┬───────┘
                           │ token stream (expanded)
                           ▼
                    ┌──────────────┐
                    │    Lexer     │
                    └──────┬───────┘
                           │ tokens
                           ▼
                    ┌──────────────┐
                    │   Parser     │  Recursive descent
                    └──────┬───────┘
                           │ AST (or direct emit)
                           ▼
                    ┌──────────────┐
                    │  Code Gen    │  Direct assembly emission
                    └──────┬───────┘
                           │
                           ▼
                    SLOW-32 assembly text (.s)
```

### Preprocessor

A minimal preprocessor handling:

| Feature | Needed By | Notes |
|---------|-----------|-------|
| `#include "file"` | All tools | Relative path resolution |
| `#include <file>` | All tools | System include path |
| `#define NAME value` | All tools | Simple object-like macros |
| `#define NAME(args) body` | Assembler, linker | Function-like macros |
| `#ifdef` / `#ifndef` | Headers | Include guards |
| `#if` / `#elif` / `#else` / `#endif` | Headers | Conditional compilation |
| `#pragma pack` | s32_formats.h | Can be ignored (natural alignment OK for SLOW-32) |
| `#undef` | Rare | Simple removal |

**Not needed:** `#pragma once`, `#line`, `#error`, variadic macros (`__VA_ARGS__`), token pasting (`##`), stringification (`#`).

### Lexer

Standard C tokenizer producing:

- Keywords: `int`, `char`, `void`, `struct`, `enum`, `typedef`, `if`, `else`, `while`, `for`, `do`, `switch`, `case`, `default`, `break`, `continue`, `return`, `static`, `const`, `sizeof`, `unsigned`, `signed`, `long`, `short`
- Identifiers
- Integer literals (decimal, hex `0x`, octal `0`)
- String literals (with escape sequences: `\n`, `\t`, `\\`, `\"`, `\0`, `\x##`)
- Character constants
- Operators and punctuation

### Parser

Recursive descent, producing either an AST or directly emitting code. Key grammar productions:

```
program         → (declaration | function_def)*
declaration     → type_spec declarator ('=' initializer)? ';'
function_def    → type_spec declarator '(' params ')' compound_stmt
compound_stmt   → '{' (declaration | statement)* '}'
statement       → if_stmt | while_stmt | for_stmt | do_stmt
                | switch_stmt | return_stmt | break | continue
                | expr_stmt | compound_stmt
expression      → assignment
assignment      → ternary (('=' | '+=' | '-=' | ...) assignment)?
ternary         → logical_or ('?' expression ':' ternary)?
logical_or      → logical_and ('||' logical_and)*
...down to primary → number | string | ident | '(' expr ')'
```

### Code Generator

Emits SLOW-32 assembly targeting the standard calling convention:

```
Arguments:  r3-r10 (a0-a7)
Return:     r1 (rv)
Temporaries: r2 (t0)
Callee-saved: r11-r28 (s0-s17)
Stack pointer: r29 (sp)
Frame pointer: r30 (fp)
Link register: r31 (lr)
```

**Function prologue:**
```asm
    addi r29, r29, -FRAME_SIZE
    stw  r29, r31, FRAME_SIZE-4     # save LR
    stw  r29, r30, FRAME_SIZE-8     # save FP
    addi r30, r29, FRAME_SIZE       # set FP
    # save callee-saved registers as needed
```

**Function epilogue:**
```asm
    # restore callee-saved registers
    ldw  r30, r29, FRAME_SIZE-8     # restore FP
    ldw  r31, r29, FRAME_SIZE-4     # restore LR
    addi r29, r29, FRAME_SIZE
    jalr r0, r31, 0                 # return
```

**Register allocation strategy:** Simple — use a small set of temporaries and spill everything else to the stack. This produces slow but correct code. The real toolchain uses LLVM's register allocator for performance; the bootstrap compiler just needs correctness.

## Implementation Language

Two options:

### Option A: Written in Forth (~3,000-5,000 lines)

Pros:

- Can be loaded directly into the stage 1 kernel
- Forth is well-suited to parsing tasks
- No chicken-and-egg problem

Cons:

- Writing a C compiler in Forth is unconventional
- Debugging is harder without a debugger

### Option B: Written in SLOW-32 Assembly (~5,000-8,000 lines)

Pros:

- Full control over generated code
- Assembled by stage 2

Cons:

- Much more verbose
- Harder to modify and debug

**Recommendation:** Option A (Forth). The existing Forth kernel has all the infrastructure needed — file I/O, string handling, dictionary for symbol tables.

## libc Functions Required

The compiler must emit calls to these libc functions, which must be available as linkable object files:

### Memory (4 functions)

- `malloc(size)` → pointer
- `calloc(count, size)` → pointer
- `realloc(ptr, size)` → pointer
- `free(ptr)`

### String (10 functions)

- `strlen`, `strcmp`, `strncmp`
- `strcpy`, `strncpy`
- `snprintf`
- `strdup`
- `strrchr`, `strchr`
- `memcpy`, `memset`

### I/O (9 functions)

- `fopen`, `fclose`
- `fread`, `fwrite`
- `fseek`, `ftell`
- `printf`, `fprintf`
- `perror`

### Character (6 functions)

- `isspace`, `isdigit`, `isalpha`, `isalnum`, `isprint`
- `tolower`

### Numeric (5 functions)

- `atoi`
- `strtol`, `strtoul`
- `strtoll`, `strtoull`

### Other (3 functions)

- `qsort`
- `exit`
- `access` (used by archiver only — can be stubbed)

Total: ~37 libc functions. All of these are already implemented in the SLOW-32 runtime library.

## Complexity Analysis

| Component | Lines (Forth est.) | Difficulty |
|-----------|--------------------|------------|
| Preprocessor | ~400 | Moderate |
| Lexer | ~300 | Easy |
| Parser | ~1,200 | Hard |
| Type system | ~400 | Moderate |
| Code generator | ~800 | Moderate |
| Symbol tables | ~200 | Easy |
| String/constant handling | ~200 | Easy |
| Driver (main, file handling) | ~100 | Easy |
| **Total** | **~3,600** | |

For reference, well-known minimal C compilers:

- **cc500**: ~500 lines (extremely minimal subset)
- **chibicc**: ~5,000 lines (nearly complete C11)
- **tcc**: ~30,000 lines (full C99 + extensions)
- **8cc**: ~6,000 lines (C11 subset)

Our target is closer to chibicc's scope.

## What the Compiler Does NOT Need

1. **Optimization** — The toolchain runs under emulation; correct code is all that matters. No register allocation beyond simple spilling, no dead code elimination, no constant folding (though constant folding is easy and useful for `sizeof` expressions).

2. **Full C standard compliance** — Only the subset used by the toolchain. No need for `long double`, complex numbers, `_Generic`, `_Atomic`, or most of C11/C17.

3. **Separate compilation model** — The compiler can process one translation unit at a time. The linker (stage 3) handles symbol resolution across files.

4. **Debug information** — Not needed for bootstrap.

## Testing Strategy

1. **Unit tests**: Compile small C programs, verify assembly output
2. **Integration test**: Compile a known-good C program (like a simple regression test), assemble, link, run, compare output
3. **Self-compilation test**: Use the compiler to compile itself (if written in C for Path B)
4. **Toolchain test**: Compile `slow32asm.c`, assemble and link, verify the resulting assembler produces correct output

## Dependencies

- **Stage 1**: Forth kernel (execution environment)
- **Stage 2**: Assembler (assembles compiler output)
- **Stage 3**: Linker (links compiler output with runtime)
- **C-SUBSET.md**: Exact feature requirements
