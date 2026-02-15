# SLOW-32 Self-Hosting Bootstrap Plan (V2)

This document supersedes [BOOTSTRAP.md](BOOTSTRAP.md) (retained as historical reference).

## Philosophy

Three principles guide this plan:

1. **Each stage is narrow.** One new tool or one new capability. Specific prerequisites, specific outputs, specific verification.
2. **Three self-hosting layers.** Forth rebuilds itself. Subset C rebuilds itself. Full C rebuilds itself. Each layer proves something different.
3. **Trust is auditable.** The only opaque binary is the Stage 0 emulator (~780 lines of C). Everything else is built from inspectable source.

### Why Fine-Grained Stages

The classic bootstrap approach squashes everything into 2-3 stages. That works for describing the process in a paper. It's terrible for actually doing the work.

Fine-grained stages give you:

- **Clear checkpoints.** Each stage has pass/fail criteria. You know when you're done.
- **Smaller debugging surface.** When something breaks, you know which stage introduced it.
- **Visible progress.** "Stage 7 of 16" is more motivating than "somewhere in Stage 2."
- **Natural parallelism.** Independent stages can be worked on concurrently.

### Why Three Self-Hosting Layers

Each layer answers a different question:

- **Forth self-hosting** (Stages 1-3): Do the assembler, archiver, and linker actually work? Can they rebuild the kernel that hosts them? This is cheap to verify and catches tool bugs early.
- **Subset C self-hosting** (Stages 4-9): Does the C compiler work? Can it compile itself? This is the critical layer — it transitions the entire toolchain from Forth to C.
- **Full C self-hosting** (Stages 10-13): Can the compiler handle real-world C? Varargs, function pointers, complex expressions? This proves the toolchain is practical, not just a toy.
- **Optimization** (Stages 14-16): Can we make it fast? This is optional but satisfying.

None of these layers is redundant. Forth self-hosting is a prerequisite for building the C compiler. Subset C self-hosting eliminates the Forth dependency. Full C self-hosting eliminates the subset restriction. Each layer stands on the previous one.


## Overview

```
Layer A: Foundation
  Stage 0:  Minimum bootstrap emulator (host C)

Layer B: Forth Self-Hosting
  Stage 1:  Forth assembler         .s → .s32o
  Stage 2:  Forth archiver          .s32o → .s32a
  Stage 3:  Forth linker            .s32o + .s32a → .s32x
            ✓ CHECKPOINT: Forth can rebuild its own kernel

Layer C: Subset C Self-Hosting
  Stage 4:  Subset C compiler       .c → .s    (written in Forth)
  Stage 5:  Subset C assembler      .s → .s32o (written in C, replaces Stage 1)
  Stage 6:  Subset C archiver       .s32o → .s32a (written in C, replaces Stage 2)
  Stage 7:  Subset C linker         .s32o + .s32a → .s32x (written in C, replaces Stage 3)
  Stage 8:  Subset C compiler in C  .c → .s    (written in C, replaces Stage 4)
  Stage 9:  Subset C fixed-point    Gen2 == Gen3
            ✓ CHECKPOINT: Entire toolchain is C. Forth no longer needed.

Layer D: Full C Self-Hosting
  Stage 10: Full emulator           enhanced Stage 0 (or self-hosted emulator)
  Stage 11: Full C compiler         extend compiler with advanced features
  Stage 12: Full C toolchain        rebuild everything with full compiler
  Stage 13: Full C fixed-point      Gen2 == Gen3
            ✓ CHECKPOINT: Full C toolchain is self-hosting.

Layer E: Optimization
  Stage 14: Optimizing compiler     add optimization passes
  Stage 15: Optimized toolchain     rebuild everything optimized
  Stage 16: Final fixed-point       Gen2 == Gen3
            ✓ CHECKPOINT: Optimized toolchain is self-hosting. Bootstrap complete.
```

### Stage Summary

| Stage | Name | Language | Lines (est.) | Builds On |
|-------|------|----------|-------------|-----------|
| 0 | Emulator | C (host) | ~780 | Host C compiler |
| 1 | Assembler | Forth | ~1,300 | Stage 0 + kernel.s32x |
| 2 | Archiver | Forth | ~735 | Stages 0-1 |
| 3 | Linker | Forth | ~1,100 | Stages 0-2 |
| 4 | C Compiler | Forth | ~4,100 | Stages 0-3 |
| 5 | C Assembler | Subset C | ~1,500 | Stages 0-4 |
| 6 | C Archiver | Subset C | ~800 | Stages 0-5 |
| 7 | C Linker | Subset C | ~2,500 | Stages 0-6 |
| 8 | C Compiler (in C) | Subset C | ~5,000 | Stages 0-7 |
| 9 | Fixed-point | (verification) | — | Stage 8 |
| 10 | Full Emulator | C | ~1,500 | Stages 0-9 |
| 11 | Full Compiler | C | ~7,000 | Stages 0-10 |
| 12 | Full Toolchain | C | (rebuild) | Stages 0-11 |
| 13 | Fixed-point | (verification) | — | Stage 12 |
| 14 | Optimizer | C | ~2,000+ | Stages 0-13 |
| 15 | Optimized Toolchain | C | (rebuild) | Stage 14 |
| 16 | Fixed-point | (verification) | — | Stage 15 |


---

## Layer A: Foundation

### Stage 0: Minimum Bootstrap Emulator

The trust root. Everything else runs on top of this.

- **You need:**

  - A C compiler on the host machine (gcc, clang, tcc — anything).
  - The `s32-emu.c` source (~780 lines of C, POSIX-only, no external dependencies).

- **You produce:**

  - `s32-emu`: a minimal emulator that loads `.s32x` binaries and executes them.

- **It handles:**

  - All 34 integer instructions (arithmetic, logic, comparison, memory, control flow).
  - Legacy MMIO: PUTCHAR, GETCHAR, BRK, EXIT.
  - MMIO ring buffer I/O: file operations, command-line args, environment.
  - `.s32x` header parsing and section loading.

- **It does NOT handle:**

  - Debugging features (trace, breakpoints, step).
  - Service negotiation protocol.
  - Performance optimization.

- **Verification:**

  - Run known-good `.s32x` binaries (from the existing toolchain).
  - Compare output character-for-character with the reference emulators (`slow32`, `slow32-fast`).


---

## Layer B: Forth Self-Hosting

The Forth kernel is the interactive programming environment that hosts Stages 1-4. You need a working `kernel.s32x` binary — either the pre-built one from the existing toolchain, or one hand-assembled from source.

Using the pre-built kernel is fine for bootstrapping. After Stage 3, you can rebuild it from source, proving the binary matches.

### Stage 1: Forth Assembler

- **You need:**

  - Stage 0 emulator.
  - Pre-built `forth/kernel.s32x`.
  - `forth/prelude.fth` (standard library definitions).
  - `selfhost/stage2/asm.fth` (~1,300 lines of Forth).

- **You produce:**

  - The ability to assemble `.s` files into `.s32o` object files.
  - Two-pass assembly with labels, directives, pseudo-instructions.
  - Symbol tables and relocation entries in the output.

- **Pipeline:**

  ```
  cat prelude.fth asm.fth commands.fth | s32-emu kernel.s32x
  ```

  Where `commands.fth` contains: `S" input.s" S" output.s32o" ASSEMBLE-FILE BYE`

- **You can now:**

  - Convert SLOW-32 assembly source to linkable object files — without the host assembler.

- **Verification:**

  - Assemble test programs (`test0.s` through `test3.s`).
  - Compare `.s32o` output with reference assembler (`slow32asm`).
  - Disassemble and verify instruction encodings.


### Stage 2: Forth Archiver

- **You need:**

  - Stage 0 emulator.
  - Pre-built `kernel.s32x`.
  - `selfhost/stage3/ar.fth` (~735 lines of Forth).
  - Object files from Stage 1.

- **You produce:**

  - The ability to create `.s32a` archive files from `.s32o` object files.
  - Archive format with symbol index, member table, member data.

- **You can now:**

  - Bundle object files into libraries (e.g., assemble runtime `.s` files and archive them into `libs32.s32a`).

- **Verification:**

  - Create archives, list contents, extract members.
  - Compare with reference archiver (`s32-ar`).


### Stage 3: Forth Linker

- **You need:**

  - Stage 0 emulator.
  - Pre-built `kernel.s32x`.
  - `selfhost/stage3/link.fth` (~1,100 lines of Forth).
  - Object files and archives from Stages 1-2.

- **You produce:**

  - The ability to link `.s32o` + `.s32a` into `.s32x` executables.
  - Section merging (`.text`, `.data`, `.bss`, `.rodata`).
  - Symbol resolution across multiple object files and archives.
  - Relocation application (HI20, LO12, BRANCH, JAL, PCREL).
  - Memory layout: code at 0x0, data at 0x100000, stack at 0x0FFFFFF0.
  - Linker-injected symbols (`__bss_start`, `__heap_start`, `__mmio_base`, etc.).

- **You can now:**

  - Produce runnable `.s32x` executables from assembly source — without any host tools.

- **Verification:**

  - Link and run test programs on Stage 0 emulator.
  - Compare linked executables with reference linker (`s32-ld`).


### Checkpoint: Forth Self-Hosted

At this point, the Forth toolchain can rebuild itself:

1. Assemble `forth/kernel.s` into `kernel.s32o` (Stage 1).
2. Assemble `runtime/crt0.s` and selected runtime `.s` files (Stage 1).
3. Archive runtime objects into `libs32.s32a` (Stage 2).
4. Link into `kernel.s32x` (Stage 3).

The rebuilt kernel should be functionally identical to the pre-built one. If bit-identical — great. If not, functional equivalence (same outputs on same inputs) is sufficient, since the Forth compiler and reference assembler may produce semantically equivalent but non-identical encodings.

This checkpoint validates the entire Forth toolchain before you stake the C compiler on it.


---

## Layer C: Subset C Self-Hosting

This is the critical layer. It transitions the toolchain from Forth to C. By the end, Forth is no longer needed — the entire toolchain is written in C and can rebuild itself.

### Stage 4: Subset C Compiler (in Forth)

The compiler is written in Forth, runs on the Forth kernel, and compiles a subset of C to SLOW-32 assembly.

- **You need:**

  - Stage 0 emulator.
  - Stages 1-3 (Forth assembler, archiver, linker).
  - Pre-built `kernel.s32x`.
  - `selfhost/stage4/cc.fth` (~4,100 lines of Forth).
  - Runtime library sources: `crt0.s` (assembly) + libc sources (subset C or assembly).
  - Subset C header files (`selfhost/stage4/include/`).

- **You produce:**

  - A C compiler: `.c` source → `.s` SLOW-32 assembly.
  - A working runtime library: `crt0.s32o` + `libc.s32a`.
  - The full compilation pipeline: `.c` → `.s` → `.s32o` → `.s32x` → run.

- **Subset C supports:**

  - Types: `int`, `char`, `unsigned`, `signed`, `short`, `long`, `void`, `struct`, `enum`, `typedef`, `bool`.
  - Pointers, arrays (single and multi-dimensional), pointer arithmetic.
  - Control flow: `if`/`else`, `while`, `for`, `do`/`while`, `switch`/`case`/`default`, `break`, `continue`, `return`.
  - All arithmetic, logical, bitwise, comparison, assignment, ternary, comma operators.
  - Preprocessor: `#include`, `#define` (simple + function-like), `#ifdef`/`#ifndef`/`#if`/`#endif`, `#pragma pack`.
  - String and character literals.
  - `sizeof`, `static`, `const`, `extern`.

- **Subset C does NOT need:**

  - Floating point (`float`, `double`).
  - Varargs (`va_list`, `va_start`, `va_arg`).
  - Function pointers.
  - Unions.
  - `goto`.
  - Bitfields, VLAs, 64-bit integers, complex initializers.

- **Runtime library note:**

  The runtime for this stage is minimal. `crt0.s` is assembly (assembled by Stage 1). The C portion of the libc must be written in subset C — no varargs, no function pointers. For I/O, this likely means a simplified `printf` with a fixed interface (e.g., `print_int`, `print_str`, `print_hex`) rather than full `printf(const char *fmt, ...)`. File I/O through MMIO wrappers.

  See [C-SUBSET.md](C-SUBSET.md) for the detailed analysis of what the toolchain actually uses.

- **You can now:**

  - Compile C programs.

- **Verification:**

  - Compile and run the test suite (`test1.c` through `test9.c`).
  - Compare output with expected results.
  - Run `selfhost/stage4/run-regression.sh`.


### Stage 5: Subset C Assembler

The first C tool. Written in subset C, compiled by the Forth-based compiler, replaces the Forth assembler.

- **You need:**

  - Stages 0-4.
  - Assembler source written in subset C (`s32-as.c`, ~1,500 lines).

- **You produce:**

  - `s32-as.s32x`: a C-based assembler that reads `.s` files and produces `.s32o` object files.

- **Build pipeline (uses Forth tools behind the scenes):**

  ```
  Stage 4 cc.fth compiles s32-as.c → s32-as.s
  Stage 1 asm.fth assembles s32-as.s → s32-as.s32o
  Stage 3 link.fth links s32-as.s32o + libc.s32a → s32-as.s32x
  ```

- **You can now:**

  - Assemble `.s` → `.s32o` without Forth.
  - The C assembler replaces the Forth assembler in the pipeline for subsequent stages.

- **Verification:**

  - Assemble the same test programs with both Forth and C assemblers.
  - Output `.s32o` files should be bit-identical (or functionally equivalent if encoding differs).


### Stage 6: Subset C Archiver

- **You need:**

  - Stages 0-5.
  - Archiver source written in subset C (`s32-ar.c`, ~800 lines).

- **You produce:**

  - `s32-ar.s32x`: a C-based archiver.

- **Build pipeline (progressive — uses C assembler from Stage 5):**

  ```
  Stage 4 cc.fth compiles s32-ar.c → s32-ar.s
  Stage 5 s32-as.s32x assembles → s32-ar.s32o
  Stage 3 link.fth links → s32-ar.s32x
  ```

- **You can now:**

  - Create `.s32a` archives without Forth. The C archiver replaces the Forth archiver.

- **Verification:**

  - Create the same archives with both archivers, compare output.


### Stage 7: Subset C Linker

- **You need:**

  - Stages 0-6.
  - Linker source written in subset C (`s32-ld.c`, ~2,500 lines).

- **You produce:**

  - `s32-ld.s32x`: a C-based linker.

- **Build pipeline (progressive — uses C assembler and archiver):**

  ```
  Stage 4 cc.fth compiles s32-ld.c → s32-ld.s
  Stage 5 s32-as.s32x assembles → s32-ld.s32o
  Stage 6 s32-ar.s32x archives libc (if rebuilding)
  Stage 3 link.fth links → s32-ld.s32x   (last use of Forth linker)
  ```

- **You can now:**

  - Link `.s32o` + `.s32a` → `.s32x` without Forth.
  - The entire assembler → archiver → linker pipeline is now C.
  - Forth is still needed ONLY for the compiler (Stage 4 cc.fth).

- **Verification:**

  - Link the same programs with both linkers, compare output.
  - Run the linked programs, verify identical behavior.


### Stage 8: Subset C Compiler (in C)

The transition point. The C compiler is rewritten in subset C and compiled by the Forth-based compiler. After this stage, Forth is no longer needed for anything.

- **You need:**

  - Stages 0-7.
  - C compiler source written in subset C (`cc.c` or multiple files, ~5,000 lines).
  - This is the most complex piece of code in the bootstrap.

- **You produce:**

  - **Gen1**: `cc.s32x` — a C-based C compiler, compiled by the Forth compiler.

- **Build pipeline:**

  ```
  Stage 4 cc.fth compiles cc.c → cc.s        (Forth compiles C compiler)
  Stage 5 s32-as.s32x assembles → cc.s32o
  Stage 7 s32-ld.s32x links → cc.s32x        (Gen1)
  ```

- **You can now:**

  - Compile C programs without Forth. Gen1 replaces the Forth-based compiler.
  - The entire toolchain (assembler, archiver, linker, compiler) is written in C.
  - The Forth kernel is no longer needed in the pipeline.

- **Verification:**

  - Gen1 should compile the test suite and produce identical results to the Forth compiler.
  - Gen1 should be able to compile the assembler, archiver, and linker sources.
  - Gen1 compiles itself (see Stage 9).


### Stage 9: Subset C Fixed-Point

Proof that the C compiler is correct and self-consistent.

- **You need:**

  - Stage 8 (Gen1 compiler).

- **Process:**

  1. **Gen1** (compiled by Forth) compiles `cc.c` → `cc.s32x` = **Gen2**.
  2. **Gen2** compiles `cc.c` → `cc.s32x` = **Gen3**.
  3. Verify: **Gen2 == Gen3** (bit-identical).

- **Why Gen1 != Gen2:**

  Gen1 was compiled by the Forth compiler. Gen2 was compiled by Gen1 (the C compiler). Different compilers produce different machine code — different register allocation, instruction selection, etc. This is expected and correct.

- **Why Gen2 == Gen3:**

  Gen2 and Gen3 are both compiled by the same compiler (Gen1 and Gen2 respectively), from the same source. If Gen2 != Gen3, there's a bug in the compiler — it doesn't consistently compile its own source code.

- **Also verify:**

  - Rebuild the entire toolchain with Gen2.
  - Run the full test suite.
  - Compare outputs with the Forth-built toolchain.


### Checkpoint: Subset C Self-Hosted

The entire toolchain is written in C, runs on SLOW-32, and can rebuild itself from source. The bootstrap chain is:

```
Host C compiler → Stage 0 emulator → kernel.s32x → Forth tools → Gen1 C tools → Gen2 C tools (= Gen3)
```

At this point, you can throw away the Forth kernel and tools. The C toolchain is self-sustaining. The only external dependency is the Stage 0 emulator (~780 lines of C on the host).


---

## Layer D: Full C Self-Hosting

The subset C toolchain is functional but limited. Real-world C programs use varargs, function pointers, unions, and other features the subset compiler doesn't handle. This layer extends the compiler to handle them.

### Stage 10: Full Emulator

The Stage 0 emulator is deliberately minimal — enough for Forth and subset C toolchain work. But real-world C programs need more. Try porting SLOW BASIC to the minimal emulator and you'll feel every missing piece.

- **You need:**

  - Stages 0-9 (working subset C toolchain).

- **What the full emulator adds over Stage 0:**

  - **Floating point instructions.** Software float routines in libc (`strtod`, `dtoa`, math functions) are pure integer code, but the compiler may need to handle `float`/`double` types, and programs like SLOW BASIC use floating point pervasively. The emulator needs to execute any FP instructions the compiler emits (even if they're just "move bits around" operations for soft-float calling conventions).
  - **Complete MMIO ring buffer coverage.** Stage 0 handles the basics (open, read, write, close, stat, brk, exit). Full programs need the complete set: seek, truncate, rename, unlink, mkdir, getcwd, environment access, and correct edge-case behavior (partial reads, error returns, etc.).
  - **Service negotiation protocol.** Dynamic MMIO service registration (opcodes 0xF0-0xF4). Needed for terminal services and any future extensions.
  - **Larger memory and robust bounds checking.** The subset toolchain fits comfortably in default memory. A full C compiler compiling large programs (MY-BASIC is ~10K lines) needs more heap, more stack, and the emulator should catch out-of-bounds accesses instead of silently corrupting.
  - **Debugging features.** Trace mode, breakpoints, register display, memory watchpoints. Not strictly required for self-hosting, but debugging full C programs without them is painful.

- **You produce:**

  - An emulator capable of running any SLOW-32 program — not just toolchain components, but real applications.

- **Two paths:**

  1. **Enhance Stage 0 on the host.** Edit `s32-emu.c` by hand. Simplest approach. The emulator stays a host-native program. Grows from ~780 to perhaps ~1,500 lines.
  2. **Build an emulator with the subset C toolchain.** Write it in subset C, compile it, run it on Stage 0. Proves the toolchain handles a non-trivial program. The result is a SLOW-32 binary that emulates SLOW-32 (metacircular, slow, but auditable).

  Path 1 is practical. Path 2 is pedagogically interesting. Either works. Both can be done.

  There's also a **Path 3** to dream about: a self-hosted DBT. A SLOW-32 dynamic binary translator, written in C, compiled by the self-hosted toolchain, running on Stage 0. It wouldn't translate SLOW-32 to x86 (it's running on SLOW-32), but it could do block-level optimization: trace hot paths, eliminate redundant loads/stores, inline dispatch. A SLOW-32-to-SLOW-32 JIT. Absurd and wonderful. And a serious validation that the full C compiler can handle the kind of pointer-heavy, bit-twiddling code a DBT requires.

- **You can now:**

  - Run full C programs: SLOW BASIC, MY-BASIC, or anything that uses floats, complex I/O, or larger memory.
  - Develop and debug the full C compiler (Stage 11) in a capable environment.

- **Verification:**

  - Run the full regression suite (all 23+ tests).
  - Run SLOW BASIC test suite (29 tests) or equivalent complex workload.
  - Compare behavior with reference emulators (`slow32`, `slow32-fast`).


### Stage 11: Full C Compiler

Extend the subset C compiler to handle the full language features needed for real-world programs.

- **You need:**

  - Stages 0-10.
  - Extended compiler source (evolves from `cc.c`).

- **New features to add:**

  - **Varargs:** `va_list`, `va_start`, `va_arg`, `va_end`. Needed for real `printf`.
  - **Function pointers:** Declaration, assignment, calling through pointers. Needed for callbacks, qsort, etc.
  - **Unions.**
  - **Full preprocessor:** `#if` constant expressions, macro expansion with arguments, stringification (`#`), token pasting (`##`).
  - **Wider type support:** Possibly 64-bit integers, `size_t`, complete type promotion rules.
  - **Designated initializers** for arrays (not just structs).

- **You can now:**

  - Compile programs that use standard C idioms.
  - Compile the existing toolchain sources (`slow32asm.c`, `s32-ld.c`, etc.) without modification.

- **Verification:**

  - Compile and run complex programs (MY-BASIC, SLOW BASIC, or equivalent).
  - Compile the existing toolchain source code.
  - Pass the full regression test suite.


### Stage 12: Full C Toolchain

Rebuild the entire toolchain with the full C compiler.

- **You need:**

  - Stage 11 (full C compiler).

- **You produce:**

  - Full C versions of: assembler, archiver, linker, compiler.
  - Full runtime library with real `printf`, `stdio`, `stdlib`, `string`, etc.
  - The compiler compiles itself.

- **You can now:**

  - Compile essentially any C program that fits the SLOW-32 memory model.
  - The toolchain handles its own source code natively.

- **Verification:**

  - All tools produce correct output.
  - Full regression suite passes.
  - Can compile and run the existing example programs.


### Stage 13: Full C Fixed-Point

- **Process:**

  1. Full C compiler compiles itself → Gen2.
  2. Gen2 compiles itself → Gen3.
  3. Verify: **Gen2 == Gen3**.

- **Also verify:**

  - Rebuild all tools with Gen2, verify they work.
  - Compare behavior with LLVM-compiled toolchain (semantic equivalence).


### Checkpoint: Full C Self-Hosted

The full C toolchain is self-hosting. It can compile real-world C programs, including itself. The bootstrap chain is fully auditable from Stage 0 emulator through this point.


---

## Layer E: Optimization

Optional but rewarding. An unoptimized compiler produces correct but slow code (accumulator model, no register allocation, no constant folding). Adding optimization passes makes the compiled programs — including the compiler itself — significantly faster.

### Stage 14: Optimizing Compiler

- **You need:**

  - Stages 0-13 (full C toolchain).

- **You produce:**

  - A C compiler with optimization passes. Candidates:
    - **Constant folding and propagation.** Evaluate constant expressions at compile time.
    - **Dead code elimination.** Remove unreachable code and unused variables.
    - **Register allocation.** Move beyond the accumulator model; use available registers.
    - **Common subexpression elimination.** Avoid recomputing the same expression.
    - **Peephole optimization.** Pattern-match and simplify instruction sequences.
    - **Strength reduction.** Replace expensive operations (multiply) with cheaper ones (shift).
    - **Inlining** of small functions.

- **You can now:**

  - Produce faster code.

- **Verification:**

  - Optimized programs must produce **identical results** to unoptimized (same inputs → same outputs).
  - Run the full test suite at each optimization level.
  - Benchmark to measure improvement.


### Stage 15: Optimized Toolchain

- **You need:**

  - Stage 14 (optimizing compiler).

- **You produce:**

  - Rebuild the entire toolchain with optimization enabled.
  - Everything runs faster: compiler, assembler, linker, archiver.

- **Verification:**

  - All tools still function correctly.
  - Measurable performance improvement on benchmarks.


### Stage 16: Final Fixed-Point

- **Process:**

  1. Optimizing compiler compiles itself (with optimization) → Gen2.
  2. Gen2 compiles itself → Gen3.
  3. Verify: **Gen2 == Gen3**.

- **This is the hardest fixed-point.** Optimizer bugs often manifest as incorrect self-compilation. If Gen2 == Gen3, the optimizer is self-consistent.

- **Also verify:**

  - Rebuild all tools with Gen2, run full test suite.
  - Cross-verify with LLVM-compiled binaries (semantic equivalence, not bit-equality).
  - Optionally: Diverse Double-Compiling (DDC) — compile with an independent compiler and compare.


### Checkpoint: Bootstrap Complete

The optimizing, self-hosting SLOW-32 C toolchain is verified correct and self-consistent. The full bootstrap chain:

```
Host C compiler
  → Stage 0 emulator (780 lines C, the only trust root)
    → Forth kernel (auditable assembly source)
      → Forth tools (auditable Forth source)
        → Subset C tools (auditable C source, fixed-point proven)
          → Full C tools (auditable C source, fixed-point proven)
            → Optimized C tools (auditable C source, fixed-point proven)
```

Every component above Stage 0 is built from source you can read. The fixed-point tests at Stages 9, 13, and 16 prove each layer is self-consistent. Per Ken Thompson's "Reflections on Trusting Trust," the only component that must be taken on faith is the Stage 0 emulator — and at ~780 lines of straightforward C, it's small enough to audit by hand.


---

## Trust Model

```
                    TRUST BOUNDARY
                         │
    Host machine         │    SLOW-32 world
                         │
    gcc/clang ──────────►│
         │               │
    s32-emu.c ──────────►│──► s32-emu (the only opaque binary)
         (~780 LOC)      │         │
                         │         ▼
                         │    kernel.s32x  ◄── kernel.s (auditable)
                         │         │
                         │         ▼
                         │    Forth tools   ◄── *.fth (auditable)
                         │         │
                         │         ▼
                         │    C tools       ◄── *.c (auditable, fixed-point proven)
                         │
```

The trust boundary is crossed exactly once: compiling `s32-emu.c` with the host compiler. After that, everything is built from auditable source within the SLOW-32 world. The fixed-point tests prove the tools are self-consistent (no hidden backdoors in the compiler output).


---

## Progressive Tool Replacement

Stages 5-8 progressively replace Forth tools with C tools. At each stage, the newly built C tool takes over from its Forth equivalent:

| Stage | Compiler | Assembler | Archiver | Linker |
|-------|----------|-----------|----------|--------|
| 4 | Forth (cc.fth) | Forth (asm.fth) | Forth (ar.fth) | Forth (link.fth) |
| 5 | Forth | **C (s32-as)** | Forth | Forth |
| 6 | Forth | C | **C (s32-ar)** | Forth |
| 7 | Forth | C | C | **C (s32-ld)** |
| 8 | **C (cc)** | C | C | C |

Each replacement is verified by comparing output with the Forth version. Once all four are C, the Forth kernel is no longer in the pipeline.

An alternative approach is to build ALL C tools using the Forth toolchain and then switch to the all-C toolchain at once. This is more conservative (each C tool is verified against Forth independently) but loses the satisfaction of watching the transition happen incrementally.


---

## Directory Mapping

The V2 stage numbers differ from the current source directory layout (which still reflects older V1 naming). Directory restructuring is intentionally deferred until the V2 stage boundaries are stable in code, tests, and CI scripts.

Execution artifacts for the reorg are tracked in:

- `selfhost/V2-REORG-PLAN.md`
- `selfhost/V2-MIGRATION-MAP.tsv`
- `selfhost/v2/` (target stage directories)

| V2 Stage | Current Directory | Key Files | Status |
|----------|-------------------|-----------|--------|
| 0 | `selfhost/stage0/` | `s32-emu.c` | Done |
| 1 | `selfhost/stage2/` | `asm.fth` | Done |
| 2 | `selfhost/stage3/` | `ar.fth` | Done |
| 3 | `selfhost/stage3/` | `link.fth` | Done |
| 4 | `selfhost/v2/stage04/` (`selfhost/stage4/` compat links) | `cc.fth` | Done (regression passing) |
| 5 | `selfhost/stage4/validation/` | `s32-as.c` | Spike proven end-to-end; replacement hardening in progress |
| 6 | `selfhost/stage4/validation/` | `s32-ar.c` | Spike started; replacement hardening in progress |
| 7 | `selfhost/stage4/validation/` | `s32-ld.c` (planned) | Not started |
| 8 | `selfhost/stage4/validation/` | `cc.c` (planned) | Not started |
| 9-16 | — | — | Not started |

**Current Stage 0-4 checkpoint (2026-02-15):**
- Forth bootstrap chain is stable: `asm.fth`, `ar.fth`, and `link.fth` produce runnable outputs.
- Stage 4 `cc.fth` passes `selfhost/stage4/run-regression.sh` on full emulator (`slow32-fast`).
- Stage 5 assembler spike is functional: stage4-built `s32-as.c` compiles, links, runs, and emits `.s32o`.
- Stage 6 archiver spike exists (`s32-ar.c`), but is not yet the default replacement in the bootstrap pipeline.


---

## Design Notes

### On the Subset C Runtime

The subset C toolchain needs a runtime library, but the real libc uses features (varargs for `printf`, function pointers for `qsort`) that the subset compiler can't handle. Options:

1. **Subset libc from scratch.** Write minimal versions: `print_int()`, `print_str()`, `print_hex()` instead of `printf()`. Inline sort instead of `qsort()`. This is cleanest but means the C tools at Stages 5-8 can't use standard `printf`.

2. **Assembly libc.** Keep the critical functions in assembly (hand-written or taken from the existing runtime `.s` files). The subset compiler handles the tool logic; the runtime is pre-assembled.

3. **Hybrid.** crt0 and I/O primitives in assembly, string/memory functions in subset C.

Option 3 is probably the right balance. The exact split depends on what the subset compiler can handle.

### On the Full Emulator

The Stage 0 emulator is deliberately austere — enough for Forth and the subset C toolchain, nothing more. Full C programs (SLOW BASIC, MY-BASIC, anything with floating point or complex I/O) need a real emulator: complete MMIO coverage, floating point instruction support, robust memory management, debugging features. Stage 10 is a genuine build step, not just a verification gate.

The simplest path is enhancing `s32-emu.c` on the host. It grows from ~780 to ~1,500 lines but remains auditable.

The ambitious path is building a SLOW-32 emulator in C, compiled by the self-hosted toolchain, running on Stage 0 (metacircular emulation). Slow, but it proves the toolchain handles non-trivial code. And if the full C toolchain can compile a DBT (dynamic binary translator) — SLOW-32 translating SLOW-32 blocks to... SLOW-32 blocks with optimized dispatch — that's a serious validation of the compiler's capability, even if the performance story is ironic.

### On Three vs. Two Self-Hosting Layers

You could skip the Forth self-hosting checkpoint (combine Layers B and C). The Forth tools would still be built and used, but you wouldn't bother rebuilding the kernel from source as a verification step.

I'd keep it. The Forth self-hosting checkpoint is cheap (it's just "assemble, archive, link the kernel source") and catches tool bugs before you bet the C compiler on them. If the linker has a relocation bug, better to find it when rebuilding the kernel than when linking the C compiler.

You could also skip the Subset C layer and go straight to Full C. This is a worse idea. The subset compiler is ~5,000 lines. The full compiler is ~7,000+ lines. Debugging a 7,000-line compiler that fails to compile itself is brutal. Debugging a 5,000-line compiler and then adding 2,000 lines of features is much more tractable.


---

## Related Documents

- [ISA-ENCODING.md](ISA-ENCODING.md) — Instruction encoding reference (essential for Stages 1-3).
- [C-SUBSET.md](C-SUBSET.md) — C language subset analysis (defines scope for Stage 4).
- [STAGE0-EMULATOR.md](STAGE0-EMULATOR.md) — Detailed Stage 0 specification.
- [STAGE1-FORTH.md](STAGE1-FORTH.md) — Forth kernel specification (used by Stages 1-4).
- [STAGE2-ASSEMBLER.md](STAGE2-ASSEMBLER.md) — Forth assembler specification (Stage 1).
- [STAGE3-LINKER.md](STAGE3-LINKER.md) — Forth linker specification (Stage 3).
- [STAGE4-COMPILER.md](STAGE4-COMPILER.md) — C compiler specification (Stage 4).
- [STAGE5-SELFHOST.md](STAGE5-SELFHOST.md) — Self-hosting procedure (Stages 5-9, 11-12).
- [STAGE6-FIXEDPOINT.md](STAGE6-FIXEDPOINT.md) — Fixed-point verification (Stages 9, 13, 16).
- [BOOTSTRAP.md](BOOTSTRAP.md) — Original (V1) bootstrap plan (historical reference).
