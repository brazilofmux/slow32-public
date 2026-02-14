# SLOW-32 Self-Hosting Bootstrap

## The Problem

SLOW-32 has a complete toolchain — assembler, linker, archiver, disassembler, dump utility — all written in C and cross-compiled via LLVM. The runtime libraries (libc, crt0, libs32) are likewise compiled with clang targeting `slow32-unknown-none`.

This means building anything for SLOW-32 requires:
1. A full LLVM build (~2 GB) with the custom SLOW-32 backend
2. A host machine capable of running LLVM

The goal of self-hosting is to break this dependency: produce a SLOW-32 toolchain that runs *on* SLOW-32 (under emulation) and can rebuild itself without LLVM.

## What's In Scope

**Must compile and run on SLOW-32:**
- `slow32asm.c` — Assembler (2,573 lines)
- `s32-ld.c` — Linker (2,289 lines)
- `s32-ar.c` — Archiver (672 lines)
- `slow32dis.c` — Disassembler (659 lines)
- `slow32dump.c` — Dump utility (513 lines)
- `s32_formats.h` — Binary format definitions (157 lines)
- `s32_hashtable.h` — Hash table / dynamic arrays (129 lines)
- Runtime: `crt0.s`, libc sources, libs32 sources
- A C compiler capable of compiling all of the above

**Total toolchain code: ~6,993 lines of C + 286 lines of headers.**

## What's Out of Scope

- LLVM backend (stays as the "production" compiler)
- QEMU TCG emulator
- Host-native emulators (`slow32`, `slow32-fast`)
- DBT (dynamic binary translator)
- Free Pascal backend
- The Forth kernel, SLOW BASIC (already self-contained)

## Two Bootstrap Paths

### Path A: Pure Bootstrap (No Host C Compiler)

Start from nothing but an emulator binary and build everything from hand-assembled machine code:

```
Stage 0: Minimal emulator (runs on host, ~300 lines of C)
   ↓
Stage 1: Hand-assembled Forth kernel (.s32x binary)
   ↓
Stage 2: Forth-hosted SLOW-32 assembler
   ↓
Stage 3: Forth-hosted minimal linker
   ↓
Stage 4: Minimal C compiler (written in Forth or assembly)
   ↓
Stage 5: Compile the real toolchain with the stage 4 compiler
   ↓
Stage 6: Verify fixed point (recompile compiler with itself)
```

This path is the intellectually pure approach. Every byte is auditable from first principles. It's also the most work.

### Path B: Practical Bootstrap (Host C Compiler Available)

Use the existing LLVM cross-compiler to build a minimal C compiler for SLOW-32, then use *that* to build the toolchain:

```
LLVM cross-compiles → minimal C compiler → runs on SLOW-32
   ↓
Compile real toolchain sources on SLOW-32
   ↓
Verify fixed point
```

This path is faster but still relies on LLVM for the initial bootstrap. Once the fixed point is reached, LLVM is no longer needed.

### Recommendation

**Start with Path A.** The Forth kernel already exists and is fully functional (150+ words, ANS Core + Core Extensions complete). The major work is stages 2-4. Path B can serve as a shortcut for testing stage 5 before the Forth-based compiler is ready.

## Stage Dependency Graph

```
[Stage 0: Emulator]
        │
        ▼
[Stage 1: Forth Kernel]
        │
        ├──────────────────┐
        ▼                  ▼
[Stage 2: Assembler]  [ISA-ENCODING.md]
        │
        ▼
[Stage 3: Linker]
        │
        ▼
[Stage 4: C Compiler] ◄── [C-SUBSET.md]
        │
        ▼
[Stage 5: Self-Host]
        │
        ▼
[Stage 6: Fixed Point]
```

## The Self-Hosting Boundary

```
┌──────────────────────────────────────────────┐
│              HOST MACHINE                     │
│                                               │
│  ┌─────────────────────────────────────────┐  │
│  │         SLOW-32 Emulator                │  │
│  │                                         │  │
│  │  ┌───────────────────────────────────┐  │  │
│  │  │     SLOW-32 Toolchain             │  │  │
│  │  │                                   │  │  │
│  │  │  slow32asm  →  .s32o files        │  │  │
│  │  │  s32-ld     →  .s32x files        │  │  │
│  │  │  s32-ar     →  .s32a archives     │  │  │
│  │  │  cc         →  .s assembly        │  │  │
│  │  │                                   │  │  │
│  │  │  Can compile its own source code  │  │  │
│  │  └───────────────────────────────────┘  │  │
│  └─────────────────────────────────────────┘  │
└──────────────────────────────────────────────┘
```

Everything inside the inner box runs as SLOW-32 code. The only host dependency is the emulator itself — a trivial ~300-line C program that anyone can audit and reimplement.

## Complexity Estimates

| Stage | Description | Estimated Size | Difficulty |
|-------|-------------|---------------|------------|
| 0 | Minimal emulator | ~300-400 lines C | Easy |
| 1 | Forth kernel (hand-assembled) | ~500-800 instructions | Tedious but mechanical |
| 2 | Forth assembler | ~300-500 Forth lines | Moderate |
| 3 | Forth linker | ~400-600 Forth lines | Moderate |
| 4 | C compiler | ~3,000-8,000 lines Forth/asm | Hard |
| 5 | Compile real toolchain | Integration work | Moderate |
| 6 | Fixed-point verification | Testing | Easy |

The existing Forth kernel (5,254 lines, ~150 words) can serve as stage 1 directly — it's already a working .s32x binary built from assembly source. For a pure bootstrap, a minimal kernel with ~35 primitives would suffice.

## Key Design Decisions

1. **Forth as the bootstrap language.** It's the simplest language that can extend itself. A minimal Forth fits in ~800 instructions of hand-assembled machine code.

2. **No floating point needed.** The entire toolchain is integer-only. The bootstrap C compiler doesn't need to handle `float` or `double`.

3. **No varargs needed.** None of the toolchain sources use `va_list`. Printf is only used for output — the bootstrap compiler can use a simpler output mechanism.

4. **Dynamic memory is essential.** The toolchain uses `malloc`/`realloc`/`free` extensively (~140 calls). The bootstrap must provide a working allocator.

5. **File I/O is essential.** The toolchain reads/writes binary files via `fopen`/`fread`/`fwrite`/`fseek`. The MMIO ring buffer subsystem provides this.

## Related Documents

- [STAGE0-EMULATOR.md](STAGE0-EMULATOR.md) — Minimal bootstrap emulator
- [STAGE1-FORTH.md](STAGE1-FORTH.md) — Hand-assembled Forth kernel
- [STAGE2-ASSEMBLER.md](STAGE2-ASSEMBLER.md) — Forth-hosted assembler
- [STAGE3-LINKER.md](STAGE3-LINKER.md) — Forth-hosted linker
- [STAGE4-COMPILER.md](STAGE4-COMPILER.md) — Minimal C compiler
- [STAGE5-SELFHOST.md](STAGE5-SELFHOST.md) — Compiling the real toolchain
- [STAGE6-FIXEDPOINT.md](STAGE6-FIXEDPOINT.md) — Verification
- [ISA-ENCODING.md](ISA-ENCODING.md) — Instruction encoding quick reference
- [C-SUBSET.md](C-SUBSET.md) — C language subset analysis
