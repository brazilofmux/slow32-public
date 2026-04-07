# Cross-Compiler: SLOW-32 to x86-64

*Closing the bootstrap loop — a SLOW-32 program that generates native x86-64 binaries.*

## Vision

Today, SLOW-32 depends on external tools to exist. The LLVM backend requires a host C++ compiler. The emulators require a host C compiler. Even the self-hosted compiler (cc, Stage 06) produces SLOW-32 binaries that need a host-compiled emulator to run.

The cross-compiler breaks this dependency. It is a SLOW-32 program that reads C source and emits native x86-64 ELF binaries. Once it can compile the Stage 00 emulator, the bootstrap chain becomes:

```
cc-x64.s32x  (runs on any SLOW-32 emulator)
    ↓ compiles
s32-emu          (native x86-64 Linux binary)
    ↓ runs
cc-x64.s32x   (now self-sustaining)
```

Eventually it compiles slow32, slow32-fast, and slow32-dbt — the entire toolchain generated from within SLOW-32 itself.

## Architecture

### What Changes, What Stays

cc is structured as a pipeline of header files included by a single driver:

```
cc.c → c_lexer_gen.c → ast.h → parser.h → sema.h
        → hir.h → hir_lower.h → hir_ssa.h → hir_opt.h → hir_licm.h
        → hir_burg.h → hir_regalloc.h → hir_codegen.h
```

Everything through `hir_licm.h` is **target-independent**. The HIR operates on abstract values, basic blocks, and control flow — no SLOW-32 specifics leak through.

The target-dependent pieces are:

| Component | SLOW-32 | x86-64 |
|-----------|---------|--------|
| BURG rules | `hir_burg.h` | `hir_burg_x64.h` (new) |
| Register allocator config | r11-r28 (18 callee-saved) | rbx, r12-r15, rbp (6 callee-saved) + rax, rcx, rdx, rsi, rdi, r8-r11 (10 caller-saved) |
| Code emission | `hir_codegen.h` (text assembly) | `hir_codegen_x64.h` (binary ELF) |
| Calling convention | r3-r10 args, r1-r2 return | rdi,rsi,rdx,rcx,r8,r9 args, rax return |
| Output format | `.s` text → assembler → `.s32o` → linker → `.s32x` | Direct `.elf` binary (no assembler needed) |

### Driver Structure

A new driver `cc-x64.c` includes the same frontend headers but swaps the backend:

```c
// cc-x64.c — Cross-compiler: C → x86-64 ELF
#include "c_lexer_gen.c"
#include "ast.h"
#include "parser.h"
#include "sema.h"
#include "hir.h"
#include "hir_lower_x64.h"    // x86-64 calling convention
#include "hir_ssa.h"
#include "hir_opt.h"
#include "hir_licm.h"
#include "hir_burg_x64.h"     // x86-64 instruction patterns
#include "hir_regalloc_x64.h" // x86-64 register set
#include "hir_codegen_x64.h"  // x86-64 binary emission + ELF writer
```

Alternatively, the lowering phase (`hir_lower.h`) could be shared with target-specific `#ifdef` sections for the calling convention. The important thing is that the frontend (lexer through optimizer) is identical and shared verbatim.

### Why Binary Output, Not Assembly Text

The SLOW-32 backend emits assembly text (`.s` files) because a SLOW-32 assembler exists in the ecosystem. No x86-64 assembler exists in the SLOW-32 world, and writing one is unnecessary complexity — x86-64 instructions are variable-length (1-15 bytes) with ModR/M, SIB, and displacement encoding that's far more complex than SLOW-32's fixed 32-bit format.

Instead, the x86-64 backend emits binary directly into a code buffer (like the DBT already does) and wraps it in a minimal ELF64 header. This is simpler than an assembler: no parsing, no symbol resolution, no two-pass label fixup. Labels are just offsets into the code buffer, patched in place.

## x86-64 Backend Design

### Instruction Encoding

The DBT (`tools/dbt/translate.c`) already contains proven x86-64 encoding logic for every instruction we need. The cross-compiler's encoder can follow the same patterns.

Core instruction forms needed:

```
REX.W + opcode + ModR/M                    (reg-reg ALU)
REX.W + opcode + ModR/M + disp8/32         (load/store)
REX.W + 0x69/0x6B + ModR/M + imm8/32      (IMUL)
REX.W + 0xC7 + ModR/M + imm32             (MOV reg, imm32)
REX.W + 0xB8+r + imm64                    (MOV reg, imm64)
0xE8 + rel32                               (CALL near)
0xE9 + rel32                               (JMP near)
0x0F 0x8x + rel32                          (Jcc near)
0xC3                                       (RET)
```

About 25-30 encoding functions, most under 10 lines each.

### BURG Rules (hir_burg_x64.h)

The BURG nonterminals change slightly for x86-64's richer addressing:

| NT | Meaning |
|----|---------|
| `BG_REG` | Value in a general-purpose register |
| `BG_IMM` | Compile-time constant (fits imm32) |
| `BG_MEM` | Memory operand [base + disp] (can be used directly in ALU) |
| `BG_ADDR` | Address computation (LEA candidate) |
| `BG_STMT` | Side effect, no value |

Key patterns:

```
HI_ICONST          → BG_IMM   cost 0    (constant, no code)
BG_IMM              → BG_REG   cost 1    (MOV reg, imm)
HI_ADD(BG_REG, BG_IMM) → BG_REG cost 1  (ADD reg, imm  or  LEA)
HI_ADD(BG_REG, BG_REG) → BG_REG cost 1  (ADD reg, reg  or  LEA)
HI_MUL(BG_REG, BG_REG) → BG_REG cost 3  (IMUL reg, reg)
HI_LOAD(BG_MEM)     → BG_REG   cost 1    (MOV reg, [base+disp])
HI_LOAD(BG_REG)     → BG_REG   cost 1    (MOV reg, [reg])
HI_STORE(BG_MEM)     → BG_STMT  cost 1    (MOV [base+disp], reg)
HI_ALLOCA           → BG_MEM   cost 0    (frame slot, no code)
HI_GADDR            → BG_ADDR  cost 1    (LEA or RIP-relative)
```

x86-64's two-operand encoding (dest = dest OP src) means the register allocator should prefer assigning the same register to a value and its most-used consumer. The BURG cost model can encourage this by making "same-reg" patterns cheaper.

### Register Allocation (hir_regalloc_x64.h)

x86-64 System V ABI register classification:

| Role | Registers | Count |
|------|-----------|-------|
| Callee-saved (survive calls) | rbx, r12, r13, r14, r15, rbp | 6 |
| Caller-saved (scratch) | rax, rcx, rdx, rsi, rdi, r8, r9, r10, r11 | 9 |
| Reserved | rsp | 1 |

**Allocation strategy**: The existing linear scan works. Callee-saved registers are preferred for values that live across calls. Caller-saved registers are preferred for temporaries. The allocator needs 15 physical registers instead of 18, but x86-64's `MOV [rsp+off], reg` is faster than SLOW-32's explicit save/restore, so spilling is cheaper.

**Special constraints**:
- `IDIV` requires dividend in RDX:RAX, destroys both → pre-color div/rem instructions
- Shift counts must be in CL for variable shifts → pre-color or use RORX/SHLX (BMI2)
- Return value in RAX
- Function args in rdi, rsi, rdx, rcx, r8, r9 (must be loaded before CALL)

### Calling Convention (System V AMD64)

```
Arguments:  rdi, rsi, rdx, rcx, r8, r9  (integers/pointers, left to right)
            stack (right to left push) for args 7+
Return:     rax (64-bit), rdx:rax (128-bit)
Callee-save: rbx, rbp, r12-r15
Caller-save: everything else
Stack:      16-byte aligned before CALL
Red zone:   128 bytes below rsp (leaf functions can use without adjusting rsp)
```

Changes to HIR lowering:
- `HI_PARAM` index 0-5 maps to rdi,rsi,rdx,rcx,r8,r9 (not r3-r10)
- Stack params at `[rbp+16]`, `[rbp+24]`, etc. (after saved rbp + return address)
- 64-bit values use a single register (not register pairs)
- `HI_CALLHI` is unnecessary — 64-bit return fits in rax

### ELF64 Output (elf_writer.h)

Minimal static ELF64 executable. No dynamic linking, no GOT/PLT, no DWARF.

```
ELF Header (64 bytes)
  - e_type: ET_EXEC
  - e_machine: EM_X86_64
  - e_entry: address of _start (or main, with crt0 stub)

Program Headers (2 entries, 56 bytes each)
  - PT_LOAD: .text (R+X), page-aligned
  - PT_LOAD: .data + .bss (R+W), page-aligned

Section data:
  .text   — executable code (all functions)
  .rodata — string literals, constants
  .data   — initialized globals
  .bss    — uninitialized globals (zero-filled by kernel)
```

The writer collects code bytes during emission, then prepends headers with computed offsets. Total ELF boilerplate: ~200-300 lines.

### Startup / crt0

A minimal `_start` stub (in the compiler, emitted directly):

```asm
_start:
    xor  rbp, rbp           ; mark end of stack frames
    mov  rdi, [rsp]          ; argc
    lea  rsi, [rsp+8]        ; argv
    call main
    mov  edi, eax            ; exit code = main's return
    mov  eax, 60             ; sys_exit
    syscall
```

This is ~20 bytes of fixed machine code prepended to .text.

### libc

The cross-compiled program needs libc functions (printf, malloc, fopen, etc.). Options:

1. **Link against host libc (glibc/musl)** — emit syscall wrappers + use dynamic linking. Adds PLT/GOT complexity.

2. **Static musl** — compile musl libc with the cross-compiler. Circular dependency (musl is complex C), but feasible once the compiler is mature.

3. **Minimal static libc** — write a small libc in the cross-compiler itself (emit inline). For Stage 00, we need: `malloc`/`free`, `open`/`read`/`write`/`close`/`lseek`, `fopen`/`fclose`/`fread`/`fwrite`/`fseek`/`ftell`/`fgetc`/`fputc`, `printf`/`fprintf`, `memcpy`/`memset`/`strlen`/`strcmp`, `stat`/`fstat`, `getenv`. All implementable as direct Linux syscalls + buffered I/O.

4. **Syscall-only approach for Stage 00** — Stage 00 doesn't use much libc. Provide thin syscall wrappers (~200 lines of machine code) and a simple allocator. Graduate to fuller libc later.

**Recommendation**: Option 4 for M1-M4, graduating to Option 3 for M5+. This avoids external dependencies entirely — the cross-compiled binary is a fully static Linux executable with no runtime dependencies.

The SLOW-32 runtime already has implementations of malloc, printf, string functions, and file I/O in `runtime/`. These can be cross-compiled once the compiler is self-sustaining.

## Stages

### Stage X0: Foundation — x86-64 Instruction Encoder

**Goal**: Binary encoding functions for the ~30 x86-64 instruction forms we need.

**Deliverables**:
- `x64_encode.h` — encoding functions: `enc_rex()`, `enc_modrm()`, `enc_sib()`, `enc_alu_rr()`, `enc_alu_ri()`, `enc_mov_rr()`, `enc_mov_ri()`, `enc_mov_rm()`, `enc_mov_mr()`, `enc_lea()`, `enc_call_rel()`, `enc_jmp_rel()`, `enc_jcc_rel()`, `enc_push()`, `enc_pop()`, `enc_ret()`, `enc_cmp_rr()`, `enc_cmp_ri()`, `enc_test_rr()`, `enc_idiv()`, `enc_imul_rr()`, `enc_imul_ri()`, `enc_shift()`, `enc_cdq()`, `enc_syscall()`, `enc_setcc()`
- Code buffer management: `x64_emit_byte()`, `x64_emit32()`, `x64_emit64()`, `x64_patch_rel32()`
- Register encoding constants (RAX=0, RCX=1, ..., R15=15)

**Test**: Standalone test program that encodes a few instructions and verifies the bytes match expected output.

**Complexity**: ~400-600 lines. Modeled on `tools/dbt/translate.c` encoding patterns.

### Stage X1: ELF64 Writer

**Goal**: Produce valid static ELF64 executables from raw code/data buffers.

**Deliverables**:
- `elf_writer.h` — ELF64 structures (Elf64_Ehdr, Elf64_Phdr, Elf64_Shdr) and writer
- `elf_write(filename, code_buf, code_len, data_buf, data_len, bss_size, entry_offset)`
- crt0 stub (inline `_start` → `main` → `sys_exit`)

**Test**: Hand-craft a tiny x86-64 program (mov eax,42; mov edi,eax; mov eax,60; syscall) in a byte array, wrap in ELF, verify it runs and exits with code 42.

**Complexity**: ~300-500 lines.

### Stage X2: Minimal Code Generation — Integer Arithmetic + Control Flow

**Goal**: Compile a subset of C to working x86-64 code. Functions, local variables, if/else, loops, basic arithmetic.

**Deliverables**:
- `hir_burg_x64.h` — BURG patterns for integer ALU, comparisons, branches, loads, stores
- `hir_regalloc_x64.h` — register set definition (15 GPRs), allocation config
- `hir_codegen_x64.h` — instruction dispatch (hcg_inst for each HIR opcode), function prologue/epilogue, basic block labels (as code offsets with backpatching)
- `hir_lower_x64.h` — System V calling convention (arg registers, return in rax, stack alignment)

**Test programs**:
```c
int add(int a, int b) { return a + b; }
int fib(int n) { if (n <= 1) return n; return fib(n-1) + fib(n-2); }
int sum(int n) { int s = 0; for (int i = 0; i < n; i++) s += i; return s; }
```

**Complexity**: ~2000-3000 lines across the four headers.

### Stage X3: Pointers, Structs, Arrays

**Goal**: Handle aggregate types and pointer operations — prerequisite for compiling any real program.

**Deliverables**:
- Struct layout (member offsets, alignment, sizeof)
- Pointer arithmetic (scaled by pointee size)
- Array indexing (decay to pointer + scale)
- Address-of (`&`) and dereference (`*`)
- Member access (`.` and `->`)
- String literals in .rodata

**Test programs**:
```c
struct point { int x; int y; };
int dist2(struct point *p) { return p->x * p->x + p->y * p->y; }
void swap(int *a, int *b) { int t = *a; *a = *b; *b = t; }
int strlen(char *s) { int n = 0; while (*s++) n++; return n; }
```

**Complexity**: Mostly frontend work — should already work if Stage 06 handles it. Backend additions: LEA for address computation, scaled indexing.

### Stage X4: Syscall Libc + Stage 00 Compilation

**Goal**: Compile the Stage 00 emulator (`selfhost/stage00/s32-emu.c`) to a native x86-64 Linux binary.

**Deliverables**:
- `libc_x64.h` or `libc_x64.c` — minimal libc via Linux syscalls:
  - Memory: `malloc`/`free`/`calloc` (simple bump allocator or freelist)
  - File I/O: `open`/`close`/`read`/`write`/`lseek`/`stat`/`fstat` (thin syscall wrappers)
  - Buffered I/O: `fopen`/`fclose`/`fread`/`fwrite`/`fseek`/`ftell`/`fgetc`/`fputc`
  - Formatted I/O: `printf`/`fprintf` (subset: `%d`, `%s`, `%x`, `%c`, `%u`, `%p`)
  - String: `memcpy`/`memset`/`strlen`/`strcmp`
  - Environment: `getenv`
- Varargs support in codegen (va_list as register save area on stack)
- 64-bit integer arithmetic (native on x86-64 — simpler than SLOW-32)

**Test**: Compile `s32-emu.c` → `s32-emu` (native x86-64 ELF). Run a simple SLOW-32 test program on it. Compare output to reference emulator.

**The milestone**: A native emulator built entirely from SLOW-32 code. The bootstrap chain works.

**Complexity**: ~1500-2500 lines for libc. Varargs is the trickiest part (x86-64 varargs ABI requires a register save area).

### Stage X5: Compile slow32 + slow32-fast

**Goal**: Compile the production emulators to native x86-64.

**Deliverables**:
- MMIO ring buffer support (struct-heavy, but no new language features)
- Remaining libc functions used by the emulators
- Any missing C features exposed by the larger codebase

**Test**: Compile slow32.c and slow32-fast.c. Run the full regression suite through them. Performance comparison against host-compiled versions.

**Complexity**: Mostly testing and debugging. The language features are already covered.

### Stage X6: Compile slow32-dbt

**Goal**: The DBT compiles itself via the cross-compiler. Full circle.

**Deliverables**:
- Function pointer support (already in Stage 06, but needs x86-64 emission for indirect CALL)
- `mmap`/`mprotect` syscalls (the DBT needs executable memory)
- Remaining libc (if any)
- Any edge cases in the 20K-line DBT codebase

**Test**: Cross-compile the DBT. Run regression suite through the cross-compiled DBT. Verify the cross-compiled DBT can itself run cc-x64 to compile Stage 00. Triple bootstrap.

### Stage X7: ARM64 Backend (Future)

Same exercise for AArch64. Reuse everything except the encoder, BURG rules, codegen, and ELF writer. The patterns from the existing ARM64 DBT backend (`translate_a64.c`, `emit_a64.c`) inform the BURG rules.

### Stage X8: Windows PE Backend (Future)

x86-64 encoding is identical. Replace ELF writer with PE/COFF writer (~400 lines). Replace Linux syscall libc with Windows API calls. The existing Windows DBT work in `~/tinymux` provides reference.

## Dependency Graph

```
                    Frontend (shared, unchanged)
                    ┌─────────────────────────┐
                    │ c_lexer_gen.c            │
                    │ ast.h, parser.h, sema.h  │
                    │ hir.h, hir_lower.h*      │
                    │ hir_ssa.h, hir_opt.h     │
                    │ hir_licm.h               │
                    └────────────┬────────────┘
                                 │
              ┌──────────────────┼──────────────────┐
              │                  │                   │
        SLOW-32 Backend    x86-64 Backend      ARM64 Backend
        (existing)         (Stages X0-X6)      (Stage X7)
        ┌────────────┐    ┌────────────────┐   ┌──────────────┐
        │ hir_burg.h │    │ x64_encode.h   │   │ a64_encode.h │
        │ hir_rega.. │    │ elf_writer.h   │   │ elf_writer.h │
        │ hir_code.. │    │ hir_burg_x64.h │   │ hir_burg_a64 │
        │ codegen.h  │    │ hir_rega.._x64 │   │ hir_rega.._a │
        └────────────┘    │ hir_code.._x64 │   │ hir_code.._a │
                          │ libc_x64.c     │   │ libc_a64.c   │
                          └────────────────┘   └──────────────┘

  * hir_lower.h needs target-specific calling convention sections
```

## What About Self-Compilation?

Once Stages X0-X4 work, the cross-compiler can compile Stage 00. But can it compile *itself*?

cc-x64 is a SLOW-32 program. To compile itself, it would need to:
1. Run on a SLOW-32 emulator (the one it just cross-compiled)
2. Read its own C source
3. Emit a native x86-64 binary of itself

This works automatically — the cross-compiler is C source that targets x86-64. It can compile any C program to x86-64, including itself. The result is a **native x86-64 cross-compiler** that no longer needs a SLOW-32 emulator to run:

```
cc-x64.s32x  →  (runs on s32-emu)  →  compiles cc-x64.c  →  cc-x64 (native)
cc-x64       →  (runs natively)    →  compiles cc-x64.c  →  cc-x64' (native)
                                          (cc-x64 == cc-x64' : fixed point)
```

At this point, SLOW-32 has produced a native compiler that perpetuates itself without any external tools.

## Estimated Effort per Stage

| Stage | New Code | Cumulative | Key Risk |
|-------|----------|------------|----------|
| X0: Encoder | ~500 lines | 500 | None — well-understood from DBT |
| X1: ELF Writer | ~400 lines | 900 | None — simple format |
| X2: Core Codegen | ~2500 lines | 3400 | Register allocator constraints (IDIV, shifts) |
| X3: Aggregates | ~500 lines | 3900 | Frontend already handles this; backend additions minimal |
| X4: Libc + Stage 00 | ~2000 lines | 5900 | Varargs ABI; printf correctness |
| X5: Emulators | ~500 lines | 6400 | Integration testing |
| X6: DBT | ~300 lines | 6700 | mmap/mprotect; function pointer edge cases |

Total new code: ~6700 lines. For reference, the SLOW-32 backend (hir_burg.h + hir_codegen.h + hir_regalloc.h) is ~3500 lines. The x86-64 backend is roughly 2x that, mostly due to variable-length instruction encoding and the inline libc.

## Files to Create

```
selfhost/stage06-cross/               (cross-compiler companion to stage06)
├── cc-x64.c                       Driver (like cc.c, swaps backend includes)
├── x64_encode.h                      x86-64 instruction encoding
├── elf_writer.h                      ELF64 binary output
├── hir_burg_x64.h                    BURG pattern table for x86-64
├── hir_regalloc_x64.h                Register set & allocation config
├── hir_codegen_x64.h                 Instruction emission & function codegen
├── hir_lower_x64.h                   Calling convention (or merged into hir_lower.h)
├── libc_x64.c                        Minimal libc (compiled separately, linked)
├── crt0_x64.h                        Startup stub (inline bytes)
└── tests/
    ├── test_encoder.c                Encoder verification
    ├── test_elf.c                    ELF output verification
    ├── test_arith.c                  Arithmetic + control flow
    ├── test_structs.c                Aggregates + pointers
    └── test_stage00.sh               Full Stage 00 compilation test
```

## Open Questions

1. **Stage number**: Is this Stage 07, or does it live alongside Stage 06 as a variant? The frontend is Stage 06's frontend — only the backend differs.

2. **Shared vs. forked hir_lower.h**: The lowering phase has target-specific calling convention code (HI_PARAM indices, stack arg layout, 64-bit value splitting). Fork the file, or use `#ifdef TARGET_X64` sections?

3. **libc strategy**: Start with syscall wrappers (Option 4), or cross-compile the existing SLOW-32 runtime libc immediately? The runtime libc is ~5000 lines across multiple files — substantial but proven.

4. **Testing infrastructure**: Do we need a test harness that runs on SLOW-32, or can we test cross-compiled binaries directly on the host?

5. **Optimization level**: The cross-compiled code doesn't need to be fast initially — correctness first. But x86-64's two-operand ISA benefits significantly from copy coalescing in the register allocator. Worth adding early?
