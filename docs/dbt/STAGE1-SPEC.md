# SLOW-32 Dynamic Binary Translator: Stage 1 Specification

## Overview

Stage 1 is a **correctness-first** DBT that translates SLOW-32 basic blocks to x86-64 machine code at runtime. The goal is a working, debuggable foundation—not peak performance.

### Design Principles

1. **Correctness over speed** — Every edge case handled, even if slow
2. **Debuggability** — Easy to inspect generated code, add tracing
3. **Incremental progress** — Start with interpreter fallback, JIT one opcode at a time
4. **Minimal dependencies** — No external JIT libraries (DynASM, asmjit, etc.)

### What Stage 1 Does

```
┌─────────────────────────────────────────────────────────────────┐
│                         Main Loop                               │
├─────────────────────────────────────────────────────────────────┤
│  1. Fetch guest PC                                              │
│  2. Translate basic block starting at PC → x86-64 code buffer   │
│  3. Execute translated code                                     │
│  4. Code returns with: next_pc, exit_reason                     │
│  5. Handle exit (branch, syscall, halt, fault)                  │
│  6. Goto 1                                                      │
└─────────────────────────────────────────────────────────────────┘
```

### What Stage 1 Does NOT Do

- Block caching (translate fresh each time)
- Direct block chaining
- Register allocation optimization
- Trace compilation
- Inline TLB

---

## Architecture

### Directory Structure

```
tools/dbt/
├── Makefile
├── dbt.c                 # Main entry point, dispatcher loop
├── cpu_state.h           # Guest CPU state structure
├── translate.c           # Block translator (guest → host)
├── translate.h
├── emit_x64.c            # x86-64 code emitter
├── emit_x64.h
├── memory.c              # Guest memory access helpers
├── memory.h
├── handlers.c            # Complex instruction handlers (MUL, DIV, MMIO)
├── handlers.h
├── debug.c               # Disassembler, tracing
├── debug.h
└── test/
    ├── test_emit.c       # Unit tests for x86-64 emitter
    └── test_translate.c  # Translation correctness tests
```

---

## Guest CPU State

All 32 SLOW-32 registers plus PC live in a C struct. Translated code accesses this struct via a base pointer held in a pinned x86-64 register.

```c
// cpu_state.h
#include <stdint.h>
#include <stdbool.h>

typedef struct {
    // Guest registers (r0 is always 0, enforced by generated code)
    uint32_t regs[32];

    // Program counter
    uint32_t pc;

    // Exit information (set by translated code before returning)
    uint32_t exit_reason;
    uint32_t exit_info;     // e.g., fault address, syscall number

    // Cycle/instruction counters
    uint64_t inst_count;
    uint64_t cycle_count;

    // Halt flag
    bool halted;

    // Pointer to guest memory base (for simple Stage 1 memory model)
    uint8_t *mem_base;
    uint32_t mem_size;

    // Code buffer for translated blocks
    uint8_t *code_buffer;
    size_t code_buffer_size;
    size_t code_buffer_used;

} dbt_cpu_state_t;

// Exit reasons
#define EXIT_BRANCH         0   // Normal branch, next_pc in cpu->pc
#define EXIT_INDIRECT       1   // Indirect branch (JALR), next_pc in cpu->pc
#define EXIT_HALT           2   // HALT instruction
#define EXIT_DEBUG          3   // DEBUG instruction (needs host I/O)
#define EXIT_YIELD          4   // YIELD instruction (MMIO)
#define EXIT_FAULT_FETCH    5   // Instruction fetch fault
#define EXIT_FAULT_LOAD     6   // Load fault
#define EXIT_FAULT_STORE    7   // Store fault
#define EXIT_ASSERT_FAIL    8   // ASSERT_EQ failed
#define EXIT_BLOCK_END      9   // Reached max instructions in block
```

### Register Mapping Strategy

x86-64 has 16 GPRs. We pin a few for the JIT runtime:

| x86-64 Register | Purpose |
|-----------------|---------|
| `rbp` | Pointer to `dbt_cpu_state_t` (never changes) |
| `r12` | Scratch for address calculation |
| `r13` | Scratch for temporaries |
| `r14` | Guest memory base (`cpu->mem_base`) |
| `r15` | Reserved for future (block chaining) |
| `rax, rbx, rcx, rdx, rsi, rdi, r8-r11` | Available for translation |

Guest registers are accessed via `[rbp + offsetof(dbt_cpu_state_t, regs) + reg*4]`.

```c
// Offset calculations
#define CPU_REGS_OFFSET     offsetof(dbt_cpu_state_t, regs)
#define CPU_PC_OFFSET       offsetof(dbt_cpu_state_t, pc)
#define CPU_EXIT_REASON     offsetof(dbt_cpu_state_t, exit_reason)
#define CPU_INST_COUNT      offsetof(dbt_cpu_state_t, inst_count)

// Access guest register in x86-64
// mov eax, [rbp + CPU_REGS_OFFSET + rs1*4]
#define GUEST_REG_OFFSET(n) (CPU_REGS_OFFSET + (n) * 4)
```

---

## x86-64 Code Emitter

A minimal emitter that outputs raw bytes. No fancy abstractions—just functions that append encoded instructions.

```c
// emit_x64.h
#include <stdint.h>
#include <stddef.h>

typedef struct {
    uint8_t *buf;       // Output buffer
    size_t capacity;    // Buffer size
    size_t offset;      // Current write position
} emit_ctx_t;

// Initialize emitter with buffer
void emit_init(emit_ctx_t *ctx, uint8_t *buf, size_t capacity);

// Get current code pointer (for patching)
uint8_t *emit_ptr(emit_ctx_t *ctx);
size_t emit_offset(emit_ctx_t *ctx);

// Basic x86-64 encoding helpers
void emit_byte(emit_ctx_t *ctx, uint8_t b);
void emit_word(emit_ctx_t *ctx, uint16_t w);
void emit_dword(emit_ctx_t *ctx, uint32_t d);
void emit_qword(emit_ctx_t *ctx, uint64_t q);

// REX prefixes
#define REX_W   0x48    // 64-bit operand
#define REX_R   0x44    // ModR/M reg extension
#define REX_X   0x42    // SIB index extension
#define REX_B   0x41    // ModR/M r/m or SIB base extension

// ModR/M byte construction
#define MODRM(mod, reg, rm) (((mod) << 6) | ((reg) << 3) | (rm))
#define MOD_INDIRECT    0
#define MOD_DISP8       1
#define MOD_DISP32      2
#define MOD_DIRECT      3

// x86-64 register encoding
typedef enum {
    RAX = 0, RCX, RDX, RBX, RSP, RBP, RSI, RDI,
    R8, R9, R10, R11, R12, R13, R14, R15
} x64_reg_t;

// Common instructions
void emit_mov_reg_reg(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src);
void emit_mov_reg_imm32(emit_ctx_t *ctx, x64_reg_t dst, uint32_t imm);
void emit_mov_reg_imm64(emit_ctx_t *ctx, x64_reg_t dst, uint64_t imm);
void emit_mov_reg_mem32(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, int32_t disp);
void emit_mov_mem32_reg(emit_ctx_t *ctx, x64_reg_t base, int32_t disp, x64_reg_t src);
void emit_mov_reg_mem8(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, int32_t disp);
void emit_mov_mem8_reg(emit_ctx_t *ctx, x64_reg_t base, int32_t disp, x64_reg_t src);

void emit_add_reg_reg(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src);
void emit_add_reg_imm32(emit_ctx_t *ctx, x64_reg_t dst, int32_t imm);
void emit_sub_reg_reg(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src);
void emit_sub_reg_imm32(emit_ctx_t *ctx, x64_reg_t dst, int32_t imm);

void emit_and_reg_reg(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src);
void emit_or_reg_reg(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src);
void emit_xor_reg_reg(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src);

void emit_shl_reg_cl(emit_ctx_t *ctx, x64_reg_t dst);
void emit_shl_reg_imm(emit_ctx_t *ctx, x64_reg_t dst, uint8_t imm);
void emit_shr_reg_cl(emit_ctx_t *ctx, x64_reg_t dst);
void emit_shr_reg_imm(emit_ctx_t *ctx, x64_reg_t dst, uint8_t imm);
void emit_sar_reg_cl(emit_ctx_t *ctx, x64_reg_t dst);
void emit_sar_reg_imm(emit_ctx_t *ctx, x64_reg_t dst, uint8_t imm);

void emit_imul_reg_reg(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src);
void emit_mul_rdx_rax(emit_ctx_t *ctx, x64_reg_t src);  // rdx:rax = rax * src
void emit_idiv_reg(emit_ctx_t *ctx, x64_reg_t src);     // rax = rdx:rax / src
void emit_div_reg(emit_ctx_t *ctx, x64_reg_t src);      // unsigned version

void emit_cmp_reg_reg(emit_ctx_t *ctx, x64_reg_t a, x64_reg_t b);
void emit_cmp_reg_imm32(emit_ctx_t *ctx, x64_reg_t a, int32_t imm);
void emit_test_reg_reg(emit_ctx_t *ctx, x64_reg_t a, x64_reg_t b);

// Conditional set (setcc family)
void emit_sete(emit_ctx_t *ctx, x64_reg_t dst);   // dst = ZF
void emit_setne(emit_ctx_t *ctx, x64_reg_t dst);  // dst = !ZF
void emit_setl(emit_ctx_t *ctx, x64_reg_t dst);   // dst = SF != OF (signed <)
void emit_setge(emit_ctx_t *ctx, x64_reg_t dst);  // dst = SF == OF (signed >=)
void emit_setb(emit_ctx_t *ctx, x64_reg_t dst);   // dst = CF (unsigned <)
void emit_setae(emit_ctx_t *ctx, x64_reg_t dst);  // dst = !CF (unsigned >=)
void emit_setg(emit_ctx_t *ctx, x64_reg_t dst);   // signed >
void emit_setle(emit_ctx_t *ctx, x64_reg_t dst);  // signed <=
void emit_seta(emit_ctx_t *ctx, x64_reg_t dst);   // unsigned >
void emit_setbe(emit_ctx_t *ctx, x64_reg_t dst);  // unsigned <=

void emit_movzx_reg32_reg8(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src);

// Jumps
void emit_jmp_rel32(emit_ctx_t *ctx, int32_t offset);
void emit_jmp_reg(emit_ctx_t *ctx, x64_reg_t target);
void emit_je_rel32(emit_ctx_t *ctx, int32_t offset);
void emit_jne_rel32(emit_ctx_t *ctx, int32_t offset);
void emit_jl_rel32(emit_ctx_t *ctx, int32_t offset);
void emit_jge_rel32(emit_ctx_t *ctx, int32_t offset);
void emit_jb_rel32(emit_ctx_t *ctx, int32_t offset);
void emit_jae_rel32(emit_ctx_t *ctx, int32_t offset);

// Call/ret
void emit_call_rel32(emit_ctx_t *ctx, int32_t offset);
void emit_call_reg(emit_ctx_t *ctx, x64_reg_t target);
void emit_ret(emit_ctx_t *ctx);

// Stack
void emit_push(emit_ctx_t *ctx, x64_reg_t reg);
void emit_pop(emit_ctx_t *ctx, x64_reg_t reg);

// Special
void emit_nop(emit_ctx_t *ctx);
void emit_int3(emit_ctx_t *ctx);  // Breakpoint for debugging
void emit_cdq(emit_ctx_t *ctx);   // Sign-extend eax to edx:eax
```

### Example: `emit_mov_reg_mem32`

```c
// mov r32, [base + disp32]  (load 32-bit from memory)
void emit_mov_reg_mem32(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, int32_t disp) {
    // Need REX prefix if using r8-r15
    uint8_t rex = 0;
    if (dst >= R8) rex |= REX_R;
    if (base >= R8) rex |= REX_B;
    if (rex) emit_byte(ctx, 0x40 | rex);

    emit_byte(ctx, 0x8B);  // MOV r32, r/m32

    // ModR/M: mod=10 (disp32), reg=dst, rm=base
    uint8_t dst_enc = dst & 7;
    uint8_t base_enc = base & 7;

    if (base_enc == 4) {
        // RSP/R12 needs SIB byte
        emit_byte(ctx, MODRM(MOD_DISP32, dst_enc, 4));
        emit_byte(ctx, 0x24);  // SIB: scale=0, index=4 (none), base=4
    } else {
        emit_byte(ctx, MODRM(MOD_DISP32, dst_enc, base_enc));
    }

    emit_dword(ctx, (uint32_t)disp);
}
```

---

## Block Translator

The translator takes a guest PC, fetches instructions until a branch/jump, and emits x86-64 code.

### Basic Block Definition

A basic block ends at:
- Any branch instruction (BEQ, BNE, BLT, BGE, BLTU, BGEU)
- Any jump instruction (JAL, JALR)
- HALT, DEBUG, YIELD, ASSERT_EQ
- Maximum instruction count (e.g., 64 instructions)

### Translation Structure

```c
// translate.h
#include "cpu_state.h"
#include "emit_x64.h"

typedef struct {
    dbt_cpu_state_t *cpu;
    emit_ctx_t emit;
    uint32_t guest_pc;          // Current guest PC being translated
    uint32_t block_start_pc;    // Start of current block
    int inst_count;             // Instructions in this block
    int max_inst;               // Max instructions per block
} translate_ctx_t;

// Translate a basic block starting at cpu->pc
// Returns pointer to executable code
typedef void (*translated_block_fn)(dbt_cpu_state_t *cpu);
translated_block_fn translate_block(translate_ctx_t *ctx);
```

### Translation Pseudocode

```c
translated_block_fn translate_block(translate_ctx_t *ctx) {
    emit_ctx_t *e = &ctx->emit;
    dbt_cpu_state_t *cpu = ctx->cpu;

    // Reset emitter to start of code buffer
    emit_init(e, cpu->code_buffer, cpu->code_buffer_size);

    // Save entry point
    void *entry = emit_ptr(e);

    // Prologue: nothing needed, rbp already points to cpu state
    // (Caller sets this up before calling translated code)

    ctx->guest_pc = cpu->pc;
    ctx->block_start_pc = cpu->pc;
    ctx->inst_count = 0;

    while (ctx->inst_count < ctx->max_inst) {
        // Fetch guest instruction
        uint32_t raw = *(uint32_t *)(cpu->mem_base + ctx->guest_pc);
        uint8_t opcode = raw & 0x7F;

        // Decode fields
        uint8_t rd  = (raw >> 7) & 0x1F;
        uint8_t rs1 = (raw >> 15) & 0x1F;
        uint8_t rs2 = (raw >> 20) & 0x1F;
        int32_t imm = decode_immediate(raw, opcode);

        // Translate based on opcode
        switch (opcode) {
            case OP_ADD:  translate_add(ctx, rd, rs1, rs2); break;
            case OP_SUB:  translate_sub(ctx, rd, rs1, rs2); break;
            case OP_ADDI: translate_addi(ctx, rd, rs1, imm); break;
            // ... etc ...

            case OP_BEQ: case OP_BNE: case OP_BLT: case OP_BGE:
            case OP_BLTU: case OP_BGEU:
                translate_branch(ctx, opcode, rs1, rs2, imm);
                goto block_done;  // Branch ends block

            case OP_JAL:
                translate_jal(ctx, rd, imm);
                goto block_done;

            case OP_JALR:
                translate_jalr(ctx, rd, rs1, imm);
                goto block_done;

            case OP_HALT:
                translate_halt(ctx);
                goto block_done;

            default:
                translate_fallback(ctx, raw);  // Interpret unknown opcodes
                break;
        }

        ctx->guest_pc += 4;
        ctx->inst_count++;
    }

    // Reached max instructions—exit with EXIT_BLOCK_END
    emit_exit(ctx, EXIT_BLOCK_END, ctx->guest_pc + 4);

block_done:
    // Make code executable
    // (assuming code_buffer was mmap'd with PROT_READ|PROT_WRITE|PROT_EXEC)

    return (translated_block_fn)entry;
}
```

---

## Instruction Translation Examples

### ADD rd, rs1, rs2

```c
void translate_add(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    emit_ctx_t *e = &ctx->emit;

    if (rd == 0) return;  // r0 is hardwired to 0, discard write

    // Load rs1 into eax
    if (rs1 == 0) {
        emit_xor_reg_reg(e, RAX, RAX);  // xor eax, eax (rs1 = 0)
    } else {
        emit_mov_reg_mem32(e, RAX, RBP, GUEST_REG_OFFSET(rs1));
    }

    // Add rs2
    if (rs2 == 0) {
        // Adding zero, nothing to do
    } else {
        emit_mov_reg_mem32(e, RCX, RBP, GUEST_REG_OFFSET(rs2));
        emit_add_reg_reg(e, RAX, RCX);
    }

    // Store to rd
    emit_mov_mem32_reg(e, RBP, GUEST_REG_OFFSET(rd), RAX);
}
```

### ADDI rd, rs1, imm

```c
void translate_addi(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    emit_ctx_t *e = &ctx->emit;

    if (rd == 0) return;

    if (rs1 == 0) {
        // rd = 0 + imm = imm (load immediate)
        emit_mov_reg_imm32(e, RAX, (uint32_t)imm);
    } else {
        emit_mov_reg_mem32(e, RAX, RBP, GUEST_REG_OFFSET(rs1));
        if (imm != 0) {
            emit_add_reg_imm32(e, RAX, imm);
        }
    }

    emit_mov_mem32_reg(e, RBP, GUEST_REG_OFFSET(rd), RAX);
}
```

### LUI rd, imm

```c
void translate_lui(translate_ctx_t *ctx, uint8_t rd, int32_t imm) {
    emit_ctx_t *e = &ctx->emit;

    if (rd == 0) return;

    // LUI loads upper 20 bits, imm already shifted by decoder
    emit_mov_reg_imm32(e, RAX, (uint32_t)imm);
    emit_mov_mem32_reg(e, RBP, GUEST_REG_OFFSET(rd), RAX);
}
```

### SLT rd, rs1, rs2 (Signed Less Than)

```c
void translate_slt(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    emit_ctx_t *e = &ctx->emit;

    if (rd == 0) return;

    // Load rs1 and rs2
    if (rs1 == 0) {
        emit_xor_reg_reg(e, RAX, RAX);
    } else {
        emit_mov_reg_mem32(e, RAX, RBP, GUEST_REG_OFFSET(rs1));
    }

    if (rs2 == 0) {
        emit_xor_reg_reg(e, RCX, RCX);
    } else {
        emit_mov_reg_mem32(e, RCX, RBP, GUEST_REG_OFFSET(rs2));
    }

    // cmp eax, ecx (compares rs1 - rs2)
    emit_cmp_reg_reg(e, RAX, RCX);

    // setl al (set al = 1 if SF != OF, else 0)
    emit_setl(e, RAX);

    // movzx eax, al (zero-extend to 32-bit)
    emit_movzx_reg32_reg8(e, RAX, RAX);

    // Store to rd
    emit_mov_mem32_reg(e, RBP, GUEST_REG_OFFSET(rd), RAX);
}
```

### LDW rd, rs1, imm (Load Word)

```c
void translate_ldw(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    emit_ctx_t *e = &ctx->emit;

    if (rd == 0) return;

    // Calculate address: rs1 + imm
    if (rs1 == 0) {
        emit_mov_reg_imm32(e, RAX, (uint32_t)imm);
    } else {
        emit_mov_reg_mem32(e, RAX, RBP, GUEST_REG_OFFSET(rs1));
        if (imm != 0) {
            emit_add_reg_imm32(e, RAX, imm);
        }
    }

    // For Stage 1: Simple memory model (guest addr = host addr + mem_base)
    // Real address = r14 + eax (r14 holds mem_base)
    // mov ecx, [r14 + rax]
    emit_mov_reg_mem32_indexed(e, RCX, R14, RAX, 1);

    // Store to rd
    emit_mov_mem32_reg(e, RBP, GUEST_REG_OFFSET(rd), RCX);
}
```

### Branch Instructions (BEQ, BNE, etc.)

Branches end a block and return control to the dispatcher with the target PC.

```c
void translate_beq(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t offset) {
    emit_ctx_t *e = &ctx->emit;
    uint32_t taken_pc = ctx->guest_pc + offset;
    uint32_t fall_pc = ctx->guest_pc + 4;

    // Load and compare
    if (rs1 == 0) {
        emit_xor_reg_reg(e, RAX, RAX);
    } else {
        emit_mov_reg_mem32(e, RAX, RBP, GUEST_REG_OFFSET(rs1));
    }

    if (rs2 == 0) {
        emit_cmp_reg_imm32(e, RAX, 0);
    } else {
        emit_mov_reg_mem32(e, RCX, RBP, GUEST_REG_OFFSET(rs2));
        emit_cmp_reg_reg(e, RAX, RCX);
    }

    // je taken_path
    size_t jcc_offset = emit_offset(e);
    emit_je_rel32(e, 0);  // Placeholder, patch later

    // Fall-through path: set pc = fall_pc, exit
    emit_mov_reg_imm32(e, RAX, fall_pc);
    emit_mov_mem32_reg(e, RBP, CPU_PC_OFFSET, RAX);
    emit_mov_reg_imm32(e, RAX, EXIT_BRANCH);
    emit_mov_mem32_reg(e, RBP, CPU_EXIT_REASON, RAX);
    emit_ret(e);

    // Taken path: patch jump target
    size_t taken_offset = emit_offset(e);
    patch_rel32(e, jcc_offset, taken_offset - jcc_offset - 4);

    emit_mov_reg_imm32(e, RAX, taken_pc);
    emit_mov_mem32_reg(e, RBP, CPU_PC_OFFSET, RAX);
    emit_mov_reg_imm32(e, RAX, EXIT_BRANCH);
    emit_mov_mem32_reg(e, RBP, CPU_EXIT_REASON, RAX);
    emit_ret(e);
}
```

### JALR (Indirect Jump)

```c
void translate_jalr(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    emit_ctx_t *e = &ctx->emit;
    uint32_t return_pc = ctx->guest_pc + 4;

    // Calculate target: (rs1 + imm) & ~1
    if (rs1 == 0) {
        emit_mov_reg_imm32(e, RAX, (uint32_t)imm & ~1u);
    } else {
        emit_mov_reg_mem32(e, RAX, RBP, GUEST_REG_OFFSET(rs1));
        if (imm != 0) {
            emit_add_reg_imm32(e, RAX, imm);
        }
        emit_and_reg_imm32(e, RAX, ~1u);  // Clear LSB
    }

    // Store target to pc
    emit_mov_mem32_reg(e, RBP, CPU_PC_OFFSET, RAX);

    // Store return address to rd (if rd != 0)
    if (rd != 0) {
        emit_mov_reg_imm32(e, RAX, return_pc);
        emit_mov_mem32_reg(e, RBP, GUEST_REG_OFFSET(rd), RAX);
    }

    // Exit with indirect branch reason
    emit_mov_reg_imm32(e, RAX, EXIT_INDIRECT);
    emit_mov_mem32_reg(e, RBP, CPU_EXIT_REASON, RAX);
    emit_ret(e);
}
```

---

## Main Dispatcher Loop

```c
// dbt.c
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include "cpu_state.h"
#include "translate.h"

#define CODE_BUFFER_SIZE (64 * 1024)  // 64KB per block (way more than needed)

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <program.s32x>\n", argv[0]);
        return 1;
    }

    dbt_cpu_state_t cpu = {0};

    // Allocate executable code buffer
    cpu.code_buffer = mmap(NULL, CODE_BUFFER_SIZE,
                           PROT_READ | PROT_WRITE | PROT_EXEC,
                           MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (cpu.code_buffer == MAP_FAILED) {
        perror("mmap code buffer");
        return 1;
    }
    cpu.code_buffer_size = CODE_BUFFER_SIZE;

    // Load program (similar to existing emulator)
    if (!load_s32x(&cpu, argv[1])) {
        fprintf(stderr, "Failed to load %s\n", argv[1]);
        return 1;
    }

    // Initialize translation context
    translate_ctx_t trans = {
        .cpu = &cpu,
        .max_inst = 64,
    };

    // Main dispatch loop
    while (!cpu.halted) {
        // Translate block at current PC
        translated_block_fn block = translate_block(&trans);

        // Execute translated code
        // Setup: rbp = &cpu, r14 = cpu.mem_base
        asm volatile(
            "push %%rbp\n\t"
            "push %%r14\n\t"
            "mov %0, %%rbp\n\t"
            "mov %1, %%r14\n\t"
            "call *%2\n\t"
            "pop %%r14\n\t"
            "pop %%rbp\n\t"
            :
            : "r"(&cpu), "r"(cpu.mem_base), "r"(block)
            : "rax", "rbx", "rcx", "rdx", "rsi", "rdi",
              "r8", "r9", "r10", "r11", "r12", "r13", "memory"
        );

        // Handle exit
        switch (cpu.exit_reason) {
            case EXIT_BRANCH:
            case EXIT_INDIRECT:
            case EXIT_BLOCK_END:
                // Continue to next block
                break;

            case EXIT_HALT:
                cpu.halted = true;
                break;

            case EXIT_DEBUG:
                putchar(cpu.exit_info);
                fflush(stdout);
                break;

            case EXIT_YIELD:
                // Handle MMIO
                handle_yield(&cpu);
                break;

            case EXIT_FAULT_FETCH:
            case EXIT_FAULT_LOAD:
            case EXIT_FAULT_STORE:
                fprintf(stderr, "Memory fault at PC=0x%08X, addr=0x%08X\n",
                        cpu.pc, cpu.exit_info);
                cpu.halted = true;
                break;

            case EXIT_ASSERT_FAIL:
                fprintf(stderr, "ASSERT_EQ failed at PC=0x%08X\n", cpu.pc);
                cpu.halted = true;
                break;
        }
    }

    printf("\nExited with r1 = %d\n", cpu.regs[1]);
    printf("Instructions: %lu\n", cpu.inst_count);

    munmap(cpu.code_buffer, CODE_BUFFER_SIZE);
    return cpu.regs[1];
}
```

---

## Opcode-to-Translation Table

| SLOW-32 Opcode | Translation Complexity | Notes |
|----------------|------------------------|-------|
| ADD, SUB, XOR, OR, AND | Simple | Direct x86-64 mapping |
| ADDI, ORI, ANDI, XORI | Simple | x86-64 immediate forms |
| SLL, SRL, SRA | Simple | x86-64 shift with CL or imm |
| SLLI, SRLI, SRAI | Simple | x86-64 shift immediate |
| SLT, SLTU, SEQ, SNE | Medium | cmp + setcc + movzx |
| SGT, SGE, SLE, SGTU, SGEU, SLEU | Medium | cmp + setcc + movzx |
| SLTI, SLTIU | Medium | cmp imm + setcc + movzx |
| MUL | Medium | x86-64 imul |
| MULH | Medium | x86-64 mul (unsigned high) |
| DIV, REM | Medium | x86-64 idiv, handle div-by-zero |
| LUI | Simple | mov imm32 |
| LDB, LDH, LDW | Medium | Memory access + sign extend |
| LDBU, LDHU | Medium | Memory access + zero extend |
| STB, STH, STW | Medium | Memory store |
| BEQ, BNE, BLT, BGE, BLTU, BGEU | Medium | cmp + conditional exit |
| JAL | Simple | Store PC+4, set PC to target |
| JALR | Medium | Indirect, set PC from register |
| NOP | Trivial | Nothing |
| DEBUG | Exit | Return to dispatcher for I/O |
| YIELD | Exit | Return for MMIO handling |
| HALT | Exit | Set halted flag |
| ASSERT_EQ | Exit | cmp + conditional fault |

---

## Memory Model (Stage 1)

For Stage 1, use the simplest possible memory model:

1. Allocate a single contiguous buffer for all guest memory (16MB default)
2. Guest address N maps to `mem_base + N`
3. No bounds checking in hot path (trust the program)
4. Protection faults caught by host SIGSEGV handler

```c
// Allocate guest memory
cpu.mem_size = 16 * 1024 * 1024;  // 16MB
cpu.mem_base = mmap(NULL, cpu.mem_size,
                    PROT_READ | PROT_WRITE,
                    MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

// Code section: make first 1MB read-only after loading
mprotect(cpu.mem_base, 0x100000, PROT_READ);
```

### Memory Access in Translated Code

```c
// LDW: guest_addr in eax, result in ecx
// Host addr = r14 (mem_base) + rax (guest addr)
// mov ecx, [r14 + rax]

// For indexed addressing:
void emit_mov_reg_mem32_indexed(emit_ctx_t *ctx, x64_reg_t dst,
                                 x64_reg_t base, x64_reg_t index, uint8_t scale) {
    // REX prefix for extended registers
    uint8_t rex = 0x40;
    if (dst >= R8) rex |= REX_R;
    if (base >= R8) rex |= REX_B;
    if (index >= R8) rex |= REX_X;
    emit_byte(ctx, rex);

    emit_byte(ctx, 0x8B);  // MOV r32, r/m32

    // ModR/M: mod=00, reg=dst, rm=100 (SIB follows)
    emit_byte(ctx, MODRM(MOD_INDIRECT, dst & 7, 4));

    // SIB: scale=0 (scale factor 1), index=index, base=base
    uint8_t scale_enc = 0;  // 2^0 = 1
    emit_byte(ctx, (scale_enc << 6) | ((index & 7) << 3) | (base & 7));
}
```

---

## Testing Strategy

### Unit Tests

```c
// test/test_emit.c
void test_emit_add() {
    uint8_t buf[64];
    emit_ctx_t ctx;
    emit_init(&ctx, buf, sizeof(buf));

    emit_add_reg_reg(&ctx, RAX, RCX);

    // Expected: 01 c8 (add eax, ecx)
    assert(ctx.offset == 2);
    assert(buf[0] == 0x01);
    assert(buf[1] == 0xC8);
}

void test_emit_mov_mem() {
    uint8_t buf[64];
    emit_ctx_t ctx;
    emit_init(&ctx, buf, sizeof(buf));

    // mov eax, [rbp + 0x10]
    emit_mov_reg_mem32(&ctx, RAX, RBP, 0x10);

    // Expected: 8b 45 10 or 8b 85 10 00 00 00 depending on displacement size
    // ... validate ...
}
```

### Integration Tests

```c
// test/test_translate.c
void test_translate_add() {
    dbt_cpu_state_t cpu = {0};
    setup_test_cpu(&cpu);

    // Set up guest code: ADD r1, r2, r3
    uint32_t *code = (uint32_t *)cpu.mem_base;
    code[0] = encode_add(1, 2, 3);  // r1 = r2 + r3
    code[1] = encode_halt();

    cpu.regs[2] = 10;
    cpu.regs[3] = 20;

    run_dbt(&cpu);

    assert(cpu.regs[1] == 30);
}
```

### Differential Testing

Run the same programs through both the interpreter and DBT, compare results:

```bash
#!/bin/bash
for test in regression/*.s32x; do
    interp_out=$(./tools/emulator/slow32 "$test" 2>&1)
    dbt_out=$(./tools/dbt/slow32-dbt "$test" 2>&1)
    if [ "$interp_out" != "$dbt_out" ]; then
        echo "MISMATCH: $test"
        diff <(echo "$interp_out") <(echo "$dbt_out")
    fi
done
```

---

## Performance Expectations

Stage 1 is not optimized. Expected overhead sources:

| Overhead | Cause |
|----------|-------|
| Translation time | Re-translate every block every time |
| Dispatcher loop | Call/ret for each basic block |
| Memory access | All regs go through memory (no register allocation) |
| No chaining | Indirect jump back to dispatcher after every block |

**Expected performance**: 100-500 MIPS (compared to ~350 MIPS interpreter, ~2000 MIPS QEMU)

Stage 1 may actually be *slower* than the interpreter! That's fine. The goal is correctness and a foundation for optimization.

---

## Development Roadmap

### Week 1: Emitter + Basic Instructions
- [ ] `emit_x64.c` with all needed instructions
- [ ] Unit tests for emitter
- [ ] Translate: ADD, SUB, AND, OR, XOR, ADDI
- [ ] Translate: LUI
- [ ] Translate: SLL, SRL, SRA (reg and imm forms)

### Week 2: Comparisons + Memory
- [ ] Translate: SLT, SLTU, SEQ, SNE, SGT, SGE, SLE, etc.
- [ ] Translate: LDW, STW (word only first)
- [ ] Translate: LDB, LDH, LDBU, LDHU, STB, STH

### Week 3: Control Flow
- [ ] Translate: JAL
- [ ] Translate: JALR
- [ ] Translate: BEQ, BNE, BLT, BGE, BLTU, BGEU
- [ ] Main dispatch loop

### Week 4: Special + Polish
- [ ] Translate: MUL, MULH, DIV, REM
- [ ] Translate: DEBUG, YIELD, HALT, NOP, ASSERT_EQ
- [ ] s32x loader integration
- [ ] Differential testing against interpreter
- [ ] Run full regression suite

---

## Stage 2 Preview

Once Stage 1 passes all tests, Stage 2 adds:

1. **Block cache**: Hash table keyed by guest PC
2. **Direct chaining**: Patch translated branches to jump directly to target blocks
3. **Lazy translation**: Only translate blocks when first reached
4. **Invalidation**: Handle self-modifying code (rare for SLOW-32)

This should bring performance to 500-1500 MIPS.

---

## References

- Intel® 64 and IA-32 Architectures Software Developer Manuals
- [x86-64 Instruction Encoding](https://wiki.osdev.org/X86-64_Instruction_Encoding)
- QEMU TCG source (for inspiration, not copying)
- [Adventures in JIT compilation](https://eli.thegreenplace.net/2017/adventures-in-jit-compilation-part-1-an-interpreter/)

---

## Appendix A: x86-64 Encoding Quick Reference

### REX Prefix (for 64-bit operands and extended registers)
```
0100 WRXB
     W = 1: 64-bit operand size
     R = 1: ModR/M reg field extension
     X = 1: SIB index field extension
     B = 1: ModR/M r/m or SIB base extension
```

### ModR/M Byte
```
MM RRR BBB
MM = mod (00=indirect, 01=disp8, 10=disp32, 11=register)
RRR = reg (register operand or opcode extension)
BBB = r/m (register/memory operand)
```

### Common Opcodes
```
01 /r       ADD r/m32, r32
29 /r       SUB r/m32, r32
09 /r       OR r/m32, r32
21 /r       AND r/m32, r32
31 /r       XOR r/m32, r32
39 /r       CMP r/m32, r32
8B /r       MOV r32, r/m32
89 /r       MOV r/m32, r32
B8+rd id    MOV r32, imm32
81 /0 id    ADD r/m32, imm32
83 /0 ib    ADD r/m32, imm8 (sign-extended)
0F 94 /r    SETE r/m8
0F 95 /r    SETNE r/m8
0F 9C /r    SETL r/m8
0F 9D /r    SETGE r/m8
0F 92 /r    SETB r/m8
0F 93 /r    SETAE r/m8
E8 cd       CALL rel32
FF /2       CALL r/m64
C3          RET
EB cb       JMP rel8
E9 cd       JMP rel32
74 cb       JE rel8
0F 84 cd    JE rel32
```

---

## Appendix B: SLOW-32 Opcode Values

```c
// From slow32.h
#define OP_ADD    0x00
#define OP_SUB    0x01
#define OP_XOR    0x02
#define OP_OR     0x03
#define OP_AND    0x04
#define OP_SLL    0x05
#define OP_SRL    0x06
#define OP_SRA    0x07
#define OP_SLT    0x08
#define OP_SLTU   0x09
#define OP_MUL    0x0A
#define OP_MULH   0x0B
#define OP_DIV    0x0C
#define OP_REM    0x0D
#define OP_SEQ    0x0E
#define OP_SNE    0x0F
#define OP_ADDI   0x10
#define OP_ORI    0x11
#define OP_ANDI   0x12
#define OP_SLLI   0x13
#define OP_SRLI   0x14
#define OP_SRAI   0x15
#define OP_SLTI   0x16
#define OP_SLTIU  0x17
#define OP_SGT    0x18
#define OP_SGTU   0x19
#define OP_SLE    0x1A
#define OP_SLEU   0x1B
#define OP_SGE    0x1C
#define OP_SGEU   0x1D
#define OP_XORI   0x1E
#define OP_LUI    0x20
#define OP_LDB    0x30
#define OP_LDH    0x31
#define OP_LDW    0x32
#define OP_LDBU   0x33
#define OP_LDHU   0x34
#define OP_STB    0x38
#define OP_STH    0x39
#define OP_STW    0x3A
#define OP_ASSERT_EQ 0x3F
#define OP_JAL    0x40
#define OP_JALR   0x41
#define OP_BEQ    0x48
#define OP_BNE    0x49
#define OP_BLT    0x4A
#define OP_BGE    0x4B
#define OP_BLTU   0x4C
#define OP_BGEU   0x4D
#define OP_NOP    0x50
#define OP_YIELD  0x51
#define OP_DEBUG  0x52
#define OP_HALT   0x7F
```
