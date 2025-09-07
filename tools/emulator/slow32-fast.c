// SLOW-32 Fast Emulator - Fixed version with correct instruction encoding
// Pre-decode + function pointer dispatch for maximum speed

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <time.h>
#include <inttypes.h>
#include "slow32.h"
#include "s32x_loader.h"

// Enable to trap on unaligned LD/ST (recommended for a strict ISA)
#ifndef S32_ALLOW_UNALIGNED
#define S32_TRAP_ON_UNALIGNED 1
#endif

// Forward declarations
typedef struct fast_cpu_state fast_cpu_state_t;
typedef struct decoded_inst decoded_inst_t;

// Handler function type
typedef void (*handler_fn)(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc);

// Pre-decoded instruction with handler pointer
struct decoded_inst {
    handler_fn handler;
    uint8_t rd;
    uint8_t rs1;
    uint8_t rs2;
    int32_t imm;
    uint32_t fallthrough_pc;
    uint32_t branch_target;  // Pre-computed for branches
};

struct fast_cpu_state {
    uint32_t regs[32];
    uint32_t pc;
    uint8_t *memory;
    uint32_t mem_size;
    decoded_inst_t *decoded_code;
    uint32_t code_words;
    bool halted;
    uint64_t cycle_count;
    uint64_t inst_count;
    
    // Memory protection limits from .s32x loader
    bool wx_enabled;
    uint32_t code_limit;
    uint32_t rodata_limit;
    uint32_t data_limit;
};

// Write protection helper
static inline bool write_protected(fast_cpu_state_t* cpu, uint32_t addr) {
    return cpu->wx_enabled && addr < cpu->rodata_limit;
}

// Bounds checking helpers
static inline bool in_bounds(fast_cpu_state_t* cpu, uint32_t addr, uint32_t size) {
    return addr <= cpu->mem_size - size;
}
static inline bool aligned(uint32_t addr, uint32_t size) {
#if S32_TRAP_ON_UNALIGNED
    return (addr & (size - 1)) == 0;
#else
    return true;
#endif
}

// Handler implementations
static void op_add(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] + cpu->regs[inst->rs2];
}

static void op_sub(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] - cpu->regs[inst->rs2];
}

static void op_xor(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] ^ cpu->regs[inst->rs2];
}

static void op_or(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] | cpu->regs[inst->rs2];
}

static void op_and(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] & cpu->regs[inst->rs2];
}

static void op_sll(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] << (cpu->regs[inst->rs2] & 0x1F);
}

static void op_srl(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] >> (cpu->regs[inst->rs2] & 0x1F);
}

static void op_sra(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = (int32_t)cpu->regs[inst->rs1] >> (cpu->regs[inst->rs2] & 0x1F);
}

static void op_slt(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = ((int32_t)cpu->regs[inst->rs1] < (int32_t)cpu->regs[inst->rs2]) ? 1 : 0;
}

static void op_sltu(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = (cpu->regs[inst->rs1] < cpu->regs[inst->rs2]) ? 1 : 0;
}

static void op_mul(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] * cpu->regs[inst->rs2];
    cpu->cycle_count += 31;
}

static void op_mulh(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    int64_t result = (int64_t)(int32_t)cpu->regs[inst->rs1] * (int64_t)(int32_t)cpu->regs[inst->rs2];
    cpu->regs[inst->rd] = result >> 32;
    cpu->cycle_count += 31;
}

static void op_div(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    uint32_t divisor = cpu->regs[inst->rs2];
    if (divisor != 0) {
        cpu->regs[inst->rd] = (int32_t)cpu->regs[inst->rs1] / (int32_t)divisor;
    } else {
        cpu->regs[inst->rd] = -1;
    }
    cpu->cycle_count += 63;
}

static void op_rem(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    uint32_t divisor = cpu->regs[inst->rs2];
    if (divisor != 0) {
        cpu->regs[inst->rd] = (int32_t)cpu->regs[inst->rs1] % (int32_t)divisor;
    } else {
        cpu->regs[inst->rd] = cpu->regs[inst->rs1];
    }
    cpu->cycle_count += 63;
}

static void op_seq(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = (cpu->regs[inst->rs1] == cpu->regs[inst->rs2]) ? 1 : 0;
}

static void op_sne(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = (cpu->regs[inst->rs1] != cpu->regs[inst->rs2]) ? 1 : 0;
}

static void op_sgt(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = ((int32_t)cpu->regs[inst->rs1] > (int32_t)cpu->regs[inst->rs2]) ? 1 : 0;
}

static void op_sgtu(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = (cpu->regs[inst->rs1] > cpu->regs[inst->rs2]) ? 1 : 0;
}

static void op_sle(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = ((int32_t)cpu->regs[inst->rs1] <= (int32_t)cpu->regs[inst->rs2]) ? 1 : 0;
}

static void op_sleu(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = (cpu->regs[inst->rs1] <= cpu->regs[inst->rs2]) ? 1 : 0;
}

static void op_sge(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = ((int32_t)cpu->regs[inst->rs1] >= (int32_t)cpu->regs[inst->rs2]) ? 1 : 0;
}

static void op_sgeu(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = (cpu->regs[inst->rs1] >= cpu->regs[inst->rs2]) ? 1 : 0;
}

static void op_addi(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] + inst->imm;
}

static void op_ori(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] | inst->imm;
}

static void op_andi(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] & inst->imm;
}

static void op_xori(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] ^ inst->imm;
}

static void op_slli(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] << (inst->imm & 0x1F);
}

static void op_srli(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] >> (inst->imm & 0x1F);
}

static void op_srai(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = (int32_t)cpu->regs[inst->rs1] >> (inst->imm & 0x1F);
}

static void op_slti(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = ((int32_t)cpu->regs[inst->rs1] < inst->imm) ? 1 : 0;
}

static void op_sltiu(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = (cpu->regs[inst->rs1] < (uint32_t)inst->imm) ? 1 : 0;
}

static void op_lui(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = inst->imm;
}

static void op_ldw(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    uint32_t addr = cpu->regs[inst->rs1] + inst->imm;
    if (!in_bounds(cpu, addr, 4) || !aligned(addr,4)) {
        fprintf(stderr, "Error: Read out of bounds at 0x%08x\n", addr);
        cpu->halted = true;
        return;
    }
    cpu->regs[inst->rd] = *(uint32_t*)(cpu->memory + addr);
    cpu->cycle_count += 3;
}

static void op_ldh(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    uint32_t addr = cpu->regs[inst->rs1] + inst->imm;
    if (!in_bounds(cpu, addr, 2) || !aligned(addr,2)) {
        fprintf(stderr, "Error: Read out of bounds at 0x%08x\n", addr);
        cpu->halted = true;
        return;
    }
    cpu->regs[inst->rd] = (int32_t)(int16_t)*(uint16_t*)(cpu->memory + addr);
    cpu->cycle_count += 3;
}

static void op_ldhu(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    uint32_t addr = cpu->regs[inst->rs1] + inst->imm;
    if (!in_bounds(cpu, addr, 2) || !aligned(addr,2)) {
        fprintf(stderr, "Error: Read out of bounds at 0x%08x\n", addr);
        cpu->halted = true;
        return;
    }
    cpu->regs[inst->rd] = *(uint16_t*)(cpu->memory + addr);
    cpu->cycle_count += 3;
}

static void op_ldb(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    uint32_t addr = cpu->regs[inst->rs1] + inst->imm;
    if (!in_bounds(cpu, addr, 1)) {
        fprintf(stderr, "Error: Read out of bounds at 0x%08x\n", addr);
        cpu->halted = true;
        return;
    }
    cpu->regs[inst->rd] = (int32_t)(int8_t)*(uint8_t*)(cpu->memory + addr);
    cpu->cycle_count += 3;
}

static void op_ldbu(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    uint32_t addr = cpu->regs[inst->rs1] + inst->imm;
    if (!in_bounds(cpu, addr, 1)) {
        fprintf(stderr, "Error: Read out of bounds at 0x%08x\n", addr);
        cpu->halted = true;
        return;
    }
    cpu->regs[inst->rd] = *(uint8_t*)(cpu->memory + addr);
    cpu->cycle_count += 3;
}

static void op_stw(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    uint32_t addr = cpu->regs[inst->rs1] + inst->imm;
    if (!in_bounds(cpu, addr, 4) || !aligned(addr,4)) {
        fprintf(stderr, "Error: Write out of bounds at 0x%08x\n", addr);
        cpu->halted = true;
        return;
    }
    if (write_protected(cpu, addr)) {
        fprintf(stderr, "Error: Write to protected memory at 0x%08x\n", addr);
        cpu->halted = true;
        return;
    }
    *(uint32_t*)(cpu->memory + addr) = cpu->regs[inst->rs2];
    cpu->cycle_count += 3;
}

static void op_sth(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    uint32_t addr = cpu->regs[inst->rs1] + inst->imm;
    if (!in_bounds(cpu, addr, 2) || !aligned(addr,2)) {
        fprintf(stderr, "Error: Write out of bounds at 0x%08x\n", addr);
        cpu->halted = true;
        return;
    }
    if (write_protected(cpu, addr)) {
        fprintf(stderr, "Error: Write to protected memory at 0x%08x\n", addr);
        cpu->halted = true;
        return;
    }
    *(uint16_t*)(cpu->memory + addr) = cpu->regs[inst->rs2] & 0xFFFF;
    cpu->cycle_count += 3;
}

static void op_stb(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    uint32_t addr = cpu->regs[inst->rs1] + inst->imm;
    if (!in_bounds(cpu, addr, 1)) {
        fprintf(stderr, "Error: Write out of bounds at 0x%08x\n", addr);
        cpu->halted = true;
        return;
    }
    if (write_protected(cpu, addr)) {
        fprintf(stderr, "Error: Write to protected memory at 0x%08x\n", addr);
        cpu->halted = true;
        return;
    }
    *(uint8_t*)(cpu->memory + addr) = cpu->regs[inst->rs2] & 0xFF;
    cpu->cycle_count += 3;
}

static void op_beq(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    if (cpu->regs[inst->rs1] == cpu->regs[inst->rs2]) {
        *next_pc = inst->branch_target;
    }
}

static void op_bne(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    if (cpu->regs[inst->rs1] != cpu->regs[inst->rs2]) {
        *next_pc = inst->branch_target;
    }
}

static void op_blt(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    if ((int32_t)cpu->regs[inst->rs1] < (int32_t)cpu->regs[inst->rs2]) {
        *next_pc = inst->branch_target;
    }
}

static void op_bge(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    if ((int32_t)cpu->regs[inst->rs1] >= (int32_t)cpu->regs[inst->rs2]) {
        *next_pc = inst->branch_target;
    }
}

static void op_bltu(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    if (cpu->regs[inst->rs1] < cpu->regs[inst->rs2]) {
        *next_pc = inst->branch_target;
    }
}

static void op_bgeu(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    if (cpu->regs[inst->rs1] >= cpu->regs[inst->rs2]) {
        *next_pc = inst->branch_target;
    }
}

static void op_jal(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = inst->fallthrough_pc;
    *next_pc = inst->branch_target;
}

static void op_jalr(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    uint32_t target = (cpu->regs[inst->rs1] + inst->imm) & ~1;
    cpu->regs[inst->rd] = inst->fallthrough_pc;
    *next_pc = target;
}

static void op_halt(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->halted = true;
}

static void op_assert_eq(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    if (cpu->regs[inst->rs1] != cpu->regs[inst->rs2]) {
        fprintf(stderr, "ASSERT r%d==r%d failed @PC=%08x\n", 
                inst->rs1, inst->rs2, cpu->pc);
        cpu->halted = true;
        cpu->regs[1] = 0xDEADBEEF;
    }
}

static void op_debug(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    putchar(cpu->regs[inst->rs1] & 0xFF);
    fflush(stdout);
}

static void op_yield(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->cycle_count += cpu->regs[inst->rs1];
}

static void op_nop(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    // Do nothing
}

static void op_invalid(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    fprintf(stderr, "Invalid instruction at PC 0x%08x\n", cpu->pc);
    cpu->halted = true;
}

// Get handler for opcode
static handler_fn get_handler(uint8_t opcode) {
    switch (opcode) {
        case OP_ADD: return op_add;
        case OP_SUB: return op_sub;
        case OP_XOR: return op_xor;
        case OP_OR: return op_or;
        case OP_AND: return op_and;
        case OP_SLL: return op_sll;
        case OP_SRL: return op_srl;
        case OP_SRA: return op_sra;
        case OP_SLT: return op_slt;
        case OP_SLTU: return op_sltu;
        case OP_MUL: return op_mul;
        case OP_MULH: return op_mulh;
        case OP_DIV: return op_div;
        case OP_REM: return op_rem;
        case OP_SEQ: return op_seq;
        case OP_SNE: return op_sne;
        case OP_SGT: return op_sgt;
        case OP_SGTU: return op_sgtu;
        case OP_SLE: return op_sle;
        case OP_SLEU: return op_sleu;
        case OP_SGE: return op_sge;
        case OP_SGEU: return op_sgeu;
        case OP_ADDI: return op_addi;
        case OP_ORI: return op_ori;
        case OP_ANDI: return op_andi;
        case OP_XORI: return op_xori;
        case OP_SLLI: return op_slli;
        case OP_SRLI: return op_srli;
        case OP_SRAI: return op_srai;
        case OP_SLTI: return op_slti;
        case OP_SLTIU: return op_sltiu;
        case OP_LUI: return op_lui;
        case OP_LDW: return op_ldw;
        case OP_LDH: return op_ldh;
        case OP_LDHU: return op_ldhu;
        case OP_LDB: return op_ldb;
        case OP_LDBU: return op_ldbu;
        case OP_STW: return op_stw;
        case OP_STH: return op_sth;
        case OP_STB: return op_stb;
        case OP_BEQ: return op_beq;
        case OP_BNE: return op_bne;
        case OP_BLT: return op_blt;
        case OP_BGE: return op_bge;
        case OP_BLTU: return op_bltu;
        case OP_BGEU: return op_bgeu;
        case OP_JAL: return op_jal;
        case OP_JALR: return op_jalr;
        case OP_NOP: return op_nop;
        case OP_YIELD: return op_yield;
        case OP_DEBUG: return op_debug;
        case OP_ASSERT_EQ: return op_assert_eq;
        case OP_HALT: return op_halt;
        default: return op_invalid;
    }
}

// Pre-decode all instructions using CORRECT encoding
static void predecode_program(fast_cpu_state_t *cpu, uint32_t code_size) {
    cpu->code_words = code_size / 4;
    cpu->decoded_code = calloc(cpu->code_words, sizeof(decoded_inst_t));
    
    for (uint32_t i = 0; i < cpu->code_words; i++) {
        uint32_t pc = i * 4;
        uint32_t raw = *(uint32_t*)(cpu->memory + pc);
        decoded_inst_t *di = &cpu->decoded_code[i];
        
        // Extract opcode from lower 7 bits (RISC-V style)
        uint8_t opcode = raw & 0x7F;
        di->handler = get_handler(opcode);
        di->fallthrough_pc = pc + 4;
        
        // Decode based on opcode ranges (matching slow32.c)
        switch (opcode) {
            case OP_ADD ... OP_SNE:
            case OP_SGT ... OP_SGEU:
                // R-format
                di->rd = (raw >> 7) & 0x1F;
                di->rs1 = (raw >> 15) & 0x1F;
                di->rs2 = (raw >> 20) & 0x1F;
                di->imm = 0;
                break;
                
            case OP_ORI ... OP_ANDI:
            case OP_XORI:
            case OP_SLTIU:
                // I-format with zero-extended immediates
                di->rd = (raw >> 7) & 0x1F;
                di->rs1 = (raw >> 15) & 0x1F;
                di->rs2 = 0;
                di->imm = (raw >> 20) & 0xFFF;  // Zero-extend 12-bit immediate
                break;
                
            case OP_ADDI:
            case OP_SLLI ... OP_SLTI:
            case OP_LDB ... OP_LDHU:
            case OP_JALR:
                // I-format with sign-extended immediates
                di->rd = (raw >> 7) & 0x1F;
                di->rs1 = (raw >> 15) & 0x1F;
                di->rs2 = 0;
                di->imm = ((int32_t)raw) >> 20;  // Sign-extend
                break;
                
            case OP_STB ... OP_STW:
                // S-format
                di->rd = 0;
                di->rs1 = (raw >> 15) & 0x1F;
                di->rs2 = (raw >> 20) & 0x1F;
                di->imm = ((raw >> 7) & 0x1F) | (((int32_t)raw >> 25) << 5);
                break;
                
            case OP_BEQ ... OP_BGEU:
                // B-format
                di->rd = 0;
                di->rs1 = (raw >> 15) & 0x1F;
                di->rs2 = (raw >> 20) & 0x1F;
                di->imm = (((raw >> 8) & 0xF) << 1) | (((raw >> 25) & 0x3F) << 5) |
                         (((raw >> 7) & 0x1) << 11) | (((int32_t)raw >> 31) << 12);
                di->branch_target = pc + 4 + di->imm;  // PC+4 relative!
                break;
                
            case OP_LUI:
                // U-format
                di->rd = (raw >> 7) & 0x1F;
                di->rs1 = 0;
                di->rs2 = 0;
                di->imm = raw & 0xFFFFF000;
                break;
                
            case OP_JAL:
                // J-format
                di->rd = (raw >> 7) & 0x1F;
                di->rs1 = 0;
                di->rs2 = 0;
                {
                    uint32_t imm20 = (raw >> 31) & 0x1;
                    uint32_t imm10_1 = (raw >> 21) & 0x3FF;
                    uint32_t imm11 = (raw >> 20) & 0x1;
                    uint32_t imm19_12 = (raw >> 12) & 0xFF;
                    di->imm = (imm20 << 20) | (imm19_12 << 12) | (imm11 << 11) | (imm10_1 << 1);
                    if (di->imm & 0x100000) di->imm |= 0xFFE00000;  // Sign extend
                }
                di->branch_target = pc + di->imm;  // PC relative for JAL
                break;
                
            case OP_NOP:
            case OP_YIELD:
            case OP_DEBUG:
            case OP_ASSERT_EQ:
            case OP_HALT:
                // Special R-format
                di->rd = (raw >> 7) & 0x1F;
                di->rs1 = (raw >> 15) & 0x1F;
                di->rs2 = (raw >> 20) & 0x1F;
                di->imm = 0;
                break;
                
            default:
                // Unknown
                di->rd = 0;
                di->rs1 = 0;
                di->rs2 = 0;
                di->imm = 0;
                break;
        }
    }
}

// Fast execution loop
static inline void cpu_step_fast(fast_cpu_state_t *cpu) {
    if ((cpu->pc >> 2) >= cpu->code_words) {
        fprintf(stderr, "PC out of bounds: 0x%08x\n", cpu->pc);
        cpu->halted = true;
        return;
    }
    
    decoded_inst_t *di = &cpu->decoded_code[cpu->pc >> 2];
    uint32_t next_pc = di->fallthrough_pc;
    
    // Direct function call - no switch!
    di->handler(cpu, di, &next_pc);
    
    cpu->pc = next_pc;
    cpu->regs[0] = 0;  // r0 always zero
    cpu->cycle_count++;
    cpu->inst_count++;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <binary>\n", argv[0]);
        return 1;
    }
    
    fast_cpu_state_t cpu = {0};
    cpu.memory = calloc(1, DEFAULT_MEM_SIZE);
    cpu.mem_size = DEFAULT_MEM_SIZE;
    if (!cpu.memory) {
        fprintf(stderr, "Failed to allocate memory\n");
        return 1;
    }
    
    s32x_loader_config_t cfg = {
        .memory = (uint32_t*)cpu.memory,
        .mem_size = DEFAULT_MEM_SIZE,
        .pc = &cpu.pc,
        .sp = NULL,
        .registers = cpu.regs,
        .verbose = 0
    };
    
    s32x_load_result_t lr = load_s32x_file(argv[1], &cfg);
    if (!lr.success) {
        fprintf(stderr, "%s\n", lr.error_msg);
        free(cpu.memory);
        return 1;
    }
    
    cpu.regs[REG_SP] = lr.stack_base;
    cpu.regs[REG_FP] = lr.stack_base;
    cpu.code_words = lr.code_limit / 4;   // predecode [0, code_limit)
    cpu.wx_enabled = lr.has_wxorx;
    cpu.code_limit = lr.code_limit;
    cpu.rodata_limit = lr.rodata_limit;
    cpu.data_limit = lr.data_limit;
    
    predecode_program(&cpu, lr.code_limit);
    
    // Time the execution
    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start);
    
    while (!cpu.halted) {
        cpu_step_fast(&cpu);
    }
    
    clock_gettime(CLOCK_MONOTONIC, &end);
    
    // Calculate elapsed time
    double elapsed = (end.tv_sec - start.tv_sec) + (end.tv_nsec - start.tv_nsec) / 1e9;
    
    printf("\nProgram halted.\n");
    printf("Exit code: %d\n", cpu.regs[1]);
    printf("Instructions executed: %" PRIu64 "\n", cpu.inst_count);
    printf("Simulated cycles: %" PRIu64 "\n", cpu.cycle_count);
    printf("Wall time: %.6f seconds\n", elapsed);
    
    if (elapsed > 0) {
        double ips = cpu.inst_count / elapsed;
        printf("Performance: %.2f MIPS (actual)\n", ips / 1e6);
        printf("            %.2f instructions/second\n", ips);
    }
    
    free(cpu.decoded_code);
    free(cpu.memory);
    return cpu.regs[1];
}