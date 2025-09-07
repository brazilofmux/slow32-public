#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <getopt.h>
#include <time.h>
#include <inttypes.h>
#include "slow32.h"
#include "s32x_loader.h"

// Enable to trap on unaligned LD/ST (recommended for a strict ISA)
#ifndef S32_ALLOW_UNALIGNED
#define S32_TRAP_ON_UNALIGNED 1
#endif

static instruction_t decode_instruction(uint32_t raw) {
    instruction_t inst = {0};
    inst.raw = raw;
    inst.opcode = raw & 0x7F;
    
    switch (inst.opcode) {
        case OP_ADD ... OP_SNE:
        case OP_SGT ... OP_SGEU:
            inst.format = FMT_R;
            inst.rd = (raw >> 7) & 0x1F;
            inst.rs1 = (raw >> 15) & 0x1F;
            inst.rs2 = (raw >> 20) & 0x1F;
            break;

        case OP_ORI ... OP_ANDI:
        case OP_XORI:
        case OP_SLTIU:
            // ORI, ANDI, XORI, and SLTIU use zero-extended immediates
            inst.format = FMT_I;
            inst.rd = (raw >> 7) & 0x1F;
            inst.rs1 = (raw >> 15) & 0x1F;
            inst.imm = (raw >> 20) & 0xFFF;  // Zero-extend 12-bit immediate
            break;
            
        case OP_ADDI:
        case OP_SLLI ... OP_SLTI:
        case OP_LDB ... OP_LDHU:
        case OP_JALR:
            inst.format = FMT_I;
            inst.rd = (raw >> 7) & 0x1F;
            inst.rs1 = (raw >> 15) & 0x1F;
            inst.imm = ((int32_t)raw) >> 20;  // Sign-extend for others
            break;
            
        case OP_STB ... OP_STW:
            inst.format = FMT_S;
            inst.rs1 = (raw >> 15) & 0x1F;
            inst.rs2 = (raw >> 20) & 0x1F;
            inst.imm = ((raw >> 7) & 0x1F) | (((int32_t)raw >> 25) << 5);
            break;
            
        case OP_BEQ ... OP_BGEU:
            inst.format = FMT_B;
            inst.rs1 = (raw >> 15) & 0x1F;
            inst.rs2 = (raw >> 20) & 0x1F;
            inst.imm = (((raw >> 8) & 0xF) << 1) | (((raw >> 25) & 0x3F) << 5) |
                      (((raw >> 7) & 0x1) << 11) | (((int32_t)raw >> 31) << 12);
            break;
            
        case OP_LUI:
            inst.format = FMT_U;
            inst.rd = (raw >> 7) & 0x1F;
            inst.imm = raw & 0xFFFFF000;  // Keep immediate in bits [31:12] position
            break;
            
        case OP_JAL:
            inst.format = FMT_J;
            inst.rd = (raw >> 7) & 0x1F;
            uint32_t imm20 = (raw >> 31) & 0x1;
            uint32_t imm10_1 = (raw >> 21) & 0x3FF;
            uint32_t imm11 = (raw >> 20) & 0x1;
            uint32_t imm19_12 = (raw >> 12) & 0xFF;
            inst.imm = (imm20 << 20) | (imm19_12 << 12) | (imm11 << 11) | (imm10_1 << 1);
            if (inst.imm & 0x100000) inst.imm |= 0xFFE00000;
            break;
            
        case OP_NOP:
        case OP_YIELD:
        case OP_DEBUG:
        case OP_HALT:
        case OP_ASSERT_EQ:
            inst.format = FMT_R;
            inst.rs1 = (raw >> 15) & 0x1F;
            if (inst.opcode == OP_ASSERT_EQ) {
                inst.rs2 = (raw >> 20) & 0x1F;  // ASSERT_EQ needs both rs1 and rs2
            }
            break;
    }
    
    return inst;
}

void cpu_init(cpu_state_t *cpu) {
    cpu->memory = calloc(1, DEFAULT_MEM_SIZE);
    if (!cpu->memory) {
        fprintf(stderr, "Failed to allocate memory\n");
        exit(1);
    }
    cpu->regs[REG_SP] = 0x0FFFFFF0;
    cpu->pc = 0;
    cpu->cycle_count = 0;
    cpu->halted = false;
    
    // Default memory protection limits (updated by .s32x loader)
    cpu->code_limit = CODE_SIZE;
    cpu->rodata_limit = CODE_SIZE;
    cpu->data_limit = DEFAULT_MEM_SIZE;
    cpu->wxorx_enabled = false;
}

void cpu_destroy(cpu_state_t *cpu) {
    if (cpu->memory) {
        free(cpu->memory);
        cpu->memory = NULL;
    }
}

// Removed legacy v2 format support

// S32X executable loader - the ONLY supported format
bool cpu_load_binary(cpu_state_t *cpu, const char *filename, uint32_t load_addr) {
    (void)load_addr;  // Unused - S32X defines its own memory layout
    
    s32x_loader_config_t config = {
        .memory = (uint32_t*)cpu->memory,
        .mem_size = DEFAULT_MEM_SIZE,
        .pc = &cpu->pc,
        .sp = NULL,  // We'll set SP via registers
        .registers = cpu->regs,
        .verbose = true
    };
    
    s32x_load_result_t result = load_s32x_file(filename, &config);
    if (!result.success) {
        fprintf(stderr, "Failed to load executable: %s\n", result.error_msg);
        fprintf(stderr, "Note: Only S32X format executables are supported.\n");
        fprintf(stderr, "Use: linker/s32-ld <object.o> -o <output.s32x>\n");
        return false;
    }
    
    // Update CPU state with memory protection info
    cpu->code_limit = result.code_limit;
    cpu->rodata_limit = result.rodata_limit;
    cpu->data_limit = result.data_limit;
    cpu->wxorx_enabled = result.has_wxorx;
    
    if (result.has_wxorx) {
        printf("W^X protection enabled: code limit = 0x%08X\n", result.code_limit);
    }
    
    return true;
}

void cpu_reset(cpu_state_t *cpu, uint32_t entry_point) {
    memset(cpu->regs, 0, sizeof(cpu->regs));
    cpu->regs[REG_SP] = 0x0FFFFFF0;
    cpu->pc = entry_point;
    cpu->cycle_count = 0;
    cpu->halted = false;
}

static uint32_t mem_read(cpu_state_t *cpu, uint32_t addr, int size) {
    if (addr > DEFAULT_MEM_SIZE - (uint32_t)size
#if S32_TRAP_ON_UNALIGNED
        || (size==4 && (addr&3)) || (size==2 && (addr&1))
#endif
       ) {
        fprintf(stderr, "Memory read out of bounds: 0x%08X\n", addr);
        cpu->halted = true;
        return 0;
    }
    
    uint32_t val = 0;
    switch (size) {
        case 1:
            val = cpu->memory[addr];
            break;
        case 2:
            val = *(uint16_t*)(cpu->memory + addr);
            break;
        case 4:
            val = *(uint32_t*)(cpu->memory + addr);
            break;
    }
    
    // Memory watch
    if (cpu->debug.watch_enabled && addr >= cpu->debug.watch_start && addr < cpu->debug.watch_end) {
        printf("[MEM READ ] 0x%08X: 0x%0*X (size=%d)\n", addr, size*2, val, size);
    }
    
    cpu->cycle_count += 3;
    return val;
}

static void mem_write(cpu_state_t *cpu, uint32_t addr, uint32_t val, int size) {
    if (addr > DEFAULT_MEM_SIZE - (uint32_t)size
#if S32_TRAP_ON_UNALIGNED
        || (size==4 && (addr&3)) || (size==2 && (addr&1))
#endif
       ) {
        fprintf(stderr, "Memory write out of bounds: 0x%08X\n", addr);
        cpu->halted = true;
        return;
    }
    
    // W^X protection: Cannot write to code or rodata segments
    if (cpu->wxorx_enabled && addr < cpu->rodata_limit) {
        fprintf(stderr, "Security violation: Attempt to write to protected memory at 0x%08X\n", addr);
        cpu->halted = true;
        return;
    }
    
    // Memory watch - before write
    if (cpu->debug.watch_enabled && addr >= cpu->debug.watch_start && addr < cpu->debug.watch_end) {
        printf("[MEM WRITE] 0x%08X: 0x%0*X (size=%d)\n", addr, size*2, val, size);
    }
    
    switch (size) {
        case 1:
            cpu->memory[addr] = val & 0xFF;
            break;
        case 2:
            *(uint16_t*)(cpu->memory + addr) = val & 0xFFFF;
            break;
        case 4:
            *(uint32_t*)(cpu->memory + addr) = val;
            break;
    }
    
    cpu->cycle_count += 3;
}

// Debug helper: Get instruction name
static const char* get_opcode_name(opcode_t op) {
    switch(op) {
        case OP_ADD: return "ADD";
        case OP_SUB: return "SUB";
        case OP_XOR: return "XOR";
        case OP_OR: return "OR";
        case OP_AND: return "AND";
        case OP_SLL: return "SLL";
        case OP_SRL: return "SRL";
        case OP_SRA: return "SRA";
        case OP_SLT: return "SLT";
        case OP_SLTU: return "SLTU";
        case OP_MUL: return "MUL";
        case OP_MULH: return "MULH";
        case OP_DIV: return "DIV";
        case OP_REM: return "REM";
        case OP_SEQ: return "SEQ";
        case OP_SNE: return "SNE";
        case OP_ADDI: return "ADDI";
        case OP_ORI: return "ORI";
        case OP_ANDI: return "ANDI";
        case OP_XORI: return "XORI";
        case OP_SLLI: return "SLLI";
        case OP_SRLI: return "SRLI";
        case OP_SRAI: return "SRAI";
        case OP_SLTI: return "SLTI";
        case OP_SLTIU: return "SLTIU";
        case OP_LUI: return "LUI";
        case OP_LDB: return "LDB";
        case OP_LDH: return "LDH";
        case OP_LDW: return "LDW";
        case OP_LDBU: return "LDBU";
        case OP_LDHU: return "LDHU";
        case OP_STB: return "STB";
        case OP_STH: return "STH";
        case OP_STW: return "STW";
        case OP_JAL: return "JAL";
        case OP_JALR: return "JALR";
        case OP_BEQ: return "BEQ";
        case OP_BNE: return "BNE";
        case OP_BLT: return "BLT";
        case OP_BGE: return "BGE";
        case OP_BLTU: return "BLTU";
        case OP_BGEU: return "BGEU";
        case OP_NOP: return "NOP";
        case OP_HALT: return "HALT";
        default: return "UNKNOWN";
    }
}

// Debug helper: Format instruction for display
static void format_instruction(char *buf, size_t size, instruction_t *inst, uint32_t pc) {
    const char *name = get_opcode_name(inst->opcode);
    
    switch(inst->format) {
        case FMT_R:
            snprintf(buf, size, "%-6s r%d, r%d, r%d", name, inst->rd, inst->rs1, inst->rs2);
            break;
        case FMT_I:
            if (inst->opcode >= OP_LDB && inst->opcode <= OP_LDHU) {
                snprintf(buf, size, "%-6s r%d, r%d+%d", name, inst->rd, inst->rs1, inst->imm);
            } else {
                snprintf(buf, size, "%-6s r%d, r%d, %d", name, inst->rd, inst->rs1, inst->imm);
            }
            break;
        case FMT_S:
            snprintf(buf, size, "%-6s r%d+%d, r%d", name, inst->rs1, inst->imm, inst->rs2);
            break;
        case FMT_B:
            snprintf(buf, size, "%-6s r%d, r%d, 0x%x", name, inst->rs1, inst->rs2, pc + inst->imm);
            break;
        case FMT_U:
            snprintf(buf, size, "%-6s r%d, 0x%x", name, inst->rd, inst->imm >> 12);
            break;
        case FMT_J:
            snprintf(buf, size, "%-6s r%d, 0x%x", name, inst->rd, pc + inst->imm);
            break;
        default:
            snprintf(buf, size, "%-6s", name);
            break;
    }
}

void cpu_step(cpu_state_t *cpu) {
    if (cpu->halted) return;
    
    // Check cycle limit
    if (cpu->debug.max_cycles > 0 && cpu->cycle_count >= cpu->debug.max_cycles) {
        fprintf(stderr, "\nCycle limit reached (%" PRIu64 " cycles)\n", cpu->cycle_count);
        fprintf(stderr, "Stopped at PC=0x%08X\n", cpu->pc);
        cpu->halted = true;
        return;
    }
    
    // Check breakpoints
    for (int i = 0; i < cpu->debug.num_breakpoints; i++) {
        if (cpu->pc == cpu->debug.breakpoints[i]) {
            fprintf(stderr, "\nBreakpoint hit at PC=0x%08X\n", cpu->pc);
            cpu->halted = true;
            return;
        }
    }
    
    if (cpu->pc >= cpu->code_limit) {
        fprintf(stderr, "Execute from non-code at 0x%08X\n", cpu->pc);
        cpu->halted = true;
        return;
    }
    
    // Save previous register values for change detection
    if (cpu->debug.show_reg_changes) {
        memcpy(cpu->debug.prev_regs, cpu->regs, sizeof(cpu->regs));
    }
    
    uint32_t inst_raw = *(uint32_t*)(cpu->memory + cpu->pc);
    instruction_t inst = decode_instruction(inst_raw);
    
    // Trace instruction before execution
    if (cpu->debug.trace_instructions) {
        char inst_str[80];
        format_instruction(inst_str, sizeof(inst_str), &inst, cpu->pc);
        printf("[0x%08X] %-30s", cpu->pc, inst_str);
        
        // Show source register values
        if (inst.format == FMT_R || inst.format == FMT_I || inst.format == FMT_B) {
            printf(" ; r%d=0x%08X", inst.rs1, cpu->regs[inst.rs1]);
            if (inst.format == FMT_R || inst.format == FMT_B) {
                printf(", r%d=0x%08X", inst.rs2, cpu->regs[inst.rs2]);
            }
        }
    }
    
    uint32_t next_pc = cpu->pc + 4;
    uint32_t rs1_val = cpu->regs[inst.rs1];
    uint32_t rs2_val = cpu->regs[inst.rs2];
    
    switch (inst.opcode) {
        case OP_ADD:
            cpu->regs[inst.rd] = rs1_val + rs2_val;
            break;
        case OP_SUB:
            cpu->regs[inst.rd] = rs1_val - rs2_val;
            break;
        case OP_XOR:
            cpu->regs[inst.rd] = rs1_val ^ rs2_val;
            break;
        case OP_OR:
            cpu->regs[inst.rd] = rs1_val | rs2_val;
            break;
        case OP_AND:
            cpu->regs[inst.rd] = rs1_val & rs2_val;
            break;
        case OP_SLL:
            cpu->regs[inst.rd] = rs1_val << (rs2_val & 0x1F);
            break;
        case OP_SRL:
            cpu->regs[inst.rd] = rs1_val >> (rs2_val & 0x1F);
            break;
        case OP_SRA:
            cpu->regs[inst.rd] = (int32_t)rs1_val >> (rs2_val & 0x1F);
            break;
        case OP_SLT:
            cpu->regs[inst.rd] = ((int32_t)rs1_val < (int32_t)rs2_val) ? 1 : 0;
            break;
        case OP_SLTU:
            cpu->regs[inst.rd] = (rs1_val < rs2_val) ? 1 : 0;
            break;
        case OP_MUL:
            cpu->regs[inst.rd] = rs1_val * rs2_val;
            cpu->cycle_count += 31;
            break;
        case OP_MULH:
            cpu->regs[inst.rd] = ((int64_t)(int32_t)rs1_val * (int64_t)(int32_t)rs2_val) >> 32;
            cpu->cycle_count += 31;
            break;
        case OP_DIV:
            if (rs2_val != 0) {
                cpu->regs[inst.rd] = (int32_t)rs1_val / (int32_t)rs2_val;
            } else {
                cpu->regs[inst.rd] = -1;
            }
            cpu->cycle_count += 63;
            break;
        case OP_REM:
            if (rs2_val != 0) {
                cpu->regs[inst.rd] = (int32_t)rs1_val % (int32_t)rs2_val;
            } else {
                cpu->regs[inst.rd] = rs1_val;
            }
            cpu->cycle_count += 63;
            break;
        case OP_SEQ:
            cpu->regs[inst.rd] = (rs1_val == rs2_val) ? 1 : 0;
            break;
        case OP_SNE:
            cpu->regs[inst.rd] = (rs1_val != rs2_val) ? 1 : 0;
            break;
        case OP_SGT:
            cpu->regs[inst.rd] = ((int32_t)rs1_val > (int32_t)rs2_val) ? 1 : 0;
            break;
        case OP_SGTU:
            cpu->regs[inst.rd] = (rs1_val > rs2_val) ? 1 : 0;
            break;
        case OP_SLE:
            cpu->regs[inst.rd] = ((int32_t)rs1_val <= (int32_t)rs2_val) ? 1 : 0;
            break;
        case OP_SLEU:
            cpu->regs[inst.rd] = (rs1_val <= rs2_val) ? 1 : 0;
            break;
        case OP_SGE:
            cpu->regs[inst.rd] = ((int32_t)rs1_val >= (int32_t)rs2_val) ? 1 : 0;
            break;
        case OP_SGEU:
            cpu->regs[inst.rd] = (rs1_val >= rs2_val) ? 1 : 0;
            break;
            
        case OP_ADDI:
            cpu->regs[inst.rd] = rs1_val + inst.imm;
            break;
        case OP_ORI:
            cpu->regs[inst.rd] = rs1_val | inst.imm;
            break;
        case OP_ANDI:
            cpu->regs[inst.rd] = rs1_val & inst.imm;
            break;
        case OP_XORI:
            cpu->regs[inst.rd] = rs1_val ^ inst.imm;
            break;
        case OP_SLLI:
            cpu->regs[inst.rd] = rs1_val << (inst.imm & 0x1F);
            break;
        case OP_SRLI:
            cpu->regs[inst.rd] = rs1_val >> (inst.imm & 0x1F);
            break;
        case OP_SRAI:
            cpu->regs[inst.rd] = (int32_t)rs1_val >> (inst.imm & 0x1F);
            break;
        case OP_SLTI:
            cpu->regs[inst.rd] = ((int32_t)rs1_val < inst.imm) ? 1 : 0;
            break;
        case OP_SLTIU:
            cpu->regs[inst.rd] = (rs1_val < (uint32_t)inst.imm) ? 1 : 0;
            break;
            
        case OP_LUI:
            cpu->regs[inst.rd] = inst.imm;  // imm already has bits [31:12] set correctly
            break;
            
        case OP_LDB:
            cpu->regs[inst.rd] = (int8_t)mem_read(cpu, rs1_val + inst.imm, 1);
            break;
        case OP_LDH:
            cpu->regs[inst.rd] = (int16_t)mem_read(cpu, rs1_val + inst.imm, 2);
            break;
        case OP_LDW:
            cpu->regs[inst.rd] = mem_read(cpu, rs1_val + inst.imm, 4);
            break;
        case OP_LDBU:
            cpu->regs[inst.rd] = mem_read(cpu, rs1_val + inst.imm, 1);
            break;
        case OP_LDHU:
            cpu->regs[inst.rd] = mem_read(cpu, rs1_val + inst.imm, 2);
            break;
            
        case OP_STB:
            mem_write(cpu, rs1_val + inst.imm, rs2_val, 1);
            break;
        case OP_STH:
            mem_write(cpu, rs1_val + inst.imm, rs2_val, 2);
            break;
        case OP_STW:
            mem_write(cpu, rs1_val + inst.imm, rs2_val, 4);
            break;
            
        case OP_JAL:
            cpu->regs[inst.rd] = next_pc;
            next_pc = cpu->pc + inst.imm;
            break;
        case OP_JALR:
            cpu->regs[inst.rd] = next_pc;
            next_pc = (rs1_val + inst.imm) & ~1;
            break;
            
        case OP_BEQ:
            if (rs1_val == rs2_val) {
                if (getenv("SLOW32_DEBUG_BRANCH")) {
                    printf("BEQ: pc=%08x imm=%d base=%08x target=%08x (taken)\n", 
                           cpu->pc, inst.imm, next_pc, next_pc + inst.imm);
                }
                next_pc = next_pc + inst.imm;  // PC+4 base!
            }
            break;
        case OP_BNE:
            if (rs1_val != rs2_val) next_pc = next_pc + inst.imm;  // PC+4 base!
            break;
        case OP_BLT:
            if ((int32_t)rs1_val < (int32_t)rs2_val) next_pc = next_pc + inst.imm;  // PC+4 base!
            break;
        case OP_BGE:
            if ((int32_t)rs1_val >= (int32_t)rs2_val) next_pc = next_pc + inst.imm;  // PC+4 base!
            break;
        case OP_BLTU:
            if (rs1_val < rs2_val) next_pc = next_pc + inst.imm;  // PC+4 base!
            break;
        case OP_BGEU:
            if (rs1_val >= rs2_val) next_pc = next_pc + inst.imm;  // PC+4 base!
            break;
            
        case OP_NOP:
            break;
        case OP_YIELD:
            cpu->cycle_count += rs1_val;
            break;
        case OP_DEBUG:
            putchar(rs1_val & 0xFF);
            fflush(stdout);
            break;
        case OP_HALT:
            cpu->halted = true;
            printf("HALT at PC=0x%08X after %" PRIu64 " cycles\n", cpu->pc, cpu->cycle_count);
            return;
            
        case 0x3F:  // ASSERT_EQ - for testing
            if (rs1_val != rs2_val) {
                fprintf(stderr, "ASSERT r%d==r%d failed @PC=%08x (%08x!=%08x)\n",
                        inst.rs1, inst.rs2, cpu->pc, rs1_val, rs2_val);
                cpu->halted = true;
                cpu->regs[1] = 0xDEADBEEF;  // Signal assertion failure
                return;
            }
            break;
            
        default:
            fprintf(stderr, "Unknown opcode: 0x%02X at PC=0x%08X\n", inst.opcode, cpu->pc);
            cpu->halted = true;
            return;
    }
    
    cpu->regs[REG_ZERO] = 0;
    
    // Complete instruction trace
    if (cpu->debug.trace_instructions) {
        // Show result for instructions that write to a register
        if (inst.rd != 0 && inst.format != FMT_S && inst.format != FMT_B && inst.opcode != OP_NOP) {
            printf(" => r%d=0x%08X", inst.rd, cpu->regs[inst.rd]);
        }
        printf("\n");
    }
    
    // Show register changes
    if (cpu->debug.show_reg_changes && !cpu->debug.trace_instructions) {
        bool any_changed = false;
        for (int i = 1; i < NUM_REGS; i++) {  // Skip r0
            if (cpu->regs[i] != cpu->debug.prev_regs[i]) {
                if (!any_changed) {
                    printf("[0x%08X] ", cpu->pc);
                    any_changed = true;
                }
                printf("r%d: 0x%08X -> 0x%08X  ", i, cpu->debug.prev_regs[i], cpu->regs[i]);
            }
        }
        if (any_changed) printf("\n");
    }
    
    cpu->pc = next_pc;
    cpu->cycle_count++;
}

void cpu_run(cpu_state_t *cpu) {
    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start);
    
    uint64_t inst_count = 0;
    while (!cpu->halted) {
        cpu_step(cpu);
        inst_count++;
    }
    
    clock_gettime(CLOCK_MONOTONIC, &end);
    double elapsed = (end.tv_sec - start.tv_sec) + (end.tv_nsec - start.tv_nsec) / 1e9;
    
    if (getenv("SHOW_PERF")) {
        printf("\nPerformance: %.2f MIPS (%.0f inst/sec)\n", 
               inst_count / elapsed / 1e6, inst_count / elapsed);
    }
}

void cpu_dump_regs(cpu_state_t *cpu) {
    printf("\nFinal state:\n");
    printf("PC=0x%08X  Cycles=%" PRIu64 "\n", cpu->pc, cpu->cycle_count);
    for (int i = 0; i < NUM_REGS; i += 4) {
        printf("r%02d=0x%08X  r%02d=0x%08X  r%02d=0x%08X  r%02d=0x%08X\n",
               i, cpu->regs[i], i+1, cpu->regs[i+1],
               i+2, cpu->regs[i+2], i+3, cpu->regs[i+3]);
    }
}

void cpu_dump_mem(cpu_state_t *cpu, uint32_t addr, uint32_t size) {
    printf("Memory dump at 0x%08X:\n", addr);
    for (uint32_t i = 0; i < size; i += 16) {
        printf("%08X: ", addr + i);
        for (int j = 0; j < 16 && (i + j) < size; j++) {
            printf("%02X ", cpu->memory[addr + i + j]);
        }
        printf("\n");
    }
}

int main(int argc, char *argv[]) {
    bool step_mode = false;
    bool dump_regs = true;
    bool trace = false;
    bool show_changes = false;
    uint64_t max_cycles = 0;
    uint32_t breakpoint_addr = 0;
    uint32_t watch_start = 0, watch_end = 0;
    
    int opt;
    while ((opt = getopt(argc, argv, "sthrc:b:w:")) != -1) {
        switch (opt) {
            case 's':
                step_mode = true;
                break;
            case 't':
                trace = true;
                break;
            case 'r':
                show_changes = true;
                break;
            case 'c':
                max_cycles = strtoull(optarg, NULL, 0);
                break;
            case 'b':
                breakpoint_addr = strtoul(optarg, NULL, 0);
                break;
            case 'w': {
                char *dash = strchr(optarg, '-');
                if (dash) {
                    *dash = '\0';
                    watch_start = strtoul(optarg, NULL, 0);
                    watch_end = strtoul(dash + 1, NULL, 0);
                } else {
                    fprintf(stderr, "Error: -w requires range (e.g., -w 0x1000-0x2000)\n");
                    return 1;
                }
                break;
            }
            case 'h':
                printf("Usage: %s [options] <binary>\n", argv[0]);
                printf("Options:\n");
                printf("  -s          Step mode (press Enter for each instruction)\n");
                printf("  -t          Trace mode (print each instruction)\n");
                printf("  -r          Show register changes only\n");
                printf("  -c <cycles> Limit execution to N cycles\n");
                printf("  -b <addr>   Set breakpoint at address\n");
                printf("  -w <range>  Watch memory range (e.g., -w 0x1000-0x2000)\n");
                printf("  -h          Show this help\n");
                return 0;
            default:
                fprintf(stderr, "Usage: %s [options] <binary>\n", argv[0]);
                fprintf(stderr, "Try '%s -h' for help\n", argv[0]);
                return 1;
        }
    }
    
    if (optind >= argc) {
        fprintf(stderr, "Error: No binary file specified\n");
        fprintf(stderr, "Usage: %s [-s] <binary>\n", argv[0]);
        return 1;
    }
    
    cpu_state_t cpu = {0};
    cpu_init(&cpu);
    
    // Configure debug options
    cpu.debug.trace_instructions = trace;
    cpu.debug.show_reg_changes = show_changes;
    cpu.debug.max_cycles = max_cycles;
    if (breakpoint_addr) {
        cpu.debug.breakpoints[0] = breakpoint_addr;
        cpu.debug.num_breakpoints = 1;
    }
    if (watch_start && watch_end) {
        cpu.debug.watch_start = watch_start;
        cpu.debug.watch_end = watch_end;
        cpu.debug.watch_enabled = true;
    }
    
    if (!cpu_load_binary(&cpu, argv[optind], 0)) {
        cpu_destroy(&cpu);
        return 1;
    }
    
    printf("Starting execution at 0x%08X\n", cpu.pc);
    if (max_cycles > 0) {
        printf("Cycle limit: %" PRIu64 "\n", max_cycles);
    }
    
    if (step_mode) {
        cpu.debug.step_mode = true;
        printf("Step mode enabled. Press Enter to execute each instruction.\n");
        while (!cpu.halted) {
            printf("PC=0x%08X: ", cpu.pc);
            getchar();
            cpu_step(&cpu);
        }
    } else {
        cpu_run(&cpu);
    }
    
    if (dump_regs) {
        cpu_dump_regs(&cpu);
    }
    
    cpu_destroy(&cpu);
    return 0;
}
