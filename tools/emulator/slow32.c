#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <getopt.h>
#include <time.h>
#include <inttypes.h>
#include <sys/mman.h>
#include <math.h>
#include "slow32.h"
#include "s32x_loader.h"
#include "memory_manager.h"
#include "mmio_ring.h"

// Enable to trap on unaligned LD/ST (recommended for a strict ISA)
#ifndef S32_ALLOW_UNALIGNED
#define S32_TRAP_ON_UNALIGNED 1
#endif

// Enable to trap on odd register numbers for f64 operations.
// The ISA requires even-numbered registers for double-precision pairs.
// x86-64 won't crash on regs[31+1], but it silently corrupts the PC field.
#ifndef S32_CHECK_F64_REGS
#define S32_CHECK_F64_REGS 1
#endif

// CPU state is now defined in slow32.h with memory_manager_t

// Initialize MMIO on first YIELD
static void cpu_init_mmio(cpu_state_t *cpu) {
    if (cpu->mmio.initialized) return;

    // Allocate MMIO state
    cpu->mmio.state = calloc(1, sizeof(mmio_ring_state_t));
    if (!cpu->mmio.state) {
        fprintf(stderr, "Failed to allocate MMIO state\n");
        cpu->mmio.enabled = false;
        return;
    }

    // Initialize with heap after data limit
    uint32_t heap_base = (cpu->data_limit + 0xFFF) & ~0xFFF;  // Page align
    uint32_t heap_size = 0x01000000;  // 16MB heap by default
    mmio_ring_init(cpu->mmio.state, heap_base, heap_size);

    // Map MMIO memory
    cpu->mmio.mem = mmio_ring_map(cpu->mmio.state);
    if (!cpu->mmio.mem) {
        fprintf(stderr, "Failed to map MMIO memory\n");
        mmio_ring_clear_args(cpu->mmio.state);
        free(cpu->mmio.state);
        cpu->mmio.state = NULL;
        cpu->mmio.enabled = false;
        return;
    }

    // Add MMIO as a memory region
    memory_region_t *mmio_region = mm_allocate_region(&cpu->mm,
                                                      cpu->mmio.base,
                                                      0x10000,  // 64KB
                                                      PROT_READ | PROT_WRITE);
    if (!mmio_region) {
        munmap(cpu->mmio.mem, 0x10000);
        mmio_ring_clear_args(cpu->mmio.state);
        free(cpu->mmio.state);
        cpu->mmio.state = NULL;
        cpu->mmio.mem = NULL;
        cpu->mmio.enabled = false;
        return;
    }

    mmio_region->host_addr = cpu->mmio.mem;
    cpu->mmio.state->base_addr = cpu->mmio.base;
    cpu->mmio.initialized = true;
}

static instruction_t decode_instruction(uint32_t raw) {
    instruction_t inst = {0};
    inst.raw = raw;
    inst.opcode = raw & 0x7F;
    
    switch (inst.opcode) {
        case OP_ADD ... OP_SNE:
        case OP_SGT ... OP_SGEU:
        case OP_MULHU:
        case OP_FADD_S ... OP_FCVT_D_LU:
            inst.format = FMT_R;
            inst.rd = (raw >> 7) & 0x1F;
            inst.rs1 = (raw >> 15) & 0x1F;
            inst.rs2 = (raw >> 20) & 0x1F;
            break;

        case OP_ORI ... OP_ANDI:
        case OP_XORI:
        case OP_SLTIU:
            inst.format = FMT_I;
            inst.rd = (raw >> 7) & 0x1F;
            inst.rs1 = (raw >> 15) & 0x1F;
            inst.imm = (raw >> 20) & 0xFFF;  // Zero-extend
            break;
            
        case OP_ADDI:
        case OP_SLLI ... OP_SLTI:
        case OP_LDB ... OP_LDHU:
        case OP_JALR:
            inst.format = FMT_I;
            inst.rd = (raw >> 7) & 0x1F;
            inst.rs1 = (raw >> 15) & 0x1F;
            inst.imm = ((int32_t)raw) >> 20;  // Sign-extend
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
            inst.imm = raw & 0xFFFFF000;
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
                inst.rs2 = (raw >> 20) & 0x1F;
            }
            break;
    }
    
    return inst;
}

void cpu_init(cpu_state_t *cpu) {
    memset(cpu, 0, sizeof(*cpu));
    mm_init(&cpu->mm, false);  // W^X will be set by loader
    cpu->regs[REG_SP] = 0x0FFFFFF0;
    cpu->pc = 0;
    cpu->cycle_count = 0;
    cpu->inst_count = 0;
    cpu->halted = false;
    
    // MMIO auto-detection fields
    cpu->mmio.enabled = false;
    cpu->mmio.initialized = false;
    cpu->mmio.base = 0;
    cpu->mmio.state = NULL;
    cpu->mmio.mem = NULL;
}

void cpu_destroy(cpu_state_t *cpu) {
    // Clean up MMIO if initialized
    if (cpu->mmio.mem) {
        munmap(cpu->mmio.mem, 0x10000);
        cpu->mmio.mem = NULL;
    }
    if (cpu->mmio.state) {
        mmio_ring_clear_args(cpu->mmio.state);
        free(cpu->mmio.state);
        cpu->mmio.state = NULL;
    }
    
    mm_destroy(&cpu->mm);
}

static int mm_write_callback(void *user_data, uint32_t addr, const void *data, uint32_t size) {
    cpu_state_t *cpu = (cpu_state_t *)user_data;
    return mm_write(&cpu->mm, addr, data, size);
}

// Modified loader to use memory manager
bool cpu_load_binary(cpu_state_t *cpu, const char *filename) {
    // Read header first to set up memory regions before loading sections
    s32x_load_result_t hdr = load_s32x_header(filename);
    if (!hdr.success) {
        fprintf(stderr, "Header error: %s\n", hdr.error_msg);
        return false;
    }

    // Setup memory manager from header metadata
    uint32_t mmio_size = hdr.has_mmio ? 0x10000u : 0u;
    if (mm_setup_from_s32x(&cpu->mm, hdr.code_limit, hdr.rodata_limit,
                          hdr.data_limit, hdr.stack_base, hdr.stack_end,
                          hdr.mmio_base, mmio_size) < 0) {
        fprintf(stderr, "Failed to allocate memory regions\n");
        return false;
    }

    // Load sections via callback into memory manager
    s32x_loader_config_t config = {
        .write_cb = mm_write_callback,
        .user_data = cpu,
        .mem_size = DEFAULT_MEM_SIZE,
        .verbose = cpu->debug.trace_instructions
    };

    s32x_load_result_t result = load_s32x_file(filename, &config);
    if (!result.success) {
        fprintf(stderr, "Loader error: %s\n", result.error_msg);
        return false;
    }

    // Set W^X policy before protecting regions
    cpu->mm.wxorx_enabled = result.has_wxorx;

    // Apply memory protection (code and rodata become read-only)
    if (mm_protect_regions(&cpu->mm, result.code_limit, result.rodata_limit) < 0) {
        fprintf(stderr, "Failed to set memory protection\n");
        return false;
    }

    // Cache the code region for fast instruction fetch
    cpu->code_region = mm_find_region(&cpu->mm, result.entry_point);
    if (!cpu->code_region) {
        for (memory_region_t *r = cpu->mm.regions; r; r = r->next) {
            if (r->vaddr_start == 0) {
                cpu->code_region = r;
                break;
            }
        }
    }

    // Set up CPU state
    cpu->pc = result.entry_point;
    if (cpu->pc >= result.code_limit) {
        fprintf(stderr, "Invalid entry point 0x%08X outside code segment [0, 0x%08X)\n",
                cpu->pc, result.code_limit);
        return false;
    }
    cpu->regs[REG_SP] = result.stack_base;
    cpu->code_limit = result.code_limit;
    cpu->rodata_limit = result.rodata_limit;
    cpu->data_limit = result.data_limit;
    cpu->wxorx_enabled = result.has_wxorx;

    if (result.has_mmio) {
        cpu->mmio.enabled = true;
        cpu->mmio.base = result.mmio_base;
        cpu_init_mmio(cpu);
    }

    return true;
}

// Memory access functions using memory manager
static uint32_t cpu_load(cpu_state_t *cpu, uint32_t addr, int size) {
    uint32_t value = 0;
    
    // Check if this is MMIO access (only if MMIO is configured)
    if (cpu->mmio.base != 0 && addr >= cpu->mmio.base && addr < cpu->mmio.base + 0x10000) {
        if (!cpu->mmio.enabled || !cpu->mmio.initialized) {
            fprintf(stderr, "MMIO fault: read at 0x%08X with MMIO disabled\n", addr);
            cpu->halted = true;
            return 0;
        }

        if (size == 4) {
            value = mmio_ring_read(cpu->mmio.state, addr, size);
        } else {
            // For byte/halfword MMIO reads, read word and extract
            uint32_t word_addr = addr & ~3;
            uint32_t word = mmio_ring_read(cpu->mmio.state, word_addr, 4);
            int shift = (addr & 3) * 8;
            if (size == 1) {
                value = (word >> shift) & 0xFF;
            } else if (size == 2) {
                value = (word >> shift) & 0xFFFF;
            }
        }
        return value;
    }
    
    if (mm_read(&cpu->mm, addr, &value, size) < 0) {
        fprintf(stderr, "Memory fault: Failed to read %d bytes at 0x%08X (PC=0x%08X SP=0x%08X)\n",
                size, addr, cpu->pc, cpu->regs[REG_SP]);
        cpu->halted = true;
        return 0;
    }
    
    // Handle partial reads for byte/halfword
    if (size == 1) {
        value &= 0xFF;
    } else if (size == 2) {
        value &= 0xFFFF;
    }
    
    return value;
}

static void cpu_store(cpu_state_t *cpu, uint32_t addr, uint32_t value, int size) {
    // Check if this is MMIO access (only if MMIO is configured)
    if (cpu->mmio.base != 0 && addr >= cpu->mmio.base && addr < cpu->mmio.base + 0x10000) {
        if (!cpu->mmio.enabled || !cpu->mmio.initialized) {
            fprintf(stderr, "MMIO fault: write at 0x%08X with MMIO disabled\n", addr);
            cpu->halted = true;
            return;
        }

        if (size == 4) {
            mmio_ring_write(cpu->mmio.state, NULL, addr, value, size);
        } else {
            // For byte/halfword MMIO writes, do read-modify-write
            uint32_t word_addr = addr & ~3;
            uint32_t word = mmio_ring_read(cpu->mmio.state, word_addr, 4);
            int shift = (addr & 3) * 8;
            
            if (size == 1) {
                word = (word & ~(0xFF << shift)) | ((value & 0xFF) << shift);
            } else if (size == 2) {
                word = (word & ~(0xFFFF << shift)) | ((value & 0xFFFF) << shift);
            }
            
            mmio_ring_write(cpu->mmio.state, NULL, word_addr, word, 4);
        }
        return;
    }
    
    // Store to memory
    // Prepare data based on size
    uint32_t data = value;
    if (size == 1) {
        data &= 0xFF;
    } else if (size == 2) {
        data &= 0xFFFF;
    }
    
    if (mm_write(&cpu->mm, addr, &data, size) < 0) {
        fprintf(stderr, "Memory fault: Failed to write %d bytes at 0x%08X (PC=0x%08X SP=0x%08X)\n",
                size, addr, cpu->pc, cpu->regs[REG_SP]);
        cpu->halted = true;
    }
}

// Instruction fetch - optimized with single boundary check and fast path
static uint32_t cpu_fetch(cpu_state_t *cpu, uint32_t addr) {
    // Single boundary check: ensure we can fetch 4 bytes of instruction
    // This protects against executing data/stack and wild jumps
    // code_limit is first byte AFTER code, so last valid inst is at code_limit-4
    if (addr >= cpu->code_limit) {
        fprintf(stderr, "Execute fault: PC=0x%08X outside code segment [0, 0x%08X)\n", 
                addr, cpu->code_limit);
        cpu->halted = true;
        return 0;
    }
    
    // Fast path: use cached code region pointer
    if (cpu->code_region &&
        addr >= cpu->code_region->vaddr_start &&
        addr + 4 <= cpu->code_region->vaddr_end) {
        uint32_t inst;
        uint32_t offset = addr - cpu->code_region->vaddr_start;
        memcpy(&inst, (uint8_t*)cpu->code_region->host_addr + offset, 4);
        return inst;
    }
    
    // Fallback to mm_read (shouldn't happen with valid .s32x)
    uint32_t inst = 0;
    if (mm_read(&cpu->mm, addr, &inst, 4) < 0) {
        fprintf(stderr, "Fetch fault: Cannot read instruction at 0x%08X\n", addr);
        cpu->halted = true;
        return 0;
    }
    return inst;
}

void cpu_step(cpu_state_t *cpu) {
    // Execute instruction
    if (cpu->halted) return;
    
    // Check breakpoints
    for (int i = 0; i < cpu->debug.num_breakpoints; i++) {
        if (cpu->pc == cpu->debug.breakpoints[i]) {
            printf("Breakpoint hit at 0x%08X\n", cpu->pc);
            cpu->debug.step_mode = true;
        }
    }
    
    // Fetch and decode instruction
    uint32_t raw_inst = cpu_fetch(cpu, cpu->pc);
    if (cpu->halted) return;
    
    instruction_t inst = decode_instruction(raw_inst);
    
    if (cpu->debug.trace_instructions) {
        printf("[%08" PRIx64 "] PC=%08X: %08X ", cpu->cycle_count, cpu->pc, raw_inst);
    }
    
    // Save current register state for change detection
    if (cpu->debug.show_reg_changes) {
        memcpy(cpu->debug.prev_regs, cpu->regs, sizeof(cpu->regs));
    }
    
    uint32_t next_pc = cpu->pc + 4;
    cpu->regs[REG_ZERO] = 0;  // Ensure r0 is always 0
    
    // Execute instruction
    switch (inst.opcode) {
        // R-type arithmetic
        case OP_ADD:
            cpu->regs[inst.rd] = cpu->regs[inst.rs1] + cpu->regs[inst.rs2];
            break;
        case OP_SUB:
            cpu->regs[inst.rd] = cpu->regs[inst.rs1] - cpu->regs[inst.rs2];
            break;
        case OP_XOR:
            cpu->regs[inst.rd] = cpu->regs[inst.rs1] ^ cpu->regs[inst.rs2];
            break;
        case OP_OR:
            cpu->regs[inst.rd] = cpu->regs[inst.rs1] | cpu->regs[inst.rs2];
            break;
        case OP_AND:
            cpu->regs[inst.rd] = cpu->regs[inst.rs1] & cpu->regs[inst.rs2];
            break;
        case OP_SLL:
            cpu->regs[inst.rd] = cpu->regs[inst.rs1] << (cpu->regs[inst.rs2] & 0x1F);
            break;
        case OP_SRL:
            cpu->regs[inst.rd] = cpu->regs[inst.rs1] >> (cpu->regs[inst.rs2] & 0x1F);
            break;
        case OP_SRA:
            cpu->regs[inst.rd] = (int32_t)cpu->regs[inst.rs1] >> (cpu->regs[inst.rs2] & 0x1F);
            break;
        case OP_SLT:
            cpu->regs[inst.rd] = ((int32_t)cpu->regs[inst.rs1] < (int32_t)cpu->regs[inst.rs2]) ? 1 : 0;
            break;
        case OP_SLTU:
            cpu->regs[inst.rd] = (cpu->regs[inst.rs1] < cpu->regs[inst.rs2]) ? 1 : 0;
            break;
        case OP_MUL:
            cpu->regs[inst.rd] = cpu->regs[inst.rs1] * cpu->regs[inst.rs2];
            break;
        case OP_MULH: {
            int64_t result = (int64_t)(int32_t)cpu->regs[inst.rs1] * (int64_t)(int32_t)cpu->regs[inst.rs2];
            cpu->regs[inst.rd] = (uint32_t)(result >> 32);
            break;
        }
        case OP_MULHU: {
            uint64_t result = (uint64_t)cpu->regs[inst.rs1] * (uint64_t)cpu->regs[inst.rs2];
            cpu->regs[inst.rd] = (uint32_t)(result >> 32);
            break;
        }
        case OP_DIV:
            if (cpu->regs[inst.rs2] == 0) {
                cpu->regs[inst.rd] = 0xFFFFFFFF;
            } else if (cpu->regs[inst.rs1] == 0x80000000 && cpu->regs[inst.rs2] == 0xFFFFFFFF) {
                cpu->regs[inst.rd] = 0x80000000;  // INT32_MIN / -1 = INT32_MIN
            } else {
                cpu->regs[inst.rd] = (int32_t)cpu->regs[inst.rs1] / (int32_t)cpu->regs[inst.rs2];
            }
            break;
        case OP_REM:
            if (cpu->regs[inst.rs2] == 0) {
                cpu->regs[inst.rd] = cpu->regs[inst.rs1];
            } else if (cpu->regs[inst.rs1] == 0x80000000 && cpu->regs[inst.rs2] == 0xFFFFFFFF) {
                cpu->regs[inst.rd] = 0;  // INT32_MIN % -1 = 0
            } else {
                cpu->regs[inst.rd] = (int32_t)cpu->regs[inst.rs1] % (int32_t)cpu->regs[inst.rs2];
            }
            break;
        case OP_SEQ:
            cpu->regs[inst.rd] = (cpu->regs[inst.rs1] == cpu->regs[inst.rs2]) ? 1 : 0;
            break;
        case OP_SNE:
            cpu->regs[inst.rd] = (cpu->regs[inst.rs1] != cpu->regs[inst.rs2]) ? 1 : 0;
            break;
        case OP_SGT:
            cpu->regs[inst.rd] = ((int32_t)cpu->regs[inst.rs1] > (int32_t)cpu->regs[inst.rs2]) ? 1 : 0;
            break;
        case OP_SGTU:
            cpu->regs[inst.rd] = (cpu->regs[inst.rs1] > cpu->regs[inst.rs2]) ? 1 : 0;
            break;
        case OP_SLE:
            cpu->regs[inst.rd] = ((int32_t)cpu->regs[inst.rs1] <= (int32_t)cpu->regs[inst.rs2]) ? 1 : 0;
            break;
        case OP_SLEU:
            cpu->regs[inst.rd] = (cpu->regs[inst.rs1] <= cpu->regs[inst.rs2]) ? 1 : 0;
            break;
        case OP_SGE:
            cpu->regs[inst.rd] = ((int32_t)cpu->regs[inst.rs1] >= (int32_t)cpu->regs[inst.rs2]) ? 1 : 0;
            break;
        case OP_SGEU:
            cpu->regs[inst.rd] = (cpu->regs[inst.rs1] >= cpu->regs[inst.rs2]) ? 1 : 0;
            break;
            
        // I-type arithmetic
        case OP_ADDI:
            cpu->regs[inst.rd] = cpu->regs[inst.rs1] + inst.imm;
            break;
        case OP_ORI:
            cpu->regs[inst.rd] = cpu->regs[inst.rs1] | inst.imm;
            break;
        case OP_ANDI:
            cpu->regs[inst.rd] = cpu->regs[inst.rs1] & inst.imm;
            break;
        case OP_XORI:
            cpu->regs[inst.rd] = cpu->regs[inst.rs1] ^ inst.imm;
            break;
        case OP_SLLI:
            cpu->regs[inst.rd] = cpu->regs[inst.rs1] << (inst.imm & 0x1F);
            break;
        case OP_SRLI:
            cpu->regs[inst.rd] = cpu->regs[inst.rs1] >> (inst.imm & 0x1F);
            break;
        case OP_SRAI:
            cpu->regs[inst.rd] = (int32_t)cpu->regs[inst.rs1] >> (inst.imm & 0x1F);
            break;
        case OP_SLTI:
            cpu->regs[inst.rd] = ((int32_t)cpu->regs[inst.rs1] < inst.imm) ? 1 : 0;
            break;
        case OP_SLTIU:
            cpu->regs[inst.rd] = (cpu->regs[inst.rs1] < (uint32_t)inst.imm) ? 1 : 0;
            break;
            
        // Load instructions
        case OP_LDB: {
            uint32_t addr = cpu->regs[inst.rs1] + inst.imm;
            uint32_t byte = cpu_load(cpu, addr, 1);
            cpu->regs[inst.rd] = (int32_t)(int8_t)byte;
            break;
        }
        case OP_LDH: {
            uint32_t addr = cpu->regs[inst.rs1] + inst.imm;
            uint32_t half = cpu_load(cpu, addr, 2);
            cpu->regs[inst.rd] = (int32_t)(int16_t)half;
            break;
        }
        case OP_LDW: {
            uint32_t addr = cpu->regs[inst.rs1] + inst.imm;
            cpu->regs[inst.rd] = cpu_load(cpu, addr, 4);
            break;
        }
        case OP_LDBU: {
            uint32_t addr = cpu->regs[inst.rs1] + inst.imm;
            cpu->regs[inst.rd] = cpu_load(cpu, addr, 1);
            break;
        }
        case OP_LDHU: {
            uint32_t addr = cpu->regs[inst.rs1] + inst.imm;
            cpu->regs[inst.rd] = cpu_load(cpu, addr, 2);
            break;
        }
            
        // Store instructions
        case OP_STB: {
            uint32_t addr = cpu->regs[inst.rs1] + inst.imm;
            cpu_store(cpu, addr, cpu->regs[inst.rs2], 1);
            break;
        }
        case OP_STH: {
            uint32_t addr = cpu->regs[inst.rs1] + inst.imm;
            cpu_store(cpu, addr, cpu->regs[inst.rs2], 2);
            break;
        }
        case OP_STW: {
            uint32_t addr = cpu->regs[inst.rs1] + inst.imm;
            cpu_store(cpu, addr, cpu->regs[inst.rs2], 4);
            break;
        }
            
        // Branch instructions
        case OP_BEQ:
            //printf("[TRACE] BEQ: r%d(0x%08X) == r%d(0x%08X)? imm=%d\n", 
            //       inst.rs1, cpu->regs[inst.rs1], inst.rs2, cpu->regs[inst.rs2], inst.imm);
            //fflush(stdout);
            if (cpu->regs[inst.rs1] == cpu->regs[inst.rs2]) {
                next_pc = next_pc + inst.imm;  // PC+4 base!
                //printf("[TRACE] BEQ taken: next_pc=0x%08X\n", next_pc);
                //fflush(stdout);
            }
            break;
        case OP_BNE:
            if (cpu->regs[inst.rs1] != cpu->regs[inst.rs2]) {
                next_pc = next_pc + inst.imm;  // PC+4 base!
            }
            break;
        case OP_BLT:
            if ((int32_t)cpu->regs[inst.rs1] < (int32_t)cpu->regs[inst.rs2]) {
                next_pc = next_pc + inst.imm;  // PC+4 base!
            }
            break;
        case OP_BGE:
            if ((int32_t)cpu->regs[inst.rs1] >= (int32_t)cpu->regs[inst.rs2]) {
                next_pc = next_pc + inst.imm;  // PC+4 base!
            }
            break;
        case OP_BLTU:
            if (cpu->regs[inst.rs1] < cpu->regs[inst.rs2]) {
                next_pc = next_pc + inst.imm;  // PC+4 base!
            }
            break;
        case OP_BGEU:
            if (cpu->regs[inst.rs1] >= cpu->regs[inst.rs2]) {
                next_pc = next_pc + inst.imm;  // PC+4 base!
            }
            break;
            
        // Jump instructions
        case OP_JAL:
            // JAL instruction
            cpu->regs[inst.rd] = cpu->pc + 4;
            next_pc = cpu->pc + inst.imm;
            break;
        case OP_JALR:
            cpu->regs[inst.rd] = cpu->pc + 4;
            next_pc = (cpu->regs[inst.rs1] + inst.imm) & ~1;
            break;
            
        // U-type
        case OP_LUI:
            cpu->regs[inst.rd] = inst.imm;
            break;
            
        // System instructions
        case OP_NOP:
            break;
        
        case OP_YIELD:
            // YIELD is the MMIO synchronization point
            if (cpu->mmio.initialized) {
                mmio_cpu_iface_t iface = { .halted = &cpu->halted, .exit_status = &cpu->regs[1] };
                // Process ring buffers in both directions
                mmio_ring_process(cpu->mmio.state, &iface);
            }
            // YIELD is also a scheduling hint (could sleep here)
            break;
            
        case OP_DEBUG:
            // DEBUG instruction output
            putchar(cpu->regs[inst.rs1] & 0xFF);
            fflush(stdout);
            break;
            
        case OP_HALT:
            // Process any final MMIO before halting
            if (cpu->mmio.initialized) {
                mmio_cpu_iface_t iface = { .halted = &cpu->halted, .exit_status = &cpu->regs[1] };
                mmio_ring_process(cpu->mmio.state, &iface);
            }
            printf("HALT at PC=0x%08X\n", cpu->pc);
            cpu->halted = true;
            break;
        case OP_ASSERT_EQ:
            if (cpu->regs[inst.rs1] != cpu->regs[inst.rs2]) {
                fprintf(stderr, "Assertion failed: r%d (0x%08X) != r%d (0x%08X)\n",
                       inst.rs1, cpu->regs[inst.rs1], inst.rs2, cpu->regs[inst.rs2]);
                cpu->halted = true;
            }
            break;
            
        // ============================================================
        // f32 (single-precision float) instructions
        // ============================================================
        case OP_FADD_S: {
            float a, b, r;
            memcpy(&a, &cpu->regs[inst.rs1], 4);
            memcpy(&b, &cpu->regs[inst.rs2], 4);
            r = a + b;
            memcpy(&cpu->regs[inst.rd], &r, 4);
            break;
        }
        case OP_FSUB_S: {
            float a, b, r;
            memcpy(&a, &cpu->regs[inst.rs1], 4);
            memcpy(&b, &cpu->regs[inst.rs2], 4);
            r = a - b;
            memcpy(&cpu->regs[inst.rd], &r, 4);
            break;
        }
        case OP_FMUL_S: {
            float a, b, r;
            memcpy(&a, &cpu->regs[inst.rs1], 4);
            memcpy(&b, &cpu->regs[inst.rs2], 4);
            r = a * b;
            memcpy(&cpu->regs[inst.rd], &r, 4);
            break;
        }
        case OP_FDIV_S: {
            float a, b, r;
            memcpy(&a, &cpu->regs[inst.rs1], 4);
            memcpy(&b, &cpu->regs[inst.rs2], 4);
            r = a / b;
            memcpy(&cpu->regs[inst.rd], &r, 4);
            break;
        }
        case OP_FSQRT_S: {
            float a, r;
            memcpy(&a, &cpu->regs[inst.rs1], 4);
            r = sqrtf(a);
            memcpy(&cpu->regs[inst.rd], &r, 4);
            break;
        }
        case OP_FEQ_S: {
            float a, b;
            memcpy(&a, &cpu->regs[inst.rs1], 4);
            memcpy(&b, &cpu->regs[inst.rs2], 4);
            cpu->regs[inst.rd] = (a == b) ? 1 : 0;
            break;
        }
        case OP_FLT_S: {
            float a, b;
            memcpy(&a, &cpu->regs[inst.rs1], 4);
            memcpy(&b, &cpu->regs[inst.rs2], 4);
            cpu->regs[inst.rd] = (a < b) ? 1 : 0;
            break;
        }
        case OP_FLE_S: {
            float a, b;
            memcpy(&a, &cpu->regs[inst.rs1], 4);
            memcpy(&b, &cpu->regs[inst.rs2], 4);
            cpu->regs[inst.rd] = (a <= b) ? 1 : 0;
            break;
        }
        case OP_FCVT_W_S: {
            float a;
            memcpy(&a, &cpu->regs[inst.rs1], 4);
            cpu->regs[inst.rd] = (uint32_t)(int32_t)a;
            break;
        }
        case OP_FCVT_WU_S: {
            float a;
            memcpy(&a, &cpu->regs[inst.rs1], 4);
            cpu->regs[inst.rd] = (uint32_t)a;
            break;
        }
        case OP_FCVT_S_W: {
            float r = (float)(int32_t)cpu->regs[inst.rs1];
            memcpy(&cpu->regs[inst.rd], &r, 4);
            break;
        }
        case OP_FCVT_S_WU: {
            float r = (float)cpu->regs[inst.rs1];
            memcpy(&cpu->regs[inst.rd], &r, 4);
            break;
        }
        case OP_FNEG_S: {
            cpu->regs[inst.rd] = cpu->regs[inst.rs1] ^ 0x80000000u;
            break;
        }
        case OP_FABS_S: {
            cpu->regs[inst.rd] = cpu->regs[inst.rs1] & 0x7FFFFFFFu;
            break;
        }

        // ============================================================
        // f64 (double-precision float) instructions -- register pairs
        // ============================================================
        #if S32_CHECK_F64_REGS
        #define CHECK_F64_REG(reg) do { \
            if ((reg) >= 31 || ((reg) & 1)) { \
                fprintf(stderr, "f64 register fault: r%d is invalid (must be even, < 31) at PC=0x%08X\n", \
                        (reg), cpu->pc); \
                cpu->halted = true; \
                break; \
            } \
        } while(0)
        #else
        #define CHECK_F64_REG(reg) ((void)0)
        #endif

        #define LOAD_F64(reg, var) do { \
            CHECK_F64_REG(reg); \
            uint32_t lo_ = cpu->regs[(reg)]; \
            uint32_t hi_ = cpu->regs[(reg) + 1]; \
            uint64_t bits_ = ((uint64_t)hi_ << 32) | lo_; \
            memcpy(&(var), &bits_, 8); \
        } while(0)

        #define STORE_F64(reg, var) do { \
            CHECK_F64_REG(reg); \
            uint64_t bits_; \
            memcpy(&bits_, &(var), 8); \
            cpu->regs[(reg)] = (uint32_t)bits_; \
            cpu->regs[(reg) + 1] = (uint32_t)(bits_ >> 32); \
        } while(0)

        case OP_FADD_D: {
            double a, b, r;
            LOAD_F64(inst.rs1, a); LOAD_F64(inst.rs2, b);
            r = a + b;
            STORE_F64(inst.rd, r);
            break;
        }
        case OP_FSUB_D: {
            double a, b, r;
            LOAD_F64(inst.rs1, a); LOAD_F64(inst.rs2, b);
            r = a - b;
            STORE_F64(inst.rd, r);
            break;
        }
        case OP_FMUL_D: {
            double a, b, r;
            LOAD_F64(inst.rs1, a); LOAD_F64(inst.rs2, b);
            r = a * b;
            STORE_F64(inst.rd, r);
            break;
        }
        case OP_FDIV_D: {
            double a, b, r;
            LOAD_F64(inst.rs1, a); LOAD_F64(inst.rs2, b);
            r = a / b;
            STORE_F64(inst.rd, r);
            break;
        }
        case OP_FSQRT_D: {
            double a, r;
            LOAD_F64(inst.rs1, a);
            r = sqrt(a);
            STORE_F64(inst.rd, r);
            break;
        }
        case OP_FEQ_D: {
            double a, b;
            LOAD_F64(inst.rs1, a); LOAD_F64(inst.rs2, b);
            cpu->regs[inst.rd] = (a == b) ? 1 : 0;
            break;
        }
        case OP_FLT_D: {
            double a, b;
            LOAD_F64(inst.rs1, a); LOAD_F64(inst.rs2, b);
            cpu->regs[inst.rd] = (a < b) ? 1 : 0;
            break;
        }
        case OP_FLE_D: {
            double a, b;
            LOAD_F64(inst.rs1, a); LOAD_F64(inst.rs2, b);
            cpu->regs[inst.rd] = (a <= b) ? 1 : 0;
            break;
        }
        case OP_FCVT_W_D: {
            double a;
            LOAD_F64(inst.rs1, a);
            cpu->regs[inst.rd] = (uint32_t)(int32_t)a;
            break;
        }
        case OP_FCVT_WU_D: {
            double a;
            LOAD_F64(inst.rs1, a);
            cpu->regs[inst.rd] = (uint32_t)a;
            break;
        }
        case OP_FCVT_D_W: {
            double r = (double)(int32_t)cpu->regs[inst.rs1];
            STORE_F64(inst.rd, r);
            break;
        }
        case OP_FCVT_D_WU: {
            double r = (double)cpu->regs[inst.rs1];
            STORE_F64(inst.rd, r);
            break;
        }
        case OP_FCVT_D_S: {
            float a;
            memcpy(&a, &cpu->regs[inst.rs1], 4);
            double r = (double)a;
            STORE_F64(inst.rd, r);
            break;
        }
        case OP_FCVT_S_D: {
            double a;
            LOAD_F64(inst.rs1, a);
            float r = (float)a;
            memcpy(&cpu->regs[inst.rd], &r, 4);
            break;
        }
        case OP_FNEG_D: {
            cpu->regs[inst.rd] = cpu->regs[inst.rs1];
            cpu->regs[inst.rd + 1] = cpu->regs[inst.rs1 + 1] ^ 0x80000000u;
            break;
        }
        case OP_FABS_D: {
            cpu->regs[inst.rd] = cpu->regs[inst.rs1];
            cpu->regs[inst.rd + 1] = cpu->regs[inst.rs1 + 1] & 0x7FFFFFFFu;
            break;
        }

        // float <-> int64 conversions
        case OP_FCVT_L_S: {
            float a;
            memcpy(&a, &cpu->regs[inst.rs1], 4);
            int64_t r = (int64_t)a;
            cpu->regs[inst.rd] = (uint32_t)r;
            cpu->regs[inst.rd + 1] = (uint32_t)((uint64_t)r >> 32);
            break;
        }
        case OP_FCVT_LU_S: {
            float a;
            memcpy(&a, &cpu->regs[inst.rs1], 4);
            uint64_t r = (uint64_t)a;
            cpu->regs[inst.rd] = (uint32_t)r;
            cpu->regs[inst.rd + 1] = (uint32_t)(r >> 32);
            break;
        }
        case OP_FCVT_S_L: {
            int64_t val = (int64_t)(((uint64_t)cpu->regs[inst.rs1 + 1] << 32) | cpu->regs[inst.rs1]);
            float r = (float)val;
            memcpy(&cpu->regs[inst.rd], &r, 4);
            break;
        }
        case OP_FCVT_S_LU: {
            uint64_t val = ((uint64_t)cpu->regs[inst.rs1 + 1] << 32) | cpu->regs[inst.rs1];
            float r = (float)val;
            memcpy(&cpu->regs[inst.rd], &r, 4);
            break;
        }
        case OP_FCVT_L_D: {
            double a;
            LOAD_F64(inst.rs1, a);
            int64_t r = (int64_t)a;
            cpu->regs[inst.rd] = (uint32_t)r;
            cpu->regs[inst.rd + 1] = (uint32_t)((uint64_t)r >> 32);
            break;
        }
        case OP_FCVT_LU_D: {
            double a;
            LOAD_F64(inst.rs1, a);
            uint64_t r = (uint64_t)a;
            cpu->regs[inst.rd] = (uint32_t)r;
            cpu->regs[inst.rd + 1] = (uint32_t)(r >> 32);
            break;
        }
        case OP_FCVT_D_L: {
            int64_t val = (int64_t)(((uint64_t)cpu->regs[inst.rs1 + 1] << 32) | cpu->regs[inst.rs1]);
            double r = (double)val;
            STORE_F64(inst.rd, r);
            break;
        }
        case OP_FCVT_D_LU: {
            uint64_t val = ((uint64_t)cpu->regs[inst.rs1 + 1] << 32) | cpu->regs[inst.rs1];
            double r = (double)val;
            STORE_F64(inst.rd, r);
            break;
        }

        #undef LOAD_F64
        #undef STORE_F64

        default:
            fprintf(stderr, "Unknown opcode: 0x%02X at PC=0x%08X\n", inst.opcode, cpu->pc);
            cpu->halted = true;
            break;
    }
    
    cpu->regs[REG_ZERO] = 0;  // Ensure r0 stays 0
    cpu->pc = next_pc;
    cpu->cycle_count++;
    cpu->inst_count++;
    
    // Show register changes
    if (cpu->debug.show_reg_changes) {
        for (int i = 1; i < NUM_REGS; i++) {
            if (cpu->regs[i] != cpu->debug.prev_regs[i]) {
                printf("  r%d: 0x%08X -> 0x%08X", i, cpu->debug.prev_regs[i], cpu->regs[i]);
            }
        }
    }
    
    if (cpu->debug.trace_instructions || cpu->debug.show_reg_changes) {
        printf("\n");
    }
    
    // Check cycle limit
    if (cpu->debug.max_cycles > 0 && cpu->cycle_count >= cpu->debug.max_cycles) {
        printf("Cycle limit reached (%lu cycles)\n", cpu->debug.max_cycles);
        cpu->halted = true;
    }
}

void cpu_run(cpu_state_t *cpu) {
    while (!cpu->halted) {
        if (cpu->debug.step_mode) {
            cpu_step(cpu);
            printf("Press Enter to continue, 'r' to run, 'q' to quit: ");
            char c = getchar();
            if (c == 'q') {
                cpu->halted = true;
            } else if (c == 'r') {
                cpu->debug.step_mode = false;
            }
        } else {
            cpu_step(cpu);
        }
    }
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s [options] <program.s32x> [-- <args...>]\n", argv[0]);
        fprintf(stderr, "Options:\n");
        fprintf(stderr, "  -s              Step mode\n");
        fprintf(stderr, "  -t              Trace instructions\n");
        fprintf(stderr, "  -r              Show register changes\n");
        fprintf(stderr, "  -c <cycles>     Limit execution cycles\n");
        fprintf(stderr, "  -b <addr>       Set breakpoint\n");
        fprintf(stderr, "  -w <start-end>  Watch memory range\n");
        return 1;
    }
    
    cpu_state_t cpu;
    cpu_init(&cpu);
    
    // Parse command line options
    int opt;
    while ((opt = getopt(argc, argv, "strc:b:w:")) != -1) {
        switch (opt) {
            case 's':
                cpu.debug.step_mode = true;
                cpu.debug.enabled = true;
                break;
            case 't':
                cpu.debug.trace_instructions = true;
                cpu.debug.enabled = true;
                break;
            case 'r':
                cpu.debug.show_reg_changes = true;
                cpu.debug.enabled = true;
                break;
            case 'c':
                cpu.debug.max_cycles = strtoull(optarg, NULL, 0);
                cpu.debug.enabled = true;
                break;
            case 'b':
                if (cpu.debug.num_breakpoints < 16) {
                    cpu.debug.breakpoints[cpu.debug.num_breakpoints++] = strtoul(optarg, NULL, 0);
                    cpu.debug.enabled = true;
                }
                break;
            case 'w': {
                char *dash = strchr(optarg, '-');
                if (dash) {
                    *dash = '\0';
                    cpu.debug.watch_start = strtoul(optarg, NULL, 0);
                    cpu.debug.watch_end = strtoul(dash + 1, NULL, 0);
                    cpu.debug.watch_enabled = true;
                    cpu.debug.enabled = true;
                }
                break;
            }
        }
    }
    
    // Load program
    if (optind >= argc) {
        fprintf(stderr, "Error: missing program path\n");
        cpu_destroy(&cpu);
        return 1;
    }

    if (!cpu_load_binary(&cpu, argv[optind])) {
        cpu_destroy(&cpu);
        return 1;
    }

    int guest_argc = argc - optind;
    char **guest_argv = &argv[optind];
    if (cpu.mmio.enabled && cpu.mmio.state) {
        if (mmio_ring_set_args(cpu.mmio.state, (uint32_t)guest_argc, guest_argv) != 0) {
            fprintf(stderr, "Error: unable to stage guest arguments (too many bytes?)\n");
            cpu_destroy(&cpu);
            return 1;
        }
        // Pass through host environment to guest
        extern char **environ;
        if (mmio_ring_set_envp(cpu.mmio.state, environ) != 0) {
            fprintf(stderr, "Warning: unable to stage host environment (too many bytes?)\n");
            // Non-fatal - continue without environment
        }
    } else if (guest_argc > 1) {
        fprintf(stderr, "Warning: guest arguments ignored because MMIO is disabled.\n");
    }
    
    // Run program
    printf("Starting execution\n");
    fflush(stdout);
    clock_t start = clock();
    cpu_run(&cpu);
    clock_t end = clock();
    
    // Calculate performance
    double elapsed = (double)(end - start) / CLOCKS_PER_SEC;
    if (elapsed > 0) {
        double mips = (cpu.inst_count / elapsed) / 1e6;
        printf("\nProgram halted.\n");
        printf("Instructions executed: %" PRIu64 "\n", cpu.inst_count);
        printf("Cycles: %" PRIu64 "\n", cpu.cycle_count);
        printf("Wall time: %.6f seconds\n", elapsed);
        printf("Performance: %.2f MIPS\n", mips);
    }
    
    int exit_code = cpu.regs[1];
    cpu_destroy(&cpu);
    return exit_code;
}
