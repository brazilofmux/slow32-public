#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <getopt.h>
#include <time.h>
#include <inttypes.h>
#include <sys/mman.h>
#include "slow32.h"
#include "s32x_loader.h"
#include "memory_manager.h"
#include "mmio_ring.h"

// Enable to trap on unaligned LD/ST (recommended for a strict ISA)
#ifndef S32_ALLOW_UNALIGNED
#define S32_TRAP_ON_UNALIGNED 1
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
    //printf("[TRACE] decode_instruction: raw=0x%08X\n", raw);
    fflush(stdout);
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
    //printf("[TRACE] cpu_init: starting\n");
    fflush(stdout);
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
    
    //printf("[TRACE] cpu_init: completed\n");
    fflush(stdout);
}

void cpu_destroy(cpu_state_t *cpu) {
    //printf("[TRACE] cpu_destroy: starting\n");
    fflush(stdout);
    
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
    //printf("[TRACE] cpu_destroy: completed\n");
    fflush(stdout);
}

// Modified loader to use memory manager
bool cpu_load_binary(cpu_state_t *cpu, const char *filename) {
    FILE *f = fopen(filename, "rb");
    if (!f) {
        fprintf(stderr, "Cannot open file: %s\n", filename);
        return false;
    }
    
    s32x_header_t header;
    if (fread(&header, sizeof(header), 1, f) != 1) {
        fprintf(stderr, "Cannot read header\n");
        fclose(f);
        return false;
    }
    
    if (header.magic != S32X_MAGIC) {
        fprintf(stderr, "Invalid magic: 0x%08X\n", header.magic);
        fclose(f);
        return false;
    }
    
    // W^X protection is enforced by hardware (mprotect) making code/rodata non-writable
    // The flag is preserved for .s32x compatibility but protection is always active
    cpu->mm.wxorx_enabled = (header.flags & S32X_FLAG_W_XOR_X) != 0;
    
    // Memory layout loaded from header
    
    // Allocate memory regions based on executable requirements
    //printf("[TRACE] About to call mem_setup_from_s32x\n");
    fflush(stdout);
    if (mm_setup_from_s32x(&cpu->mm, header.code_limit, header.rodata_limit,
                          header.data_limit, header.stack_base) < 0) {
        fprintf(stderr, "Failed to allocate memory regions\n");
        fclose(f);
        return false;
    }
    //printf("[TRACE] mem_setup_from_s32x completed successfully\n");
    fflush(stdout);
    
    // Read string table
    //printf("[TRACE] Allocating string table: size=%u\n", header.str_size);
    fflush(stdout);
    char *strtab = malloc(header.str_size);
    if (!strtab) {
        fprintf(stderr, "Out of memory for string table\n");
        fclose(f);
        return false;
    }
    
    //printf("[TRACE] Seeking to string table at offset %u\n", header.str_offset);
    fflush(stdout);
    fseek(f, header.str_offset, SEEK_SET);
    //printf("[TRACE] Reading string table\n");
    fflush(stdout);
    fread(strtab, 1, header.str_size, f);
    //printf("[TRACE] String table read complete\n");
    fflush(stdout);
    
    // Load sections into allocated regions
    //printf("[TRACE] Loading %u sections starting at offset %u\n", header.nsections, header.sec_offset);
    fflush(stdout);
    fseek(f, header.sec_offset, SEEK_SET);
    for (uint32_t i = 0; i < header.nsections; i++) {
        //printf("[TRACE] Loading section %u/%u\n", i+1, header.nsections);
        fflush(stdout);
        s32x_section_t section;
        if (fread(&section, sizeof(section), 1, f) != 1) {
            fprintf(stderr, "Cannot read section %u\n", i);
            free(strtab);
            fclose(f);
            return false;
        }
        
        const char *name = (section.name_offset < header.str_size) ? 
                           &strtab[section.name_offset] : "?";
        
        // Loading section
        
        // Load section data
        if (section.size > 0 && section.offset > 0) {
            long current_pos = ftell(f);
            fseek(f, section.offset, SEEK_SET);
            
            // Read into temporary buffer first
            uint8_t *temp = malloc(section.size);
            if (!temp || fread(temp, 1, section.size, f) != section.size) {
                fprintf(stderr, "Cannot read section '%s' data\n", name);
                free(temp);
                free(strtab);
                fclose(f);
                return false;
            }
            
            // Write to memory through memory manager
            if (mm_write(&cpu->mm, section.vaddr, temp, section.size) < 0) {
                fprintf(stderr, "Cannot write section '%s' to memory\n", name);
                free(temp);
                free(strtab);
                fclose(f);
                return false;
            }
            
            free(temp);
            fseek(f, current_pos, SEEK_SET);
        }
        
        // Zero-fill BSS sections and padding
        if (section.mem_size > section.size) {
            uint32_t zero_size = section.mem_size - section.size;
            uint8_t *zeros = calloc(1, zero_size);
            if (zeros) {
                mm_write(&cpu->mm, section.vaddr + section.size, zeros, zero_size);
                free(zeros);
            }
        }
    }
    
    //printf("[TRACE] All sections loaded, freeing string table\n");
    fflush(stdout);
    free(strtab);
    //printf("[TRACE] Closing file\n");
    fflush(stdout);
    fclose(f);
    
    // Finalize memory protection after loading
    //printf("[TRACE] About to call mem_protect_regions\n");
    fflush(stdout);
    // Apply memory protection (code and rodata become read-only)
    if (mm_protect_regions(&cpu->mm, header.code_limit, header.rodata_limit) < 0) {
        fprintf(stderr, "Failed to set memory protection\n");
        return false;
    }
    
    // Cache the code region for fast instruction fetch
    cpu->code_region = NULL;
    for (memory_region_t *r = cpu->mm.regions; r; r = r->next) {
        if (r->vaddr_start == 0) {
            cpu->code_region = r;
            break;
        }
    }
    
    // Set up CPU state
    //printf("[TRACE] mem_protect_regions completed, setting up CPU state\n");
    fflush(stdout);
    cpu->pc = header.entry;
    cpu->regs[REG_SP] = header.stack_base;
    cpu->code_limit = header.code_limit;
    cpu->rodata_limit = header.rodata_limit;
    cpu->data_limit = header.data_limit;
    cpu->wxorx_enabled = cpu->mm.wxorx_enabled;  // Kept for compatibility
    
    // Check header flag for MMIO configuration
    if (header.flags & S32X_FLAG_MMIO) {
        cpu->mmio.enabled = true;
        cpu->mmio.base = header.mmio_base;
        // Initialize MMIO immediately since we know it's needed
        cpu_init_mmio(cpu);
    }
    
    // Executable loaded successfully
    
    //printf("[TRACE] About to call mem_print_map\n");
    fflush(stdout);
    // Memory map configured
    //printf("[TRACE] cpu_load_binary completed successfully\n");
    fflush(stdout);
    
    return true;
}

// Memory access functions using memory manager
static uint32_t cpu_load(cpu_state_t *cpu, uint32_t addr, int size) {
    fflush(stdout);
    uint32_t value = 0;
    
    // Check if this is MMIO access (only if MMIO is configured)
    if (cpu->mmio.base != 0 && addr >= cpu->mmio.base && addr < cpu->mmio.base + 0x10000) {

        if (cpu->mmio.initialized) {
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
        }
        return value;
    }
    
    if (mm_read(&cpu->mm, addr, &value, size) < 0) {
        fprintf(stderr, "Memory fault: Failed to read %d bytes at 0x%08X\n", size, addr);
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

        if (cpu->mmio.initialized) {
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
        fprintf(stderr, "Memory fault: Failed to write %d bytes at 0x%08X\n", size, addr);
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
    if (cpu->code_region && addr < cpu->code_region->vaddr_end) {
        uint32_t inst;
        memcpy(&inst, (uint8_t*)cpu->code_region->host_addr + addr, 4);
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
        case OP_DIV:
            if (cpu->regs[inst.rs2] == 0) {
                cpu->regs[inst.rd] = 0xFFFFFFFF;
            } else {
                cpu->regs[inst.rd] = (int32_t)cpu->regs[inst.rs1] / (int32_t)cpu->regs[inst.rs2];
            }
            break;
        case OP_REM:
            if (cpu->regs[inst.rs2] == 0) {
                cpu->regs[inst.rd] = cpu->regs[inst.rs1];
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
                mmio_cpu_iface_t iface = { .halted = &cpu->halted };
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
                mmio_cpu_iface_t iface = { .halted = &cpu->halted };
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
    //printf("[TRACE] cpu_run: ENTER, halted=%d\n", cpu->halted);
    fflush(stdout);
    while (!cpu->halted) {
        //printf("[TRACE] cpu_run: loop iteration\n");
        fflush(stdout);
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
    } else if (guest_argc > 1) {
        fprintf(stderr, "Warning: guest arguments ignored because MMIO is disabled.\n");
    }
    
    // Run program
    printf("Starting execution\n");
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
    
    cpu_destroy(&cpu);
    return 0;
}
