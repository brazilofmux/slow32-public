// SLOW32 Disassembler
// Simple disassembler for SLOW32 executables and object files

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include "../common/s32_formats.h"

// SLOW32 instruction formats
typedef enum {
    FMT_R, FMT_I, FMT_S, FMT_B, FMT_U, FMT_J, FMT_UNKNOWN
} inst_format_t;

// Instruction info
typedef struct {
    const char *mnemonic;
    uint32_t opcode;
    inst_format_t format;
} instruction_info_t;

// SLOW32 instruction table
static instruction_info_t instructions[] = {
    {"add",   0x00, FMT_R},
    {"sub",   0x01, FMT_R},
    {"xor",   0x02, FMT_R},
    {"or",    0x03, FMT_R},
    {"and",   0x04, FMT_R},
    {"sll",   0x05, FMT_R},
    {"srl",   0x06, FMT_R},
    {"sra",   0x07, FMT_R},
    {"slt",   0x08, FMT_R},
    {"sltu",  0x09, FMT_R},
    {"mul",   0x0A, FMT_R},
    {"mulh",  0x0B, FMT_R},
    {"div",   0x0C, FMT_R},
    {"rem",   0x0D, FMT_R},
    {"seq",   0x0E, FMT_R},
    {"sne",   0x0F, FMT_R},
    
    {"addi",  0x10, FMT_I},
    {"ori",   0x11, FMT_I},
    {"andi",  0x12, FMT_I},
    {"slli",  0x13, FMT_I},
    {"srli",  0x14, FMT_I},
    {"srai",  0x15, FMT_I},
    {"slti",  0x16, FMT_I},
    {"sltiu", 0x17, FMT_I},
    {"sgt",   0x18, FMT_R},
    {"sgtu",  0x19, FMT_R},
    {"sle",   0x1A, FMT_R},
    {"sleu",  0x1B, FMT_R},
    {"sge",   0x1C, FMT_R},
    {"sgeu",  0x1D, FMT_R},
    {"xori",  0x1E, FMT_I},
    
    {"lui",   0x20, FMT_U},
    
    {"ldb",   0x30, FMT_I},
    {"ldh",   0x31, FMT_I},
    {"ldw",   0x32, FMT_I},
    {"ldbu",  0x33, FMT_I},
    {"ldhu",  0x34, FMT_I},
    
    {"stb",   0x38, FMT_S},
    {"sth",   0x39, FMT_S},
    {"stw",   0x3A, FMT_S},
    
    {"assert_eq", 0x3F, FMT_R},
    
    {"jal",   0x40, FMT_J},
    {"jalr",  0x41, FMT_I},
    
    {"beq",   0x48, FMT_B},
    {"bne",   0x49, FMT_B},
    {"blt",   0x4A, FMT_B},
    {"bge",   0x4B, FMT_B},
    {"bltu",  0x4C, FMT_B},
    {"bgeu",  0x4D, FMT_B},
    
    {"nop",   0x50, FMT_R},
    {"yield", 0x51, FMT_R},
    {"debug", 0x52, FMT_I},
    {"halt",  0x7F, FMT_R},
    
    {NULL, 0, FMT_UNKNOWN}
};

// Extract instruction fields
static inline uint32_t extract_opcode(uint32_t inst) {
    return inst & 0x7F;
}

static inline int extract_rd(uint32_t inst) {
    return (inst >> 7) & 0x1F;
}

static inline int extract_rs1(uint32_t inst) {
    return (inst >> 15) & 0x1F;
}

static inline int extract_rs2(uint32_t inst) {
    return (inst >> 20) & 0x1F;
}

static inline int extract_imm_i(uint32_t inst) {
    int32_t imm = (int32_t)inst >> 20;
    return imm;
}

static inline int extract_imm_s(uint32_t inst) {
    // SLOW-32 store format (matching emulator)
    int32_t imm = ((inst >> 7) & 0x1F) | (((int32_t)inst >> 25) << 5);
    return imm;
}

static inline int extract_imm_b(uint32_t inst) {
    // SLOW-32 branch format (matching emulator)
    int32_t imm = (((inst >> 8) & 0xF) << 1) | (((inst >> 25) & 0x3F) << 5) |
                  (((inst >> 7) & 0x1) << 11) | (((int32_t)inst >> 31) << 12);
    return imm;
}

static inline int extract_imm_u(uint32_t inst) {
    // SLOW-32 LUI format - immediate already in bits [31:12]
    return inst & 0xFFFFF000;
}

static inline int extract_imm_j(uint32_t inst) {
    // SLOW-32 JAL format (matching emulator)
    uint32_t imm20 = (inst >> 31) & 0x1;
    uint32_t imm10_1 = (inst >> 21) & 0x3FF;
    uint32_t imm11 = (inst >> 20) & 0x1;
    uint32_t imm19_12 = (inst >> 12) & 0xFF;
    int32_t imm = (imm20 << 20) | (imm19_12 << 12) | (imm11 << 11) | (imm10_1 << 1);
    // Sign extend from 21 bits
    if (imm & 0x100000) imm |= 0xFFE00000;
    return imm;
}

// Find instruction by opcode
static instruction_info_t* find_instruction(uint32_t opcode) {
    for (int i = 0; instructions[i].mnemonic; i++) {
        if (instructions[i].opcode == opcode) {
            return &instructions[i];
        }
    }
    return NULL;
}

// Special case disassembly helpers
static const char* get_reg_name(int reg) {
    static char bufs[4][8];  // Rotate through 4 buffers
    static int buf_idx = 0;
    
    if (reg == 0) return "zero";
    if (reg == 29) return "sp";
    if (reg == 30) return "fp";
    if (reg == 31) return "lr";
    
    char *buf = bufs[buf_idx];
    buf_idx = (buf_idx + 1) % 4;
    sprintf(buf, "r%d", reg);
    return buf;
}

// Disassemble a single instruction
static void disassemble_instruction(uint32_t addr, uint32_t inst) {
    uint32_t opcode = extract_opcode(inst);
    instruction_info_t *info = find_instruction(opcode);
    
    printf("0x%08x: %08x  ", addr, inst);
    
    if (!info) {
        printf("unknown opcode 0x%02x\n", opcode);
        return;
    }
    
    printf("%s", info->mnemonic);
    
    // Special cases
    if (opcode == 0x7F) {  // HALT
        printf("\n");
        return;
    }
    
    if (opcode == 0x52) {  // DEBUG
        int rs1 = extract_rs1(inst);
        printf(" %s\n", get_reg_name(rs1));
        return;
    }
    
    switch (info->format) {
        case FMT_R: {
            int rd = extract_rd(inst);
            int rs1 = extract_rs1(inst);
            int rs2 = extract_rs2(inst);
            printf(" %s, %s, %s", get_reg_name(rd), get_reg_name(rs1), get_reg_name(rs2));
            break;
        }
        
        case FMT_I: {
            int rd = extract_rd(inst);
            int rs1 = extract_rs1(inst);
            int imm = extract_imm_i(inst);
            
            if (opcode == 0x41) {  // jalr
                printf(" %s, %s, %d", get_reg_name(rd), get_reg_name(rs1), imm);
            } else if (opcode >= 0x30 && opcode <= 0x34) {  // loads
                printf(" %s, %s+%d", get_reg_name(rd), get_reg_name(rs1), imm);
            } else {
                printf(" %s, %s, %d", get_reg_name(rd), get_reg_name(rs1), imm);
            }
            break;
        }
        
        case FMT_S: {
            int rs1 = extract_rs1(inst);
            int rs2 = extract_rs2(inst);
            int imm = extract_imm_s(inst);
            printf(" %s+%d, %s", get_reg_name(rs1), imm, get_reg_name(rs2));
            break;
        }
        
        case FMT_B: {
            int rs1 = extract_rs1(inst);
            int rs2 = extract_rs2(inst);
            int imm = extract_imm_b(inst);
            // Branches are PC+4 relative in SLOW-32
            printf(" %s, %s, 0x%x", get_reg_name(rs1), get_reg_name(rs2), addr + 4 + imm);
            break;
        }
        
        case FMT_U: {
            int rd = extract_rd(inst);
            int imm = extract_imm_u(inst);
            printf(" %s, 0x%x", get_reg_name(rd), imm >> 12);  // Show upper 20 bits
            break;
        }
        
        case FMT_J: {
            int rd = extract_rd(inst);
            int imm = extract_imm_j(inst);
            if (rd == 0) {
                printf(" 0x%x", addr + imm);  // Unconditional jump
            } else {
                printf(" %s, 0x%x", get_reg_name(rd), addr + imm);
            }
            break;
        }
        
        default:
            break;
    }
    
    printf("\n");
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <file> [start_addr] [end_addr]\n", argv[0]);
        return 1;
    }
    
    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Cannot open file");
        return 1;
    }
    
    // Read header
    s32x_header_t header;
    if (fread(&header, sizeof(header), 1, f) != 1) {
        fprintf(stderr, "Cannot read header\n");
        fclose(f);
        return 1;
    }
    
    // Check magic
    if (header.magic != S32X_MAGIC) {
        fprintf(stderr, "Not a valid S32X file (magic: 0x%08x)\n", header.magic);
        fclose(f);
        return 1;
    }
    
    printf("S32X Executable Disassembly\n");
    printf("Entry point: 0x%08x\n", header.entry);
    printf("Stack base:  0x%08x\n", header.stack_base);
    printf("\n");
    
    // Read string table
    char *strtab = malloc(header.str_size);
    if (!strtab) {
        fprintf(stderr, "Cannot allocate string table\n");
        fclose(f);
        return 1;
    }
    fseek(f, header.str_offset, SEEK_SET);
    fread(strtab, 1, header.str_size, f);
    
    // Read sections
    fseek(f, header.sec_offset, SEEK_SET);
    for (uint32_t i = 0; i < header.nsections; i++) {
        s32x_section_t sect;
        if (fread(&sect, sizeof(sect), 1, f) != 1) break;
        
        if (sect.type == S32_SEC_CODE) {  // Code section
            const char *name = &strtab[sect.name_offset];
            printf("Section: %s at 0x%08x (%d bytes)\n", name, sect.vaddr, sect.mem_size);
            printf("----------------------------------------\n");
            
            // Save position
            long pos = ftell(f);
            
            // Read code
            fseek(f, sect.offset, SEEK_SET);
            uint32_t *code = malloc(sect.size);
            if (code) {
                fread(code, 1, sect.size, f);
                
                // Disassemble
                uint32_t addr = sect.vaddr;
                uint32_t start = 0;
                uint32_t end = sect.size / 4;
                
                // Optional address range from command line
                if (argc > 2) {
                    uint32_t start_addr = strtoul(argv[2], NULL, 0);
                    if (start_addr >= addr) {
                        start = (start_addr - addr) / 4;
                    }
                }
                if (argc > 3) {
                    uint32_t end_addr = strtoul(argv[3], NULL, 0);
                    if (end_addr >= addr) {
                        uint32_t new_end = (end_addr - addr) / 4 + 1;
                        if (new_end < end) end = new_end;
                    }
                }
                
                for (uint32_t j = start; j < end && j < sect.size/4; j++) {
                    disassemble_instruction(addr + j*4, code[j]);
                }
                
                free(code);
            }
            
            // Restore position
            fseek(f, pos, SEEK_SET);
            printf("\n");
        }
    }
    
    free(strtab);
    fclose(f);
    return 0;
}