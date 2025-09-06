// SLOW32 Disassembler
// Simple disassembler for SLOW32 executables and object files

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>

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
    {"mul",   0x02, FMT_R},
    {"div",   0x03, FMT_R},
    {"rem",   0x04, FMT_R},
    {"and",   0x05, FMT_R},
    {"or",    0x06, FMT_R},
    {"xor",   0x07, FMT_R},
    {"sll",   0x08, FMT_R},
    {"srl",   0x09, FMT_R},
    {"sra",   0x0A, FMT_R},
    {"slt",   0x0B, FMT_R},
    {"sltu",  0x0C, FMT_R},
    
    {"addi",  0x10, FMT_I},
    {"andi",  0x11, FMT_I},
    {"ori",   0x12, FMT_I},
    {"xori",  0x13, FMT_I},
    {"slli",  0x14, FMT_I},
    {"srli",  0x15, FMT_I},
    {"srai",  0x16, FMT_I},
    {"slti",  0x17, FMT_I},
    {"sltiu", 0x18, FMT_I},
    {"sgt",   0x19, FMT_R},
    {"sgtu",  0x1A, FMT_R},
    {"sle",   0x1B, FMT_R},
    {"sleu",  0x1C, FMT_R},
    {"sge",   0x1D, FMT_R},
    {"sgeu",  0x1E, FMT_R},
    
    {"lui",   0x20, FMT_U},
    
    {"ldb",   0x30, FMT_I},
    {"ldh",   0x31, FMT_I},
    {"ldw",   0x32, FMT_I},
    {"ldbu",  0x33, FMT_I},
    {"ldhu",  0x34, FMT_I},
    
    {"stb",   0x38, FMT_S},
    {"sth",   0x39, FMT_S},
    {"stw",   0x3A, FMT_S},
    
    {"beq",   0x40, FMT_B},
    {"bne",   0x41, FMT_B},
    {"blt",   0x42, FMT_B},
    {"bge",   0x43, FMT_B},
    {"bltu",  0x44, FMT_B},
    {"bgeu",  0x45, FMT_B},
    
    {"jal",   0x50, FMT_J},
    {"jalr",  0x51, FMT_I},
    
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
    int32_t imm = ((inst >> 7) & 0x1F) | ((inst >> 20) & 0xFE0);
    // Sign extend from 12 bits
    if (imm & 0x800) imm |= 0xFFFFF000;
    return imm;
}

static inline int extract_imm_b(uint32_t inst) {
    int32_t imm = ((inst >> 8) & 0x1E) | ((inst >> 20) & 0x7E0) |
                  ((inst << 4) & 0x800) | ((inst >> 19) & 0x1000);
    // Sign extend from 13 bits
    if (imm & 0x1000) imm |= 0xFFFFE000;
    return imm;
}

static inline int extract_imm_u(uint32_t inst) {
    return (inst >> 12) & 0xFFFFF;  // RISC-V style: 20-bit immediate in bits [31:12]
}

static inline int extract_imm_j(uint32_t inst) {
    int32_t imm = ((inst >> 12) & 0xFF) | ((inst >> 3) & 0x300) |
                  ((inst << 2) & 0x400) | ((inst >> 9) & 0x7FE) |
                  ((inst >> 11) & 0x100000);
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
    
    switch (info->format) {
        case FMT_R: {
            int rd = extract_rd(inst);
            int rs1 = extract_rs1(inst);
            int rs2 = extract_rs2(inst);
            printf(" r%d, r%d, r%d", rd, rs1, rs2);
            break;
        }
        
        case FMT_I: {
            int rd = extract_rd(inst);
            int rs1 = extract_rs1(inst);
            int imm = extract_imm_i(inst);
            
            if (opcode == 0x51) {  // jalr
                printf(" r%d, r%d, %d", rd, rs1, imm);
            } else if (opcode >= 0x30 && opcode <= 0x34) {  // loads
                printf(" r%d, r%d+%d", rd, rs1, imm);
            } else {
                printf(" r%d, r%d, %d", rd, rs1, imm);
            }
            break;
        }
        
        case FMT_S: {
            int rs1 = extract_rs1(inst);
            int rs2 = extract_rs2(inst);
            int imm = extract_imm_s(inst);
            printf(" r%d+%d, r%d", rs1, imm, rs2);
            break;
        }
        
        case FMT_B: {
            int rs1 = extract_rs1(inst);
            int rs2 = extract_rs2(inst);
            int imm = extract_imm_b(inst);
            printf(" r%d, r%d, 0x%x", rs1, rs2, addr + imm);
            break;
        }
        
        case FMT_U: {
            int rd = extract_rd(inst);
            int imm = extract_imm_u(inst);
            printf(" r%d, %d", rd, imm);
            break;
        }
        
        case FMT_J: {
            int rd = extract_rd(inst);
            int imm = extract_imm_j(inst);
            if (rd == 0) {
                printf(" 0x%x", addr + imm);  // Unconditional jump
            } else {
                printf(" r%d, 0x%x", rd, addr + imm);
            }
            break;
        }
        
        default:
            break;
    }
    
    printf("\n");
}

// S32X header structure
typedef struct {
    uint32_t magic;        // 'X23S'
    uint32_t version;
    uint32_t entry_point;
    uint32_t stack_base;
    uint32_t sect_count;
    uint32_t sect_offset;
    uint32_t sym_count;
    uint32_t sym_offset;
} s32x_header_t;

// Section header
typedef struct {
    uint32_t type;
    uint32_t flags;
    uint32_t vaddr;
    uint32_t size;
    uint32_t offset;
    uint32_t align;
    char name[16];
} s32x_section_t;

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
    
    // Check magic (little-endian: 'X23S' = 0x53333258)
    if (header.magic != 0x53333258) {  // 'X23S' in little-endian
        fprintf(stderr, "Not a valid S32X file (magic: 0x%08x)\n", header.magic);
        fclose(f);
        return 1;
    }
    
    printf("S32X Executable Disassembly\n");
    printf("Entry point: 0x%08x\n", header.entry_point);
    printf("Stack base:  0x%08x\n", header.stack_base);
    printf("\n");
    
    // Read sections
    fseek(f, header.sect_offset, SEEK_SET);
    for (uint32_t i = 0; i < header.sect_count; i++) {
        s32x_section_t sect;
        if (fread(&sect, sizeof(sect), 1, f) != 1) break;
        
        if (sect.type == 1) {  // Code section
            printf("Section: %s at 0x%08x (%d bytes)\n", sect.name, sect.vaddr, sect.size);
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
    
    fclose(f);
    return 0;
}