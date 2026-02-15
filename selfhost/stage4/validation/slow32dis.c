// SLOW32 Disassembler
// Disassembles SLOW32 executables (.s32x) and object files (.s32o)

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <s32_formats.h>

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
static instruction_info_t instructions[96];
static bool instructions_initialized = false;
static char reg_name_buf0[16];
static char reg_name_buf1[16];
static char reg_name_buf2[16];
static char reg_name_buf3[16];
static int reg_name_buf_idx = 0;

static void init_instructions(void) {
    if (instructions_initialized) return;
    int i = 0;
    instructions[i].mnemonic = "add";
    instructions[i].opcode = 0x00;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "sub";
    instructions[i].opcode = 0x01;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "xor";
    instructions[i].opcode = 0x02;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "or";
    instructions[i].opcode = 0x03;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "and";
    instructions[i].opcode = 0x04;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "sll";
    instructions[i].opcode = 0x05;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "srl";
    instructions[i].opcode = 0x06;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "sra";
    instructions[i].opcode = 0x07;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "slt";
    instructions[i].opcode = 0x08;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "sltu";
    instructions[i].opcode = 0x09;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "mul";
    instructions[i].opcode = 0x0A;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "mulh";
    instructions[i].opcode = 0x0B;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "div";
    instructions[i].opcode = 0x0C;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "rem";
    instructions[i].opcode = 0x0D;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "seq";
    instructions[i].opcode = 0x0E;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "sne";
    instructions[i].opcode = 0x0F;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "addi";
    instructions[i].opcode = 0x10;
    instructions[i].format = FMT_I;
    i++;
    instructions[i].mnemonic = "ori";
    instructions[i].opcode = 0x11;
    instructions[i].format = FMT_I;
    i++;
    instructions[i].mnemonic = "andi";
    instructions[i].opcode = 0x12;
    instructions[i].format = FMT_I;
    i++;
    instructions[i].mnemonic = "slli";
    instructions[i].opcode = 0x13;
    instructions[i].format = FMT_I;
    i++;
    instructions[i].mnemonic = "srli";
    instructions[i].opcode = 0x14;
    instructions[i].format = FMT_I;
    i++;
    instructions[i].mnemonic = "srai";
    instructions[i].opcode = 0x15;
    instructions[i].format = FMT_I;
    i++;
    instructions[i].mnemonic = "slti";
    instructions[i].opcode = 0x16;
    instructions[i].format = FMT_I;
    i++;
    instructions[i].mnemonic = "sltiu";
    instructions[i].opcode = 0x17;
    instructions[i].format = FMT_I;
    i++;
    instructions[i].mnemonic = "sgt";
    instructions[i].opcode = 0x18;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "sgtu";
    instructions[i].opcode = 0x19;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "sle";
    instructions[i].opcode = 0x1A;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "sleu";
    instructions[i].opcode = 0x1B;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "sge";
    instructions[i].opcode = 0x1C;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "sgeu";
    instructions[i].opcode = 0x1D;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "xori";
    instructions[i].opcode = 0x1E;
    instructions[i].format = FMT_I;
    i++;
    instructions[i].mnemonic = "mulhu";
    instructions[i].opcode = 0x1F;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "lui";
    instructions[i].opcode = 0x20;
    instructions[i].format = FMT_U;
    i++;
    instructions[i].mnemonic = "ldb";
    instructions[i].opcode = 0x30;
    instructions[i].format = FMT_I;
    i++;
    instructions[i].mnemonic = "ldh";
    instructions[i].opcode = 0x31;
    instructions[i].format = FMT_I;
    i++;
    instructions[i].mnemonic = "ldw";
    instructions[i].opcode = 0x32;
    instructions[i].format = FMT_I;
    i++;
    instructions[i].mnemonic = "ldbu";
    instructions[i].opcode = 0x33;
    instructions[i].format = FMT_I;
    i++;
    instructions[i].mnemonic = "ldhu";
    instructions[i].opcode = 0x34;
    instructions[i].format = FMT_I;
    i++;
    instructions[i].mnemonic = "stb";
    instructions[i].opcode = 0x38;
    instructions[i].format = FMT_S;
    i++;
    instructions[i].mnemonic = "sth";
    instructions[i].opcode = 0x39;
    instructions[i].format = FMT_S;
    i++;
    instructions[i].mnemonic = "stw";
    instructions[i].opcode = 0x3A;
    instructions[i].format = FMT_S;
    i++;
    instructions[i].mnemonic = "assert_eq";
    instructions[i].opcode = 0x3F;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "jal";
    instructions[i].opcode = 0x40;
    instructions[i].format = FMT_J;
    i++;
    instructions[i].mnemonic = "jalr";
    instructions[i].opcode = 0x41;
    instructions[i].format = FMT_I;
    i++;
    instructions[i].mnemonic = "beq";
    instructions[i].opcode = 0x48;
    instructions[i].format = FMT_B;
    i++;
    instructions[i].mnemonic = "bne";
    instructions[i].opcode = 0x49;
    instructions[i].format = FMT_B;
    i++;
    instructions[i].mnemonic = "blt";
    instructions[i].opcode = 0x4A;
    instructions[i].format = FMT_B;
    i++;
    instructions[i].mnemonic = "bge";
    instructions[i].opcode = 0x4B;
    instructions[i].format = FMT_B;
    i++;
    instructions[i].mnemonic = "bltu";
    instructions[i].opcode = 0x4C;
    instructions[i].format = FMT_B;
    i++;
    instructions[i].mnemonic = "bgeu";
    instructions[i].opcode = 0x4D;
    instructions[i].format = FMT_B;
    i++;
    instructions[i].mnemonic = "nop";
    instructions[i].opcode = 0x50;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "yield";
    instructions[i].opcode = 0x51;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "debug";
    instructions[i].opcode = 0x52;
    instructions[i].format = FMT_I;
    i++;
    instructions[i].mnemonic = "fadd.s";
    instructions[i].opcode = 0x53;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fsub.s";
    instructions[i].opcode = 0x54;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fmul.s";
    instructions[i].opcode = 0x55;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fdiv.s";
    instructions[i].opcode = 0x56;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fsqrt.s";
    instructions[i].opcode = 0x57;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "feq.s";
    instructions[i].opcode = 0x58;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "flt.s";
    instructions[i].opcode = 0x59;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fle.s";
    instructions[i].opcode = 0x5A;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fcvt.w.s";
    instructions[i].opcode = 0x5B;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fcvt.wu.s";
    instructions[i].opcode = 0x5C;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fcvt.s.w";
    instructions[i].opcode = 0x5D;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fcvt.s.wu";
    instructions[i].opcode = 0x5E;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fneg.s";
    instructions[i].opcode = 0x5F;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fabs.s";
    instructions[i].opcode = 0x60;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fadd.d";
    instructions[i].opcode = 0x61;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fsub.d";
    instructions[i].opcode = 0x62;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fmul.d";
    instructions[i].opcode = 0x63;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fdiv.d";
    instructions[i].opcode = 0x64;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fsqrt.d";
    instructions[i].opcode = 0x65;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "feq.d";
    instructions[i].opcode = 0x66;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "flt.d";
    instructions[i].opcode = 0x67;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fle.d";
    instructions[i].opcode = 0x68;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fcvt.w.d";
    instructions[i].opcode = 0x69;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fcvt.wu.d";
    instructions[i].opcode = 0x6A;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fcvt.d.w";
    instructions[i].opcode = 0x6B;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fcvt.d.wu";
    instructions[i].opcode = 0x6C;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fcvt.d.s";
    instructions[i].opcode = 0x6D;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fcvt.s.d";
    instructions[i].opcode = 0x6E;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fneg.d";
    instructions[i].opcode = 0x6F;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fabs.d";
    instructions[i].opcode = 0x70;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fcvt.l.s";
    instructions[i].opcode = 0x71;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fcvt.lu.s";
    instructions[i].opcode = 0x72;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fcvt.s.l";
    instructions[i].opcode = 0x73;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fcvt.s.lu";
    instructions[i].opcode = 0x74;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fcvt.l.d";
    instructions[i].opcode = 0x75;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fcvt.lu.d";
    instructions[i].opcode = 0x76;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fcvt.d.l";
    instructions[i].opcode = 0x77;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "fcvt.d.lu";
    instructions[i].opcode = 0x78;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = "halt";
    instructions[i].opcode = 0x7F;
    instructions[i].format = FMT_R;
    i++;
    instructions[i].mnemonic = NULL;
    instructions[i].opcode = 0;
    instructions[i].format = FMT_UNKNOWN;
    instructions_initialized = true;
}


// ============================================================================
// Symbol table for label resolution
// ============================================================================

static int num_syms = 0;

static const char *lookup_symbol(uint32_t addr) {
    (void)addr;
    return NULL;
}

// ============================================================================
// Instruction field extraction
// ============================================================================

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
    int32_t imm = ((inst >> 7) & 0x1F) | (((int32_t)inst >> 25) << 5);
    return imm;
}

static inline int extract_imm_b(uint32_t inst) {
    int32_t imm = (((inst >> 8) & 0xF) << 1) | (((inst >> 25) & 0x3F) << 5) |
                  (((inst >> 7) & 0x1) << 11) | (((int32_t)inst >> 31) << 12);
    return imm;
}

static inline int extract_imm_u(uint32_t inst) {
    return inst & 0xFFFFF000;
}

static inline int extract_imm_j(uint32_t inst) {
    uint32_t imm20 = (inst >> 31) & 0x1;
    uint32_t imm10_1 = (inst >> 21) & 0x3FF;
    uint32_t imm11 = (inst >> 20) & 0x1;
    uint32_t imm19_12 = (inst >> 12) & 0xFF;
    int32_t imm = (imm20 << 20) | (imm19_12 << 12) | (imm11 << 11) | (imm10_1 << 1);
    if (imm & 0x100000) imm |= 0xFFE00000;
    return imm;
}

static instruction_info_t* find_instruction(uint32_t opcode) {
    init_instructions();
    for (int i = 0; instructions[i].mnemonic; i++) {
        if (instructions[i].opcode == opcode) {
            return &instructions[i];
        }
    }
    return NULL;
}

static const char* get_reg_name(int reg) {
    char *buf;

    if (reg == 0) return "zero";
    if (reg == 29) return "sp";
    if (reg == 30) return "fp";
    if (reg == 31) return "lr";

    if (reg_name_buf_idx == 0) {
        buf = reg_name_buf0;
    } else if (reg_name_buf_idx == 1) {
        buf = reg_name_buf1;
    } else if (reg_name_buf_idx == 2) {
        buf = reg_name_buf2;
    } else {
        buf = reg_name_buf3;
    }
    reg_name_buf_idx = (reg_name_buf_idx + 1) % 4;
    snprintf(buf, 16, "r%d", reg);
    return buf;
}

// ============================================================================
// Disassembly
// ============================================================================

static void disassemble_instruction(uint32_t addr, uint32_t inst) {
    uint32_t opcode = extract_opcode(inst);
    instruction_info_t *info = find_instruction(opcode);

    // Print label if this address has a symbol
    const char *label = lookup_symbol(addr);
    if (label) {
        printf("\n%s:\n", label);
    }

    printf("  0x%08x: %08x  ", addr, inst);

    if (!info) {
        printf("unknown opcode 0x%02x\n", opcode);
        return;
    }

    printf("%-10s", info->mnemonic);

    // Special cases
    if (opcode == 0x7F) {  // HALT
        printf("\n");
        return;
    }

    if (opcode == 0x52) {  // DEBUG
        int rs1 = extract_rs1(inst);
        printf("%s\n", get_reg_name(rs1));
        return;
    }

    switch (info->format) {
        case FMT_R: {
            int rd = extract_rd(inst);
            int rs1 = extract_rs1(inst);
            int rs2 = extract_rs2(inst);
            printf("%s, %s, %s", get_reg_name(rd), get_reg_name(rs1), get_reg_name(rs2));
            break;
        }

        case FMT_I: {
            int rd = extract_rd(inst);
            int rs1 = extract_rs1(inst);
            int imm = extract_imm_i(inst);

            if (opcode == 0x41) {  // jalr
                printf("%s, %s, %d", get_reg_name(rd), get_reg_name(rs1), imm);
            } else if (opcode >= 0x30 && opcode <= 0x34) {  // loads
                printf("%s, %s+%d", get_reg_name(rd), get_reg_name(rs1), imm);
            } else {
                printf("%s, %s, %d", get_reg_name(rd), get_reg_name(rs1), imm);
            }
            break;
        }

        case FMT_S: {
            int rs1 = extract_rs1(inst);
            int rs2 = extract_rs2(inst);
            int imm = extract_imm_s(inst);
            printf("%s+%d, %s", get_reg_name(rs1), imm, get_reg_name(rs2));
            break;
        }

        case FMT_B: {
            int rs1 = extract_rs1(inst);
            int rs2 = extract_rs2(inst);
            int imm = extract_imm_b(inst);
            uint32_t target = addr + 4 + imm;
            const char *target_sym = lookup_symbol(target);
            if (target_sym) {
                printf("%s, %s, %s", get_reg_name(rs1), get_reg_name(rs2), target_sym);
            } else {
                printf("%s, %s, 0x%x", get_reg_name(rs1), get_reg_name(rs2), target);
            }
            break;
        }

        case FMT_U: {
            int rd = extract_rd(inst);
            int imm = extract_imm_u(inst);
            printf("%s, 0x%x", get_reg_name(rd), imm >> 12);
            break;
        }

        case FMT_J: {
            int rd = extract_rd(inst);
            int imm = extract_imm_j(inst);
            uint32_t target = addr + imm;
            const char *target_sym = lookup_symbol(target);
            if (rd == 0) {
                if (target_sym) {
                    printf("%s", target_sym);
                } else {
                    printf("0x%x", target);
                }
            } else {
                if (target_sym) {
                    printf("%s, %s", get_reg_name(rd), target_sym);
                } else {
                    printf("%s, 0x%x", get_reg_name(rd), target);
                }
            }
            break;
        }

        default:
            break;
    }

    printf("\n");
}

// ============================================================================
// Symbol loading
// ============================================================================

// Load symbols from SYMTAB/STRTAB sections in a .s32x file
static void load_symbols_s32x(FILE *f, s32x_header_t *header) {
    (void)f;
    (void)header;
    num_syms = 0;
}

// Load symbols from .s32o object file (only code section symbols)
static void load_symbols_s32o(FILE *f, s32o_header_t *header) {
    (void)f;
    (void)header;
    num_syms = 0;
}

static void free_symbols(void) {
    num_syms = 0;
}

// ============================================================================
// Disassemble a code section
// ============================================================================

static void disassemble_section(FILE *f, const char *name, uint32_t vaddr,
                                uint32_t file_offset, uint32_t size,
                                uint32_t start_addr, uint32_t end_addr,
                                bool have_range) {
    printf("Section: %s at 0x%08x (%d bytes)\n", name, vaddr, size);
    printf("----------------------------------------\n");

    if (size == 0 || (size % 4) != 0) {
        fprintf(stderr, "Error: Invalid section size %u for '%s'\n", size, name);
        return;
    }

    if (size > (16u * 1024u * 1024u)) {
        fprintf(stderr, "Error: Section '%s' too large (%u bytes)\n", name, size);
        return;
    }

    fseek(f, file_offset, SEEK_SET);
    uint32_t *code = malloc(size);
    if (!code) return;
    if (fread(code, 1, size, f) != size) {
        fprintf(stderr, "Error: Failed to read section '%s'\n", name);
        free(code);
        return;
    }

    uint32_t start = 0;
    uint32_t end = size / 4;

    if (have_range) {
        if (start_addr >= vaddr) start = (start_addr - vaddr) / 4;
        if (end_addr >= vaddr) {
            uint32_t new_end = (end_addr - vaddr) / 4 + 1;
            if (new_end < end) end = new_end;
        }
    }

    for (uint32_t j = start; j < end && j < size / 4; j++) {
        disassemble_instruction(vaddr + j * 4, code[j]);
    }

    free(code);
    printf("\n");
}

// ============================================================================
// Main: auto-detect .s32x vs .s32o
// ============================================================================

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <file.s32x|file.s32o> [start_addr] [end_addr]\n", argv[0]);
        return 1;
    }

    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Cannot open file");
        return 1;
    }

    // Read magic to detect file type
    uint32_t magic;
    if (fread(&magic, sizeof(magic), 1, f) != 1) {
        fprintf(stderr, "Cannot read file header\n");
        fclose(f);
        return 1;
    }
    fseek(f, 0, SEEK_SET);

    // Parse optional address range
    bool have_range = (argc > 2);
    uint32_t start_addr = 0, end_addr = 0xFFFFFFFF;
    if (argc > 2) start_addr = strtoul(argv[2], NULL, 0);
    if (argc > 3) end_addr = strtoul(argv[3], NULL, 0);

    if (magic == S32X_MAGIC) {
        // ---- .s32x executable ----
        s32x_header_t header;
        if (fread(&header, sizeof(header), 1, f) != 1) {
            fprintf(stderr, "Cannot read S32X header\n");
            fclose(f);
            return 1;
        }

        printf("S32X Executable Disassembly\n");
        printf("Entry point: 0x%08x\n", header.entry);
        printf("Stack base:  0x%08x\n", header.stack_base);
        printf("\n");

        // Read section string table
        char *strtab = malloc(header.str_size);
        if (!strtab) { fclose(f); return 1; }
        fseek(f, header.str_offset, SEEK_SET);
        fread(strtab, 1, header.str_size, f);

        // Load symbols from SYMTAB section
        load_symbols_s32x(f, &header);
        if (num_syms > 0) {
            printf("Loaded %d symbols\n\n", num_syms);
        }

        // Disassemble code sections
        fseek(f, header.sec_offset, SEEK_SET);
        for (uint32_t i = 0; i < header.nsections; i++) {
            s32x_section_t sect;
            if (fread(&sect, sizeof(sect), 1, f) != 1) break;

            if (sect.type == S32_SEC_CODE) {
                long pos = ftell(f);
                const char *name = &strtab[sect.name_offset];
                disassemble_section(f, name, sect.vaddr, sect.offset, sect.size,
                                    start_addr, end_addr, have_range);
                fseek(f, pos, SEEK_SET);
            }
        }

        free(strtab);

    } else {
        fprintf(stderr, "Not a valid S32X file (magic: 0x%08x)\n", magic);
        fclose(f);
        return 1;
    }

    free_symbols();
    fclose(f);
    return 0;
}
