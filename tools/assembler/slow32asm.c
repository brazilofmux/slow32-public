#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <ctype.h>
#include <stdbool.h>
#include <getopt.h>
#include "../common/s32_formats.h"

#define MAX_LINE 256
#define MAX_TOKENS 8
#define MAX_LABELS 1024
#define MAX_INSTRUCTIONS 65536
#define MAX_DATA_SIZE (1024 * 1024)  // 1MB max data section
#define MAX_RELOCATIONS 4096
#define MAX_STRING_TABLE (64 * 1024) // 64KB string table

// SLOW-32 assembler now only outputs object files (.s32o)
// Binary executables are created by the linker (.s32x)

typedef enum {
    SECTION_CODE,
    SECTION_DATA,
    SECTION_BSS,
    SECTION_RODATA
} section_t;

typedef struct {
    char name[64];
    uint32_t address;
    section_t section;
    bool is_global;
    bool is_defined;
} label_t;

typedef struct {
    uint32_t offset;      // Offset in section
    char symbol[64];      // Symbol name
    uint32_t type;        // Relocation type
    int32_t addend;       // Addend
    section_t section;    // Which section this relocation is in
} relocation_t;

typedef struct {
    uint32_t opcode;
    uint32_t instruction;
    uint32_t address;
    bool has_label_ref;
    char label_ref[64];
    int label_offset;
    bool has_symbol_ref;    // True if this references a global symbol
    char symbol_ref[64];    // Symbol name for %hi()/%lo()
    bool symbol_is_hi;      // True if this is %hi(symbol)
    bool symbol_is_lo;      // True if this is %lo(symbol)
    section_t section;      // Which section this belongs to
    bool is_data_byte;      // True if this is a .db directive byte
    bool needs_relocation;  // True if this instruction references an undefined symbol
} instruction_t;

typedef struct {
    label_t labels[MAX_LABELS];
    int num_labels;
    instruction_t *instructions;  // Dynamically allocated
    int num_instructions;
    relocation_t relocations[MAX_RELOCATIONS];
    int num_relocations;
    uint32_t current_addr;
    section_t current_section;  // Track current section during assembly
    uint32_t code_size;         // Size of code section
    uint32_t data_size;         // Size of data section
    uint32_t bss_size;          // Size of bss section
    uint32_t rodata_size;       // Size of rodata section
    uint32_t data_start_addr;   // Starting address of data section (0x100000)
    bool generate_object;       // True for object file, false for executable
    
    // String table
    char string_table[MAX_STRING_TABLE];
    uint32_t string_table_size;
} assembler_t;

typedef enum {
    FMT_R, FMT_I, FMT_S, FMT_B, FMT_U, FMT_J, FMT_NONE
} inst_format_t;

typedef struct {
    const char *mnemonic;
    uint32_t opcode;
    inst_format_t format;
} instruction_def_t;

static instruction_def_t instruction_table[] = {
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
    
    {"lui",   0x20, FMT_U},
    
    {"ldb",   0x30, FMT_I},
    {"ldh",   0x31, FMT_I},
    {"ldw",   0x32, FMT_I},
    {"ldbu",  0x33, FMT_I},
    {"ldhu",  0x34, FMT_I},
    
    {"stb",   0x38, FMT_S},
    {"sth",   0x39, FMT_S},
    {"stw",   0x3A, FMT_S},
    
    {"jal",   0x40, FMT_J},
    {"jalr",  0x41, FMT_I},
    
    {"beq",   0x48, FMT_B},
    {"bne",   0x49, FMT_B},
    {"blt",   0x4A, FMT_B},
    {"bge",   0x4B, FMT_B},
    {"bltu",  0x4C, FMT_B},
    {"bgeu",  0x4D, FMT_B},
    
    {"nop",   0x50, FMT_NONE},
    {"yield", 0x51, FMT_R},
    {"debug", 0x52, FMT_R},
    {"halt",  0x7F, FMT_NONE},
    {"assert_eq", 0x3F, FMT_R},  // Testing instruction
    
    {NULL, 0, FMT_NONE}
};

static int parse_register(const char *str) {
    if (str[0] == 'r' || str[0] == 'R') {
        return atoi(str + 1);
    }
    // case-insensitive names
    char tmp[8]; int i=0;
    for (; str[i] && i < 7; ++i) tmp[i] = (char)tolower((unsigned char)str[i]);
    tmp[i] = '\0';
    if (strcmp(tmp, "zero") == 0) return 0;
    if (strcmp(tmp, "sp")   == 0) return 29;
    if (strcmp(tmp, "fp")   == 0) return 30;
    if (strcmp(tmp, "lr")   == 0) return 31;
    return -1;
}

// Check if a string starts with a valid identifier character
static bool is_identifier_start(char c) {
    return isalpha(c) || c == '_' || c == '.';
}

// Parse immediate value or %hi()/%lo() operator
// Returns -1 if it's a symbolic reference that needs relocation
static int parse_immediate_or_symbol(const char *str, char *symbol_out, int *is_hi, int *is_lo) {
    // Check for %hi(symbol)
    if (strncmp(str, "%hi(", 4) == 0) {
        const char *end = strchr(str + 4, ')');
        if (end) {
            int len = end - (str + 4);
            strncpy(symbol_out, str + 4, len);
            symbol_out[len] = '\0';
            *is_hi = 1;
            *is_lo = 0;
            return -1;  // Needs relocation
        }
    }
    // Check for %lo(symbol)
    else if (strncmp(str, "%lo(", 4) == 0) {
        const char *end = strchr(str + 4, ')');
        if (end) {
            int len = end - (str + 4);
            strncpy(symbol_out, str + 4, len);
            symbol_out[len] = '\0';
            *is_hi = 0;
            *is_lo = 1;
            return -1;  // Needs relocation
        }
    }
    
    // Regular immediate
    *is_hi = 0;
    *is_lo = 0;
    
    // Check if it's a number or a symbol
    if (str[0] == '0' && str[1] == 'x') {
        symbol_out[0] = '\0';
        return (int)strtol(str + 2, NULL, 16);
    } else if (str[0] == '-' || (str[0] >= '0' && str[0] <= '9')) {
        // It's a decimal number
        symbol_out[0] = '\0';
        return atoi(str);
    } else {
        // It's a symbol name - needs relocation
        strncpy(symbol_out, str, 63);
        symbol_out[63] = '\0';
        return -1;
    }
}

// Compatibility wrapper for old code
static int parse_immediate(const char *str) {
    char dummy[64];
    int hi, lo;
    return parse_immediate_or_symbol(str, dummy, &hi, &lo);
}

// Helper to update the size of the current section
static void bump_size(assembler_t* as, section_t sec, uint32_t n) {
    switch (sec) {
        case SECTION_CODE:   as->code_size   += n; break;
        case SECTION_DATA:   as->data_size   += n; break;
        case SECTION_RODATA: as->rodata_size += n; break;
        case SECTION_BSS:    as->bss_size    += n; break;
    }
}

static char *trim(char *str) {
    while (isspace(*str)) str++;
    char *end = str + strlen(str) - 1;
    while (end > str && isspace(*end)) *end-- = '\0';
    return str;
}

static int tokenize(char *line, char tokens[][64]) {
    int count = 0;
    char *p = line;
    
    while (*p && count < MAX_TOKENS) {
        // Skip whitespace and commas
        while (*p && (*p == ' ' || *p == '\t' || *p == ',')) p++;
        if (!*p) break;
        
        char *token_start = tokens[count];
        char *token_ptr = token_start;
        
        // Handle quoted strings
        if (*p == '"') {
            // Copy the entire quoted string including quotes
            *token_ptr++ = *p++;
            while (*p && *p != '"') {
                if (*p == '\\' && *(p+1)) {
                    *token_ptr++ = *p++;
                    *token_ptr++ = *p++;
                } else {
                    *token_ptr++ = *p++;
                }
            }
            if (*p == '"') {
                *token_ptr++ = *p++;
            }
            *token_ptr = '\0';
            count++;
        }
        // Handle brackets
        else if (*p == '[' || *p == ']') {
            *token_ptr++ = *p++;
            *token_ptr = '\0';
            count++;
        }
        // Handle regular tokens
        else {
            while (*p && *p != ' ' && *p != '\t' && *p != ',' && *p != '[' && *p != ']') {
                *token_ptr++ = *p++;
            }
            *token_ptr = '\0';
            if (token_ptr > token_start) {
                count++;
            }
        }
    }
    
    return count;
}

static instruction_def_t *find_instruction(const char *mnemonic) {
    for (int i = 0; instruction_table[i].mnemonic; i++) {
        if (strcmp(instruction_table[i].mnemonic, mnemonic) == 0) {
            return &instruction_table[i];
        }
    }
    return NULL;
}

static void add_label(assembler_t *as, const char *name, uint32_t addr) {
    if (as->num_labels >= MAX_LABELS) {
        fprintf(stderr, "Too many labels\n");
        exit(1);
    }
    strcpy(as->labels[as->num_labels].name, name);
    as->labels[as->num_labels].address = addr;
    as->labels[as->num_labels].section = as->current_section;
    as->labels[as->num_labels].is_global = false;  // Default to local
    as->labels[as->num_labels].is_defined = true;
    as->num_labels++;
}

// Add string to string table, return offset
static uint32_t add_string(assembler_t *as, const char *str) {
    if (str == NULL || *str == '\0') {
        return 0;  // Empty string at offset 0
    }
    
    // Check if string already exists
    uint32_t offset = 1;  // Skip null at beginning
    while (offset < as->string_table_size) {
        if (strcmp(&as->string_table[offset], str) == 0) {
            return offset;
        }
        offset += strlen(&as->string_table[offset]) + 1;
    }
    
    // Add new string
    size_t len = strlen(str) + 1;
    if (as->string_table_size + len > MAX_STRING_TABLE) {
        fprintf(stderr, "String table overflow\n");
        return 0;
    }
    
    offset = as->string_table_size;
    strcpy(&as->string_table[offset], str);
    as->string_table_size += len;
    
    return offset;
}

// Add a relocation
static void add_relocation(assembler_t *as, uint32_t offset, const char *symbol, 
                           uint32_t type, int32_t addend, section_t section) {
    if (as->num_relocations >= MAX_RELOCATIONS) {
        fprintf(stderr, "Too many relocations\n");
        return;
    }
    
    relocation_t *rel = &as->relocations[as->num_relocations++];
    rel->offset = offset;
    strncpy(rel->symbol, symbol, sizeof(rel->symbol) - 1);
    rel->type = type;
    rel->addend = addend;
    rel->section = section;
}

static int find_label_index(assembler_t *as, const char *name) {
    for (int i = 0; i < as->num_labels; i++) {
        if (strcmp(as->labels[i].name, name) == 0) {
            return i;
        }
    }
    return -1;
}

static label_t *find_label(assembler_t *as, const char *name) {
    int idx = find_label_index(as, name);
    return idx >= 0 ? &as->labels[idx] : NULL;
}

// Process a string literal and emit bytes
// Returns the number of bytes processed, or -1 on error
static int process_string_literal(assembler_t *as, const char *str) {
    int bytes_emitted = 0;
    const char *p = str;
    
    // Skip opening quote
    if (*p != '"') return -1;
    p++;
    
    while (*p && *p != '"') {
        uint8_t byte;
        
        if (*p == '\\' && *(p+1)) {
            // Handle escape sequences
            p++;
            switch (*p) {
                case 'n': byte = '\n'; break;
                case 't': byte = '\t'; break;
                case 'r': byte = '\r'; break;
                case '\\': byte = '\\'; break;
                case '"': byte = '"'; break;
                case '0': byte = '\0'; break;
                case 'x': // Hex escape \xHH
                    if (*(p+1) && *(p+2)) {
                        char hex[3] = {*(p+1), *(p+2), '\0'};
                        byte = (uint8_t)strtol(hex, NULL, 16);
                        p += 2;
                    } else {
                        return -1; // Invalid hex escape
                    }
                    break;
                default:
                    byte = *p; // Unknown escape, use literal
                    break;
            }
            p++;
        } else {
            byte = *p++;
        }
        
        // Emit the byte
        as->instructions[as->num_instructions].instruction = byte | 0x80000000;
        as->instructions[as->num_instructions].address = as->current_addr;
        as->instructions[as->num_instructions].section = as->current_section;
        as->instructions[as->num_instructions].is_data_byte = true;
        as->num_instructions++;
        as->current_addr++;
        bump_size(as, as->current_section, 1);
        bytes_emitted++;
    }
    
    if (*p != '"') return -1; // Unterminated string
    
    return bytes_emitted;
}

static uint32_t encode_r(uint32_t op, int rd, int rs1, int rs2) {
    return op | (rd << 7) | (rs1 << 15) | (rs2 << 20);
}

static uint32_t encode_i(uint32_t op, int rd, int rs1, int imm) {
    return op | (rd << 7) | (rs1 << 15) | ((imm & 0xFFF) << 20);
}

static uint32_t encode_s(uint32_t op, int rs1, int rs2, int imm) {
    int imm_11_5 = (imm >> 5) & 0x7F;
    int imm_4_0 = imm & 0x1F;
    return op | (imm_4_0 << 7) | (rs1 << 15) | (rs2 << 20) | (imm_11_5 << 25);
}

static uint32_t encode_b(uint32_t op, int rs1, int rs2, int imm) {
    int imm_12 = (imm >> 12) & 1;
    int imm_11 = (imm >> 11) & 1;
    int imm_10_5 = (imm >> 5) & 0x3F;
    int imm_4_1 = (imm >> 1) & 0xF;
    
    return op | (imm_11 << 7) | (imm_4_1 << 8) | 
           (rs1 << 15) | (rs2 << 20) | 
           (imm_10_5 << 25) | (imm_12 << 31);
}

static uint32_t encode_u(uint32_t op, int rd, int imm) {
    // LUI loads a 20-bit immediate into bits [31:12]
    // This is the RISC-V standard U-format encoding
    // The immediate value should already be the 20-bit value to load
    return op | (rd << 7) | ((imm & 0xFFFFF) << 12);
}

static uint32_t encode_j(uint32_t op, int rd, int imm) {
    int imm_20 = (imm >> 20) & 1;
    int imm_19_12 = (imm >> 12) & 0xFF;
    int imm_11 = (imm >> 11) & 1;
    int imm_10_1 = (imm >> 1) & 0x3FF;
    
    return op | (rd << 7) | (imm_19_12 << 12) | 
           (imm_11 << 20) | (imm_10_1 << 21) | (imm_20 << 31);
}

static bool assemble_line(assembler_t *as, char *line) {
    char *comment = strchr(line, '#');
    if (comment) *comment = '\0';
    
    line = trim(line);
    if (strlen(line) == 0) return true;
    
    // Find colon for label, but not inside quoted strings
    char *colon = NULL;
    char *p = line;
    bool in_quotes = false;
    while (*p) {
        if (*p == '"') {
            in_quotes = !in_quotes;
        } else if (*p == ':' && !in_quotes) {
            colon = p;
            break;
        }
        p++;
    }
    
    if (colon) {
        *colon = '\0';
        char *label_name = trim(line);
        
        // Check if label already exists (e.g., from .global)
        label_t *existing = find_label(as, label_name);
        if (existing) {
            // Update existing label
            existing->address = as->current_addr;
            existing->section = as->current_section;
            existing->is_defined = true;
        } else {
            // Create new label
            add_label(as, label_name, as->current_addr);
        }
        
        line = trim(colon + 1);
        if (strlen(line) == 0) return true;
    }
    
    char tokens[MAX_TOKENS][64];
    int num_tokens = tokenize(line, tokens);
    if (num_tokens == 0) return true;
    
    // Lowercase only mnemonic/directive (tokens[0]); keep operand/symbol case
    for (char *pp = tokens[0]; *pp; ++pp) *pp = (char)tolower((unsigned char)*pp);
    
    // Handle directives
    if (tokens[0][0] == '.') {
        if (strcmp(tokens[0], ".data") == 0) {
            // Switch to data section
            as->current_section = SECTION_DATA;
            // In object mode, sections are always relative (start at 0)
            // In executable mode, data section starts at absolute address
            if (as->generate_object) {
                as->current_addr = as->data_size;
            } else {
                // Update current address to data section if this is the first time
                if (as->data_size == 0) {
                    as->current_addr = as->data_start_addr;
                }
            }
            return true;
        } else if (strcmp(tokens[0], ".text") == 0 || strcmp(tokens[0], ".code") == 0) {
            // Switch to code section
            as->current_section = SECTION_CODE;
            // Reset to code address
            as->current_addr = as->code_size;
            return true;
        } else if (strcmp(tokens[0], ".bss") == 0) {
            // Switch to BSS section (uninitialized data)
            as->current_section = SECTION_BSS;
            as->current_addr = as->bss_size;
            return true;
        } else if (strcmp(tokens[0], ".rodata") == 0) {
            // Switch to read-only data section
            as->current_section = SECTION_RODATA;
            as->current_addr = as->rodata_size;
            return true;
        } else if (strcmp(tokens[0], ".section") == 0) {
            // Handle .section directive with arguments
            // Example: .section .rodata.str1.1,"aMS",@progbits,1
            if (num_tokens > 1) {
                // Check if it's a rodata variant
                if (strncmp(tokens[1], ".rodata", 7) == 0) {
                    as->current_section = SECTION_RODATA;
                    as->current_addr = as->rodata_size;
                    return true;
                }
                // Ignore other sections for now (like .note.GNU-stack)
            }
            return true;
        } else if (strcmp(tokens[0], ".global") == 0 || strcmp(tokens[0], ".globl") == 0) {
            // Mark symbol as global
            if (num_tokens > 1) {
                label_t *label = find_label(as, tokens[1]);
                if (label) {
                    label->is_global = true;
                } else {
                    // Create placeholder for forward reference
                    add_label(as, tokens[1], 0);
                    as->labels[as->num_labels - 1].is_global = true;
                    as->labels[as->num_labels - 1].is_defined = false;
                }
            }
            return true;
        } else if (strcmp(tokens[0], ".word") == 0 && num_tokens > 1) {
            // .word - emit 32-bit words (numbers or symbols → REL_32)
            for (int i = 1; i < num_tokens; i++) {
                char sym[64]; int is_hi=0, is_lo=0;
                int val = parse_immediate_or_symbol(tokens[i], sym, &is_hi, &is_lo);
                if (val == -1) {
                    if (is_hi || is_lo) {
                        fprintf(stderr, ".word %%hi/%%lo not supported here; use plain symbol or split words\n");
                        return false;
                    }
                    // placeholder + relocation
                    as->instructions[as->num_instructions].instruction = 0;
                    as->instructions[as->num_instructions].address = as->current_addr;
                    as->instructions[as->num_instructions].section = as->current_section;
                    as->instructions[as->num_instructions].is_data_byte = false;
                    as->num_instructions++;
                    add_relocation(as, as->current_addr, sym, S32O_REL_32, 0, as->current_section);
                    if (getenv("DEBUG_RELOC")) {
                        fprintf(stderr, "Added REL_32 relocation for symbol '%s' at offset 0x%x in section %d\n",
                                sym, as->current_addr, as->current_section);
                    }
                } else {
                    as->instructions[as->num_instructions].instruction = (uint32_t)val;
                    as->instructions[as->num_instructions].address = as->current_addr;
                    as->instructions[as->num_instructions].section = as->current_section;
                    as->instructions[as->num_instructions].is_data_byte = false;
                    as->num_instructions++;
                }
                as->current_addr += 4;
                bump_size(as, as->current_section, 4);
            }
            return true;
        } else if (strcmp(tokens[0], ".half") == 0 && num_tokens > 1) {
            // .half - emit 16-bit halfwords
            for (int i = 1; i < num_tokens; i++) {
                uint16_t value = parse_immediate(tokens[i]) & 0xFFFF;
                // Store as two bytes in little-endian order
                as->instructions[as->num_instructions].instruction = (value & 0xFF) | 0x80000000;
                as->instructions[as->num_instructions].address = as->current_addr;
                as->instructions[as->num_instructions].section = as->current_section;
                as->instructions[as->num_instructions].is_data_byte = true;
                as->num_instructions++;
                as->current_addr += 1;
                
                as->instructions[as->num_instructions].instruction = ((value >> 8) & 0xFF) | 0x80000000;
                as->instructions[as->num_instructions].address = as->current_addr;
                as->instructions[as->num_instructions].section = as->current_section;
                as->instructions[as->num_instructions].is_data_byte = true;
                as->num_instructions++;
                as->current_addr += 1;
                
                bump_size(as, as->current_section, 2);
            }
            return true;
        } else if (strcmp(tokens[0], ".align") == 0) {
            // .align N - align to 2^N boundary (default 2 = word alignment)
            int align_power = 2;  // Default to word alignment
            if (num_tokens > 1) {
                align_power = parse_immediate(tokens[1]);
            }
            int align_bytes = 1 << align_power;
            int align_mask = align_bytes - 1;
            
            // Calculate padding needed
            int padding = 0;
            if (as->current_addr & align_mask) {
                padding = align_bytes - (as->current_addr & align_mask);
            }
            
            // Add NOP instructions (0x00000000) or zero bytes as padding
            for (int p = 0; p < padding; p++) {
                as->instructions[as->num_instructions].instruction = 0x00000000 | 0x80000000;
                as->instructions[as->num_instructions].address = as->current_addr;
                as->instructions[as->num_instructions].section = as->current_section;
                as->instructions[as->num_instructions].is_data_byte = true;
                as->num_instructions++;
                as->current_addr += 1;
                bump_size(as, as->current_section, 1);
            }
            return true;
        } else if (strcmp(tokens[0], ".byte") == 0) {
            // .byte - emit 8-bit bytes (can be numbers or strings)
            for (int i = 1; i < num_tokens; i++) {
                if (tokens[i][0] == '"') {
                    // Process string literal
                    int result = process_string_literal(as, tokens[i]);
                    if (result < 0) {
                        fprintf(stderr, "Invalid string literal: %s\n", tokens[i]);
                        return false;
                    }
                } else {
                    // Process numeric byte
                    uint8_t byte = parse_immediate(tokens[i]) & 0xFF;
                    as->instructions[as->num_instructions].instruction = byte | 0x80000000;
                    as->instructions[as->num_instructions].address = as->current_addr;
                    as->instructions[as->num_instructions].section = as->current_section;
                    as->instructions[as->num_instructions].is_data_byte = true;
                    as->num_instructions++;
                    as->current_addr += 1;
                    bump_size(as, as->current_section, 1);
                }
            }
            return true;
        } else if ((strcmp(tokens[0], ".string") == 0 || strcmp(tokens[0], ".asciz") == 0) && num_tokens > 1) {
            // .string/.asciz - each arg is a separate NUL-terminated string
            for (int i = 1; i < num_tokens; i++) {
                if (tokens[i][0] == '"') {
                    int result = process_string_literal(as, tokens[i]);
                    if (result < 0) {
                        fprintf(stderr, "Invalid string literal: %s\n", tokens[i]);
                        return false;
                    }
                    // NUL terminator after each string
                    as->instructions[as->num_instructions].instruction = 0x80000000;
                    as->instructions[as->num_instructions].address = as->current_addr;
                    as->instructions[as->num_instructions].section = as->current_section;
                    as->instructions[as->num_instructions].is_data_byte = true;
                    as->num_instructions++;
                    as->current_addr++;
                    bump_size(as, as->current_section, 1);
                } else {
                    fprintf(stderr, ".string expects string literals\n");
                    return false;
                }
            }
            return true;
        } else if (strcmp(tokens[0], ".ascii") == 0 && num_tokens > 1) {
            // Emit string without null terminator
            for (int i = 1; i < num_tokens; i++) {
                if (tokens[i][0] == '"') {
                    int result = process_string_literal(as, tokens[i]);
                    if (result < 0) {
                        fprintf(stderr, "Invalid string literal: %s\n", tokens[i]);
                        return false;
                    }
                } else {
                    fprintf(stderr, ".ascii expects string literals\n");
                    return false;
                }
            }
            return true;
        }
        // Ignore other directives
        return true;
    }
    
    instruction_def_t *inst_def = find_instruction(tokens[0]);
    if (!inst_def) {
        fprintf(stderr, "Unknown instruction: %s\n", tokens[0]);
        return false;
    }
    
    // Auto-align to word boundary before instructions in code section
    if (as->current_section == SECTION_CODE && (as->current_addr & 3)) {
        int padding = 4 - (as->current_addr & 3);
        for (int p = 0; p < padding; p++) {
            as->instructions[as->num_instructions].instruction = 0x00000000 | 0x80000000;
            as->instructions[as->num_instructions].address = as->current_addr;
            as->instructions[as->num_instructions].section = as->current_section;
            as->instructions[as->num_instructions].is_data_byte = true;
            as->num_instructions++;
            as->current_addr += 1;
            bump_size(as, SECTION_CODE, 1);
        }
    }
    
    instruction_t *inst = &as->instructions[as->num_instructions];
    inst->opcode = inst_def->opcode;
    inst->address = as->current_addr;
    inst->has_label_ref = false;
    inst->has_symbol_ref = false;
    inst->symbol_is_hi = false;
    inst->symbol_is_lo = false;
    inst->section = as->current_section;
    inst->is_data_byte = false;
    
    switch (inst_def->format) {
        case FMT_R: {
            if (inst->opcode == 0x51) { // YIELD - only needs rs1
                if (num_tokens < 2) return false;
                int rs1 = parse_register(tokens[1]);
                inst->instruction = encode_r(inst->opcode, 0, rs1, 0);
            } else if (inst->opcode == 0x52) { // DEBUG - only needs rs1
                if (num_tokens < 2) return false;
                int rs1 = parse_register(tokens[1]);
                inst->instruction = encode_r(inst->opcode, 0, rs1, 0);
            } else if (inst->opcode == 0x3F) { // ASSERT_EQ - needs rs1, rs2
                if (num_tokens < 3) return false;
                int rs1 = parse_register(tokens[1]);
                int rs2 = parse_register(tokens[2]);
                inst->instruction = encode_r(inst->opcode, 0, rs1, rs2);
            } else {
                if (num_tokens < 4) return false;
                int rd = parse_register(tokens[1]);
                int rs1 = parse_register(tokens[2]);
                int rs2 = parse_register(tokens[3]);
                inst->instruction = encode_r(inst->opcode, rd, rs1, rs2);
            }
            break;
        }
        
        case FMT_I: {
            if (inst->opcode >= 0x30 && inst->opcode <= 0x34) {
                if (num_tokens < 3) return false;
                int rd = parse_register(tokens[1]);
                int rs1, imm;
                
                char *plus = strchr(tokens[2], '+');
                if (plus) {
                    *plus = '\0';
                    rs1 = parse_register(tokens[2]);
                    
                    // Check for %lo(symbol) in loads
                    char symbol[64];
                    int is_hi, is_lo;
                    imm = parse_immediate_or_symbol(plus + 1, symbol, &is_hi, &is_lo);
                    
                    if (imm == -1 && is_lo) {
                        inst->has_symbol_ref = true;
                        inst->symbol_is_lo = true;
                        strcpy(inst->symbol_ref, symbol);
                        inst->instruction = encode_i(inst->opcode, rd, rs1, 0);
                    } else {
                        inst->instruction = encode_i(inst->opcode, rd, rs1, imm);
                    }
                } else {
                    rs1 = parse_register(tokens[2]);
                    imm = (num_tokens > 3) ? parse_immediate(tokens[3]) : 0;
                    inst->instruction = encode_i(inst->opcode, rd, rs1, imm);
                }
            } else {
                if (num_tokens < 4) return false;
                int rd = parse_register(tokens[1]);
                int rs1 = parse_register(tokens[2]);
                
                // Check for %lo(symbol)
                char symbol[64];
                int is_hi, is_lo;
                int imm = parse_immediate_or_symbol(tokens[3], symbol, &is_hi, &is_lo);
                
                if (imm == -1 && is_lo) {
                    // This is a %lo(symbol) reference - needs LO12 relocation
                    inst->has_symbol_ref = true;
                    inst->symbol_is_hi = false;
                    inst->symbol_is_lo = true;
                    strcpy(inst->symbol_ref, symbol);
                    inst->instruction = encode_i(inst->opcode, rd, rs1, 0);  // Placeholder
                } else if (is_identifier_start(tokens[3][0])) {
                    // Raw labels in I-type are not supported
                    fprintf(stderr, "Error: Raw label '%s' in I-type instruction. Use %%hi/%%lo(symbol) instead.\n", tokens[3]);
                    return false;
                } else {
                    inst->instruction = encode_i(inst->opcode, rd, rs1, imm);
                }
            }
            break;
        }
        
        case FMT_S: {
            if (num_tokens < 3) return false;
            int rs1, rs2, imm = 0;
            
            char *plus = strchr(tokens[1], '+');
            if (plus) {
                *plus = '\0';
                rs1 = parse_register(tokens[1]);
                
                // Check for %lo(symbol) in stores
                char symbol[64];
                int is_hi, is_lo;
                imm = parse_immediate_or_symbol(plus + 1, symbol, &is_hi, &is_lo);
                
                if (imm == -1 && is_lo) {
                    inst->has_symbol_ref = true;
                    inst->symbol_is_lo = true;
                    strcpy(inst->symbol_ref, symbol);
                    imm = 0;  // Placeholder
                }
            } else {
                rs1 = parse_register(tokens[1]);
                if (num_tokens > 3) {
                    // Check for %lo(symbol) in 3-operand form
                    char symbol[64];
                    int is_hi, is_lo;
                    imm = parse_immediate_or_symbol(tokens[3], symbol, &is_hi, &is_lo);
                    
                    if (imm == -1 && is_lo) {
                        inst->has_symbol_ref = true;
                        inst->symbol_is_lo = true;
                        strcpy(inst->symbol_ref, symbol);
                        imm = 0;  // Placeholder
                    }
                }
            }
            rs2 = parse_register(tokens[2]);
            inst->instruction = encode_s(inst->opcode, rs1, rs2, imm);
            break;
        }
        
        case FMT_B: {
            if (num_tokens < 3) return false;
            int rs1 = parse_register(tokens[1]);
            int rs2 = parse_register(tokens[2]);
            
            if (num_tokens > 3 && is_identifier_start(tokens[3][0])) {
                inst->has_label_ref = true;
                strcpy(inst->label_ref, tokens[3]);
                inst->label_offset = 0;
                inst->instruction = encode_b(inst->opcode, rs1, rs2, 0);
            } else if (num_tokens > 3) {
                int imm = parse_immediate(tokens[3]);
                inst->instruction = encode_b(inst->opcode, rs1, rs2, imm);
            }
            break;
        }
        
        case FMT_U: {
            if (num_tokens < 3) return false;
            int rd = parse_register(tokens[1]);
            
            // Check for %hi(symbol)
            char symbol[64];
            int is_hi, is_lo;
            int imm = parse_immediate_or_symbol(tokens[2], symbol, &is_hi, &is_lo);
            
            if (imm == -1 && is_hi) {
                // This is a %hi(symbol) reference - needs HI20 relocation
                inst->has_symbol_ref = true;
                inst->symbol_is_hi = true;
                inst->symbol_is_lo = false;
                strcpy(inst->symbol_ref, symbol);
                inst->instruction = encode_u(inst->opcode, rd, 0);  // Placeholder
            } else {
                inst->instruction = encode_u(inst->opcode, rd, imm);
            }
            break;
        }
        
        case FMT_J: {
            if (num_tokens < 2) return false;
            int rd = (num_tokens > 2) ? parse_register(tokens[1]) : 31;
            
            char *target = (num_tokens > 2) ? tokens[2] : tokens[1];
            if (is_identifier_start(target[0])) {
                inst->has_label_ref = true;
                strcpy(inst->label_ref, target);
                inst->label_offset = 0;
                inst->instruction = encode_j(inst->opcode, rd, 0);
            } else {
                int imm = parse_immediate(target);
                inst->instruction = encode_j(inst->opcode, rd, imm);
            }
            break;
        }
        
        case FMT_NONE: {
            inst->instruction = inst->opcode;
            break;
        }
    }
    
    as->num_instructions++;
    as->current_addr += 4;
    bump_size(as, as->current_section, 4);
    return true;
}


// Write object file (.s32o format)
static int write_object_file(assembler_t *as, const char *filename) {
    FILE *f = fopen(filename, "wb");
    if (!f) {
        fprintf(stderr, "Cannot open output file: %s\n", filename);
        return -1;
    }
    
    // Initialize string table with empty string at offset 0
    as->string_table[0] = '\0';
    as->string_table_size = 1;
    
    // Count sections and add section names to string table
    int num_sections = 0;
    uint32_t text_name_offset = 0, data_name_offset = 0, bss_name_offset = 0, rodata_name_offset = 0;
    
    // Count relocations per section  
    int text_relocs = 0, data_relocs = 0, rodata_relocs = 0, bss_relocs = 0;
    for (int i = 0; i < as->num_relocations; i++) {
        switch (as->relocations[i].section) {
            case SECTION_CODE: text_relocs++; break;
            case SECTION_DATA: data_relocs++; break;
            case SECTION_RODATA: rodata_relocs++; break;
            case SECTION_BSS: bss_relocs++; break;
        }
    }
    
    if (as->code_size > 0) {
        text_name_offset = add_string(as, ".text");
        num_sections++;
    }
    if (as->rodata_size > 0) {
        rodata_name_offset = add_string(as, ".rodata");
        num_sections++;
    }
    if (as->data_size > 0) {
        data_name_offset = add_string(as, ".data");
        num_sections++;
    }
    if (as->bss_size > 0) {
        bss_name_offset = add_string(as, ".bss");
        num_sections++;
    }
    
    // Create mapping from internal section enum to output section index
    int section_map[4] = {0, 0, 0, 0};  // Maps SECTION_* to output section index
    int output_section_idx = 1;  // Output sections are 1-based
    if (as->code_size > 0) section_map[SECTION_CODE] = output_section_idx++;
    if (as->rodata_size > 0) section_map[SECTION_RODATA] = output_section_idx++;
    if (as->data_size > 0) section_map[SECTION_DATA] = output_section_idx++;
    if (as->bss_size > 0) section_map[SECTION_BSS] = output_section_idx++;
    
    // Build symbol table
    int num_symbols = 0;
    s32o_symbol_t symbols[MAX_LABELS];
    
    // 1) Add all global labels
    for (int i = 0; i < as->num_labels; i++) {
        if (!as->labels[i].is_global) continue;
        
        symbols[num_symbols].name_offset = add_string(as, as->labels[i].name);
        symbols[num_symbols].value = as->labels[i].address;
        symbols[num_symbols].section = section_map[as->labels[i].section];
        symbols[num_symbols].type = S32O_SYM_NOTYPE;
        symbols[num_symbols].binding = S32O_BIND_GLOBAL;
        symbols[num_symbols].size = 0;
        num_symbols++;
    }
    
    // 2) Ensure every relocation target has a symtab entry:
    //    - if it's a defined local label in this file -> add as LOCAL/defined
    //    - otherwise -> add as GLOBAL/undefined (linker will resolve)
    for (int i = 0; i < as->num_relocations; i++) {
        const char *sym = as->relocations[i].symbol;
        if (!sym || !sym[0]) continue;
        
        // Already present?
        bool present = false;
        for (int j = 0; j < num_symbols; j++) {
            const char *sym_name = &as->string_table[symbols[j].name_offset];
            if (strcmp(sym_name, sym) == 0) {
                present = true;
                break;
            }
        }
        if (present) continue;
        
        // Is it a defined label in this object (likely a local .L*)?
        int lbl = find_label_index(as, sym);
        //fprintf(stderr, "DEBUG: Looking for symbol '%s', lbl=%d\n", sym, lbl);
        if (lbl >= 0 && as->labels[lbl].is_defined) {
            //fprintf(stderr, "DEBUG: Adding local symbol '%s' at section %d\n", sym, section_map[as->labels[lbl].section]);
            symbols[num_symbols].name_offset = add_string(as, sym);
            symbols[num_symbols].value = as->labels[lbl].address;
            symbols[num_symbols].section = section_map[as->labels[lbl].section];
            symbols[num_symbols].type = S32O_SYM_NOTYPE;
            symbols[num_symbols].binding = S32O_BIND_LOCAL;
            symbols[num_symbols].size = 0;
            num_symbols++;
        } else {
            // treat as undefined, let the linker resolve from other objects
            symbols[num_symbols].name_offset = add_string(as, sym);
            symbols[num_symbols].value = 0;
            symbols[num_symbols].section = 0; // Undefined
            symbols[num_symbols].type = S32O_SYM_NOTYPE;
            symbols[num_symbols].binding = S32O_BIND_GLOBAL;
            symbols[num_symbols].size = 0;
            num_symbols++;
        }
    }
    
    // Calculate file layout
    uint32_t offset = sizeof(s32o_header_t);
    uint32_t section_table_offset = offset;
    offset += num_sections * sizeof(s32o_section_t);
    
    uint32_t symbol_table_offset = offset;
    offset += num_symbols * sizeof(s32o_symbol_t);
    
    // Calculate relocation table offsets
    uint32_t text_reloc_offset = 0, data_reloc_offset = 0;
    uint32_t rodata_reloc_offset = 0, bss_reloc_offset = 0;
    
    if (text_relocs > 0) {
        text_reloc_offset = offset;
        offset += text_relocs * sizeof(s32o_reloc_t);
    }
    if (data_relocs > 0) {
        data_reloc_offset = offset;
        offset += data_relocs * sizeof(s32o_reloc_t);
    }
    if (rodata_relocs > 0) {
        rodata_reloc_offset = offset;
        offset += rodata_relocs * sizeof(s32o_reloc_t);
    }
    if (bss_relocs > 0) {
        bss_reloc_offset = offset;
        offset += bss_relocs * sizeof(s32o_reloc_t);
    }
    
    uint32_t string_table_offset = offset;
    offset += as->string_table_size;
    
    // Align to 4 bytes
    offset = (offset + 3) & ~3;
    
    // Section data offsets
    uint32_t code_offset = 0, data_offset = 0, rodata_offset = 0;
    if (as->code_size > 0) {
        code_offset = offset;
        offset += as->code_size;
    }
    if (as->rodata_size > 0) {
        rodata_offset = offset;
        offset += as->rodata_size;
    }
    if (as->data_size > 0) {
        data_offset = offset;
        offset += as->data_size;
    }
    // BSS has no data in file
    
    // Write header
    s32o_header_t header = {
        .magic = S32O_MAGIC,
        .version = 1,
        .endian = S32_ENDIAN_LITTLE,
        .machine = S32_MACHINE_SLOW32,
        .flags = 0,
        .nsections = num_sections,
        .sec_offset = section_table_offset,
        .nsymbols = num_symbols,
        .sym_offset = symbol_table_offset,
        .str_offset = string_table_offset,
        .str_size = as->string_table_size,
        .checksum = 0
    };
    
    fwrite(&header, sizeof(header), 1, f);
    
    // Write section table
    if (as->code_size > 0) {
        s32o_section_t section = {
            .name_offset = text_name_offset,
            .type = S32_SEC_CODE,
            .flags = S32_SEC_FLAG_READ | S32_SEC_FLAG_EXEC | S32_SEC_FLAG_ALLOC,
            .size = as->code_size,
            .offset = code_offset,
            .align = 4,
            .nrelocs = text_relocs,
            .reloc_offset = text_reloc_offset
        };
        fwrite(&section, sizeof(section), 1, f);
    }
    
    if (as->rodata_size > 0) {
        s32o_section_t section = {
            .name_offset = rodata_name_offset,
            .type = S32_SEC_RODATA,
            .flags = S32_SEC_FLAG_READ | S32_SEC_FLAG_ALLOC,
            .size = as->rodata_size,
            .offset = rodata_offset,
            .align = 4,
            .nrelocs = rodata_relocs,
            .reloc_offset = rodata_reloc_offset
        };
        fwrite(&section, sizeof(section), 1, f);
    }
    
    if (as->data_size > 0) {
        s32o_section_t section = {
            .name_offset = data_name_offset,
            .type = S32_SEC_DATA,
            .flags = S32_SEC_FLAG_READ | S32_SEC_FLAG_WRITE | S32_SEC_FLAG_ALLOC,
            .size = as->data_size,
            .offset = data_offset,
            .align = 4,
            .nrelocs = data_relocs,
            .reloc_offset = data_reloc_offset
        };
        fwrite(&section, sizeof(section), 1, f);
    }
    
    if (as->bss_size > 0) {
        s32o_section_t section = {
            .name_offset = bss_name_offset,
            .type = S32_SEC_BSS,
            .flags = S32_SEC_FLAG_READ | S32_SEC_FLAG_WRITE | S32_SEC_FLAG_ALLOC,
            .size = as->bss_size,
            .offset = 0,  // BSS has no file data
            .align = 4,
            .nrelocs = bss_relocs,
            .reloc_offset = bss_reloc_offset
        };
        fwrite(&section, sizeof(section), 1, f);
    }
    
    // Write symbol table
    fwrite(symbols, sizeof(s32o_symbol_t), num_symbols, f);
    
    // Write relocations for each section
    for (int sec = 0; sec < 4; sec++) {
        section_t sec_type = (section_t)sec;
        for (int i = 0; i < as->num_relocations; i++) {
            if (as->relocations[i].section != sec_type) continue;
            
            s32o_reloc_t reloc;
            reloc.offset = as->relocations[i].offset;
            reloc.type = as->relocations[i].type;
            reloc.addend = as->relocations[i].addend;
            
            // Find symbol index
            reloc.symbol = 0;
            for (int j = 0; j < num_symbols; j++) {
                const char *sym_name = &as->string_table[symbols[j].name_offset];
                if (strcmp(sym_name, as->relocations[i].symbol) == 0) {
                    reloc.symbol = j;
                    break;
                }
            }
            
            fwrite(&reloc, sizeof(reloc), 1, f);
        }
    }
    
    // Write string table
    fwrite(as->string_table, 1, as->string_table_size, f);
    
    // Align to 4 bytes
    while (ftell(f) % 4) {
        uint8_t zero = 0;
        fwrite(&zero, 1, 1, f);
    }
    
    // Write section data - must write each section at its designated offset
    // Write .text section
    if (as->code_size > 0) {
        fseek(f, code_offset, SEEK_SET);
        for (int i = 0; i < as->num_instructions; i++) {
            instruction_t *inst = &as->instructions[i];
            if (inst->section != SECTION_CODE) continue;
            
            if (inst->is_data_byte) {
                uint8_t byte = inst->instruction & 0xFF;
                fwrite(&byte, 1, 1, f);
            } else {
                fwrite(&inst->instruction, 4, 1, f);
            }
        }
    }
    
    // Write .rodata section
    if (as->rodata_size > 0) {
        fseek(f, rodata_offset, SEEK_SET);
        for (int i = 0; i < as->num_instructions; i++) {
            instruction_t *inst = &as->instructions[i];
            if (inst->section != SECTION_RODATA) continue;
            
            if (inst->is_data_byte) {
                uint8_t byte = inst->instruction & 0xFF;
                fwrite(&byte, 1, 1, f);
            } else {
                fwrite(&inst->instruction, 4, 1, f);
            }
        }
    }
    
    // Write .data section
    if (as->data_size > 0) {
        fseek(f, data_offset, SEEK_SET);
        for (int i = 0; i < as->num_instructions; i++) {
            instruction_t *inst = &as->instructions[i];
            if (inst->section != SECTION_DATA) continue;
            
            if (inst->is_data_byte) {
                uint8_t byte = inst->instruction & 0xFF;
                fwrite(&byte, 1, 1, f);
            } else {
                fwrite(&inst->instruction, 4, 1, f);
            }
        }
    }
    // BSS has no data to write
    
    fclose(f);
    
    printf("Generated object file: %s\n", filename);
    printf("  Sections: %d\n", num_sections);
    printf("  Symbols: %d (global only)\n", num_symbols);
    
    return 0;
}

int main(int argc, char *argv[]) {
    int opt;
    const char *output_file = NULL;
    
    // Parse command-line options
    while ((opt = getopt(argc, argv, "ho:")) != -1) {
        switch (opt) {
            case 'h':
                printf("Usage: %s [options] <input.s> [output.o]\n", argv[0]);
                printf("  Assembles SLOW-32 source to object file (.s32o)\n");
                printf("  Use s32-ld to link object files into executables\n");
                printf("Options:\n");
                printf("  -o <file>   Specify output file (alternative to positional arg)\n");
                printf("  -h          Show this help\n");
                return 0;
            case 'o':
                output_file = optarg;
                break;
            default:
                fprintf(stderr, "Usage: %s [options] <input.s> [output.o]\n", argv[0]);
                fprintf(stderr, "Try '%s -h' for help\n", argv[0]);
                return 1;
        }
    }
    
    // Check for required input file
    if (optind >= argc) {
        fprintf(stderr, "Error: No input file specified\n");
        fprintf(stderr, "Usage: %s [options] <input.s> [output.o]\n", argv[0]);
        return 1;
    }
    
    const char *input_file = argv[optind];
    
    // Determine output file: -o flag takes precedence, then positional arg
    if (output_file == NULL && optind + 1 < argc) {
        output_file = argv[optind + 1];
    }
    
    if (output_file == NULL) {
        fprintf(stderr, "Error: No output file specified\n");
        fprintf(stderr, "Usage: %s [options] <input.s> [output.o]\n", argv[0]);
        return 1;
    }
    
    FILE *input = fopen(input_file, "r");
    if (!input) {
        fprintf(stderr, "Cannot open input file: %s\n", input_file);
        return 1;
    }
    
    assembler_t as = {0};
    as.instructions = calloc(MAX_INSTRUCTIONS, sizeof(instruction_t));
    if (!as.instructions) {
        fprintf(stderr, "Memory allocation failed\n");
        return 1;
    }
    as.current_section = SECTION_CODE;  // Start in code section
    // Object files use section-relative addresses
    as.data_start_addr = 0;
    as.generate_object = true;  // Always generate object files
    char line[MAX_LINE];
    int line_num = 0;
    
    while (fgets(line, MAX_LINE, input)) {
        line_num++;
        if (!assemble_line(&as, line)) {
            fprintf(stderr, "Error at line %d: %s", line_num, line);
            fclose(input);
            return 1;
        }
    }
    fclose(input);
    
    // Process all symbol references (%hi and %lo)
    for (int i = 0; i < as.num_instructions; i++) {
        instruction_t *inst = &as.instructions[i];
        if (inst->has_symbol_ref) {
            // Add relocation for this symbol reference
            uint32_t reloc_type = inst->symbol_is_hi ? S32O_REL_HI20 : S32O_REL_LO12;
            add_relocation(&as, inst->address, inst->symbol_ref, 
                          reloc_type, 0, inst->section);
            inst->needs_relocation = true;
        }
    }
    
    // Process all label references
    for (int i = 0; i < as.num_instructions; i++) {
        instruction_t *inst = &as.instructions[i];
        if (!inst->has_label_ref) continue;
        
        int label_idx = find_label_index(&as, inst->label_ref);
        
        if (label_idx >= 0) {
            // Local label - resolve it (PC-relative within section is stable)
            uint32_t label_addr = as.labels[label_idx].address;
            
            instruction_def_t *inst_def = NULL;
            for (int j = 0; instruction_table[j].mnemonic; j++) {
                if (instruction_table[j].opcode == inst->opcode) {
                    inst_def = &instruction_table[j];
                    break;
                }
            }
            
            if (!inst_def) continue;
            
            int32_t offset;
            switch (inst_def->format) {
                case FMT_B:
                    // Branches are PC+4 relative
                    offset = label_addr - (inst->address + 4);
                    inst->instruction = (inst->instruction & 0x01FFF07F) | 
                        (encode_b(0, 0, 0, offset) & 0xFE000F80);
                    break;
                case FMT_J:
                    // JAL is PC relative (not PC+4)
                    offset = label_addr - inst->address;
                    inst->instruction = (inst->instruction & 0x00000FFF) | 
                        (encode_j(0, 0, offset) & 0xFFFFF000);
                    break;
                default:
                    break;
            }
        } else {
            // External symbol - need relocation
            instruction_def_t *inst_def = NULL;
            for (int j = 0; instruction_table[j].mnemonic; j++) {
                if (instruction_table[j].opcode == inst->opcode) {
                    inst_def = &instruction_table[j];
                    break;
                }
            }
            
            if (inst_def) {
                uint32_t reloc_type = S32O_REL_NONE;
                switch (inst_def->format) {
                    case FMT_B: reloc_type = S32O_REL_BRANCH; break;
                    case FMT_J: reloc_type = S32O_REL_JAL; break;
                    // TODO: Handle U+I pairs for absolute addresses
                    default: break;
                }
                
                if (reloc_type != S32O_REL_NONE) {
                    add_relocation(&as, inst->address, inst->label_ref, 
                                 reloc_type, 0, inst->section);
                }
            }
        }
    }
    
    int result = write_object_file(&as, output_file);
    free(as.instructions);
    return result;
}
