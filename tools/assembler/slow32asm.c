#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <ctype.h>
#include <stdbool.h>
#include <getopt.h>
#include "../../common/s32_formats.h"

#define MAX_LINE 65536
#define MAX_TOKENS 8
#define MAX_TOKEN_LEN 16384
#define MAX_LABELS 32768
#define INITIAL_INSTRUCTION_CAPACITY 65536
#define MAX_DATA_SIZE (1024 * 1024)  // 1MB max data section
#define MAX_RELOCATIONS 16384
#define MAX_STRING_TABLE (256 * 1024) // 256KB string table
#define MAX_LABEL_DIFFS 256

// SLOW-32 assembler now only outputs object files (.s32o)
// Binary executables are created by the linker (.s32x)

typedef enum {
    SECTION_CODE,
    SECTION_DATA,
    SECTION_BSS,
    SECTION_RODATA,
    SECTION_INIT_ARRAY
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

typedef enum {
    TOK_END, TOK_NUMBER, TOK_IDENTIFIER, TOK_PLUS, TOK_MINUS, TOK_MUL, TOK_DIV,
    TOK_LPAREN, TOK_RPAREN, TOK_COMMA, TOK_PERCENT, TOK_STRING, TOK_BRACKET_L, TOK_BRACKET_R
} token_type_t;

typedef struct {
    token_type_t type;
    int32_t val;
    char sval[256];
} token_t;

typedef struct {
    const char *p;
    token_t curr;
} scanner_t;

typedef struct {
    int32_t val;
    char symbol[64];
    bool has_symbol;
    char minus_symbol[64];   // For label1 - label2 expressions
    bool has_minus_symbol;
    bool is_hi;
    bool is_lo;
    bool is_pcrel_hi;
    bool is_pcrel_lo;
} expr_result_t;

static void scanner_next(scanner_t *s) {
    while (*s->p && isspace(*s->p)) s->p++;
    if (!*s->p) {
        s->curr.type = TOK_END;
        return;
    }
    
    char c = *s->p++;
    switch (c) {
        case '+': s->curr.type = TOK_PLUS; return;
        case '-': s->curr.type = TOK_MINUS; return;
        case '*': s->curr.type = TOK_MUL; return;
        case '/': s->curr.type = TOK_DIV; return;
        case '(': s->curr.type = TOK_LPAREN; return;
        case ')': s->curr.type = TOK_RPAREN; return;
        case '[': s->curr.type = TOK_BRACKET_L; return;
        case ']': s->curr.type = TOK_BRACKET_R; return;
        case ',': s->curr.type = TOK_COMMA; return;
        case '%': s->curr.type = TOK_PERCENT; return;
        case '"':
            s->curr.type = TOK_STRING;
            int i = 0;
            s->curr.sval[i++] = '"';
            while (*s->p && *s->p != '"') {
                if (*s->p == '\\' && *(s->p+1)) {
                    s->curr.sval[i++] = *s->p++;
                }
                if (i < 254) s->curr.sval[i++] = *s->p++;
                else s->p++;
            }
            if (*s->p == '"') s->curr.sval[i++] = *s->p++;
            s->curr.sval[i] = '\0';
            return;
        default:
            if (isdigit(c)) {
                s->curr.type = TOK_NUMBER;
                s->p--;
                char *end;
                s->curr.val = (int32_t)strtol(s->p, &end, 0);
                s->p = end;
                return;
            }
            if (isalpha(c) || c == '_' || c == '.') {
                s->curr.type = TOK_IDENTIFIER;
                int i = 0;
                s->curr.sval[i++] = c;
                while (isalnum(*s->p) || *s->p == '_' || *s->p == '.') {
                    if (i < 63) s->curr.sval[i++] = *s->p;
                    s->p++;
                }
                s->curr.sval[i] = '\0';
                return;
            }
            s->curr.type = TOK_END;
    }
}

static bool parse_expression(scanner_t *s, expr_result_t *res);

static bool parse_factor(scanner_t *s, expr_result_t *res) {
    if (s->curr.type == TOK_NUMBER) {
        res->val = s->curr.val;
        scanner_next(s);
    } else if (s->curr.type == TOK_IDENTIFIER) {
        strncpy(res->symbol, s->curr.sval, 63);
        res->symbol[63] = '\0';
        res->has_symbol = true;
        scanner_next(s);
    } else if (s->curr.type == TOK_LPAREN) {
        scanner_next(s);
        if (!parse_expression(s, res)) return false;
        if (s->curr.type != TOK_RPAREN) return false;
        scanner_next(s);
    } else if (s->curr.type == TOK_PERCENT) {
        scanner_next(s);
        if (s->curr.type != TOK_IDENTIFIER) return false;
        if (strcmp(s->curr.sval, "hi") == 0) res->is_hi = true;
        else if (strcmp(s->curr.sval, "lo") == 0) res->is_lo = true;
        else if (strcmp(s->curr.sval, "pcrel_hi") == 0) res->is_pcrel_hi = true;
        else if (strcmp(s->curr.sval, "pcrel_lo") == 0) res->is_pcrel_lo = true;
        else return false;
        scanner_next(s);
        if (s->curr.type != TOK_LPAREN) return false;
        scanner_next(s);
        if (!parse_expression(s, res)) return false;
        if (s->curr.type != TOK_RPAREN) return false;
        scanner_next(s);
    } else if (s->curr.type == TOK_MINUS) {
        scanner_next(s);
        if (!parse_factor(s, res)) return false;
        res->val = -res->val;
    } else if (s->curr.type == TOK_PLUS) {
        scanner_next(s);
        if (!parse_factor(s, res)) return false;
    } else {
        return false;
    }
    return true;
}

static bool parse_term(scanner_t *s, expr_result_t *res) {
    if (!parse_factor(s, res)) return false;
    while (s->curr.type == TOK_MUL || s->curr.type == TOK_DIV) {
        token_type_t op = s->curr.type;
        scanner_next(s);
        expr_result_t rhs = {0};
        if (!parse_factor(s, &rhs)) return false;
        if (op == TOK_MUL) res->val *= rhs.val;
        else if (rhs.val != 0) res->val /= rhs.val;
        if (rhs.has_symbol) return false;
    }
    return true;
}

static bool parse_expression(scanner_t *s, expr_result_t *res) {
    if (!parse_term(s, res)) return false;
    while (s->curr.type == TOK_PLUS || s->curr.type == TOK_MINUS) {
        token_type_t op = s->curr.type;
        scanner_next(s);
        expr_result_t rhs = {0};
        if (!parse_term(s, &rhs)) return false;
        if (op == TOK_PLUS) res->val += rhs.val;
        else res->val -= rhs.val;
        if (rhs.has_symbol) {
            if (res->has_symbol && op == TOK_PLUS) return false;
            if (res->has_symbol && op == TOK_MINUS) {
                // label1 - label2: store as label difference
                strcpy(res->minus_symbol, rhs.symbol);
                res->has_minus_symbol = true;
            } else {
                strcpy(res->symbol, rhs.symbol);
                res->has_symbol = true;
            }
        }
    }
    return true;
}

static bool parse_expression_all(const char *str, expr_result_t *res) {
    scanner_t s = { .p = str };
    memset(res, 0, sizeof(*res));
    scanner_next(&s);
    if (s.curr.type == TOK_END) return false;
    if (!parse_expression(&s, res)) return false;
    return true;
}

static int parse_immediate(const char *str) {
    expr_result_t res;
    if (!parse_expression_all(str, &res)) return 0;
    return res.val;
}

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

static int parse_register_scanner(scanner_t *s) {
    if (s->curr.type == TOK_IDENTIFIER) {
        int reg = parse_register(s->curr.sval);
        if (reg >= 0) {
            scanner_next(s);
            return reg;
        }
    } else if (s->curr.type == TOK_NUMBER && s->curr.val == 0) {
        scanner_next(s);
        return 0; // r0
    }
    return -1;
}

static bool parse_operand_scanner(scanner_t *s, expr_result_t *res) {
    memset(res, 0, sizeof(*res));
    if (s->curr.type == TOK_END) return false;
    if (!parse_expression(s, res)) return false;
    return true;
}

typedef struct {
    uint32_t opcode;
    uint32_t instruction;
    uint32_t address;
    bool has_label_ref;
    char label_ref[64];
    int label_offset;
    bool has_symbol_ref;    // True if this references a global symbol
    char symbol_ref[64];    // Symbol name for %hi()/%lo()/%pcrel_hi()/%pcrel_lo()
    bool symbol_is_hi;      // True if this is %hi(symbol)
    bool symbol_is_lo;      // True if this is %lo(symbol)
    bool symbol_is_pcrel_hi;// True if this is %pcrel_hi(symbol)
    bool symbol_is_pcrel_lo;// True if this is %pcrel_lo(label)
    int32_t symbol_addend;  // Addend for symbol+offset forms
    section_t section;      // Which section this belongs to
    bool is_data_byte;      // True if this is a .db directive byte
    bool needs_relocation;  // True if this instruction references an undefined symbol
} instruction_t;

typedef struct {
    label_t labels[MAX_LABELS];
    int num_labels;
    instruction_t *instructions;  // Dynamically allocated
    int instructions_capacity;    // Current capacity
    int num_instructions;
    relocation_t relocations[MAX_RELOCATIONS];
    int num_relocations;
    uint32_t current_addr;
    section_t current_section;  // Track current section during assembly
    uint32_t code_size;         // Size of code section
    uint32_t data_size;         // Size of data section
    uint32_t bss_size;          // Size of bss section
    uint32_t rodata_size;       // Size of rodata section
    uint32_t init_array_size;   // Size of init_array section
    uint32_t data_start_addr;   // Starting address of data section (0x100000)
    bool generate_object;       // True for object file, false for executable
    
    // Label-difference fixups (resolved after all labels are defined)
    struct {
        int instruction_index;       // which instruction slot to patch
        char plus_symbol[64];        // positive label
        char minus_symbol[64];       // negative label
        int32_t addend;              // constant addend
    } label_diffs[MAX_LABEL_DIFFS];
    int num_label_diffs;

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
    {"mulhu", 0x1F, FMT_R},
    {"div",   0x0C, FMT_R},
    {"rem",   0x0D, FMT_R},
    {"seq",   0x0E, FMT_R},
    {"sne",   0x0F, FMT_R},
    
    {"addi",  0x10, FMT_I},
    {"ori",   0x11, FMT_I},
    {"andi",  0x12, FMT_I},
    {"xori",  0x1E, FMT_I},
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

    // f32 (single-precision float) instructions
    {"fadd.s",    0x53, FMT_R},
    {"fsub.s",    0x54, FMT_R},
    {"fmul.s",    0x55, FMT_R},
    {"fdiv.s",    0x56, FMT_R},
    {"fsqrt.s",   0x57, FMT_R},
    {"feq.s",     0x58, FMT_R},
    {"flt.s",     0x59, FMT_R},
    {"fle.s",     0x5A, FMT_R},
    {"fcvt.w.s",  0x5B, FMT_R},
    {"fcvt.wu.s", 0x5C, FMT_R},
    {"fcvt.s.w",  0x5D, FMT_R},
    {"fcvt.s.wu", 0x5E, FMT_R},
    {"fneg.s",    0x5F, FMT_R},
    {"fabs.s",    0x60, FMT_R},

    // f64 (double-precision float) instructions
    {"fadd.d",    0x61, FMT_R},
    {"fsub.d",    0x62, FMT_R},
    {"fmul.d",    0x63, FMT_R},
    {"fdiv.d",    0x64, FMT_R},
    {"fsqrt.d",   0x65, FMT_R},
    {"feq.d",     0x66, FMT_R},
    {"flt.d",     0x67, FMT_R},
    {"fle.d",     0x68, FMT_R},
    {"fcvt.w.d",  0x69, FMT_R},
    {"fcvt.wu.d", 0x6A, FMT_R},
    {"fcvt.d.w",  0x6B, FMT_R},
    {"fcvt.d.wu", 0x6C, FMT_R},
    {"fcvt.d.s",  0x6D, FMT_R},
    {"fcvt.s.d",  0x6E, FMT_R},
    {"fneg.d",    0x6F, FMT_R},
    {"fabs.d",    0x70, FMT_R},

    // float <-> int64 conversions
    {"fcvt.l.s",  0x71, FMT_R},
    {"fcvt.lu.s", 0x72, FMT_R},
    {"fcvt.s.l",  0x73, FMT_R},
    {"fcvt.s.lu", 0x74, FMT_R},
    {"fcvt.l.d",  0x75, FMT_R},
    {"fcvt.lu.d", 0x76, FMT_R},
    {"fcvt.d.l",  0x77, FMT_R},
    {"fcvt.d.lu", 0x78, FMT_R},

    {"halt",  0x7F, FMT_NONE},
    {"assert_eq", 0x3F, FMT_R},  // Testing instruction

    {NULL, 0, FMT_NONE}
};

// Helper to update the size of the current section
static void bump_size(assembler_t* as, section_t sec, uint32_t n) {
    switch (sec) {
        case SECTION_CODE:       as->code_size       += n; break;
        case SECTION_DATA:       as->data_size       += n; break;
        case SECTION_RODATA:     as->rodata_size     += n; break;
        case SECTION_BSS:        as->bss_size        += n; break;
        case SECTION_INIT_ARRAY: as->init_array_size += n; break;
    }
}

// Ensure there is room for at least `additional` more instructions/data bytes
static void ensure_instruction_capacity(assembler_t *as, int additional) {
    if (additional < 0) additional = 0;
    int needed = as->num_instructions + additional;
    if (needed <= as->instructions_capacity) return;

    int new_cap = as->instructions_capacity > 0 ? as->instructions_capacity : INITIAL_INSTRUCTION_CAPACITY;
    while (new_cap < needed) {
        // Grow geometrically to amortize realloc cost
        new_cap *= 2;
        if (new_cap <= 0) {
            fprintf(stderr, "Instruction buffer size overflow\n");
            exit(1);
        }
    }

    instruction_t *new_buf = realloc(as->instructions, new_cap * sizeof(instruction_t));
    if (!new_buf) {
        fprintf(stderr, "Memory allocation failed while expanding instruction buffer\n");
        exit(1);
    }

    as->instructions = new_buf;
    as->instructions_capacity = new_cap;
}

static char *trim(char *str) {
    while (isspace(*str)) str++;
    char *end = str + strlen(str) - 1;
    while (end > str && isspace(*end)) *end-- = '\0';
    return str;
}

static int tokenize(char *line, char tokens[][MAX_TOKEN_LEN]) {
    int count = 0;
    char *p = line;
    
    while (*p && count < MAX_TOKENS) {
        // Skip whitespace and commas
        while (*p && (*p == ' ' || *p == '\t' || *p == ',')) p++;
        if (!*p) break;
        
        char *token_start = tokens[count];
        char *token_ptr = token_start;
        int paren_depth = 0;
        bool in_quotes = false;
        
        // Handle brackets as standalone tokens (maintained from original)
        if ((*p == '[' || *p == ']') && paren_depth == 0 && !in_quotes) {
            *token_ptr++ = *p++;
            *token_ptr = '\0';
            count++;
            continue;
        }

        while (*p) {
            if (*p == '\\' && *(p+1)) {
                if (token_ptr - token_start < MAX_TOKEN_LEN - 2) {
                    *token_ptr++ = *p++;
                    *token_ptr++ = *p++;
                } else {
                    p += 2;
                }
                continue;
            }
            
            if (*p == '"') {
                in_quotes = !in_quotes;
            } else if (!in_quotes) {
                if (*p == '(') paren_depth++;
                else if (*p == ')') paren_depth--;
                else if (paren_depth == 0) {
                    if (*p == ' ' || *p == '\t' || *p == ',' || *p == '[' || *p == ']') {
                        break; // End of token
                    }
                }
            }
            
            if (token_ptr - token_start < MAX_TOKEN_LEN - 1) {
                *token_ptr++ = *p++;
            } else {
                p++;
            }
        }
        *token_ptr = '\0';
        if (token_ptr > token_start) {
            count++;
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
    rel->symbol[sizeof(rel->symbol) - 1] = '\0';
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
            if (*p >= '0' && *p <= '7') {
                int val = 0;
                int digits = 0;
                while (digits < 3 && *p >= '0' && *p <= '7') {
                    val = (val * 8) + (*p - '0');
                    p++;
                    digits++;
                }
                byte = (uint8_t)val;
            } else if (*p == 'x') {
                // Hex escape \xHH
                if (*(p+1) && *(p+2)) {
                    char hex[3] = {*(p+1), *(p+2), '\0'};
                    byte = (uint8_t)strtol(hex, NULL, 16);
                    p += 3;
                } else {
                    return -1; // Invalid hex escape
                }
            } else {
                switch (*p) {
                    case 'a': byte = '\a'; break;  // alert/bell
                    case 'b': byte = '\b'; break;  // backspace
                    case 'f': byte = '\f'; break;  // form feed
                    case 'n': byte = '\n'; break;  // newline
                    case 'r': byte = '\r'; break;  // carriage return
                    case 't': byte = '\t'; break;  // tab
                    case 'v': byte = '\v'; break;  // vertical tab
                    case '\\': byte = '\\'; break;
                    case '"': byte = '"'; break;
                    default:
                        byte = *p; // Unknown escape, use literal
                        break;
                }
                p++;
            }
        } else {
            byte = *p++;
        }
        
        // Emit the byte
        ensure_instruction_capacity(as, 1);
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
    bool in_quotes = false;
    char *colon = NULL;
    char *comment = NULL;

    // Single pass to find comment start and label separator while respecting strings
    for (char *p = line; *p; p++) {
        if (*p == '\\' && *(p+1)) {
            p++; // Skip escaped character
            continue;
        }
        if (*p == '"') {
            in_quotes = !in_quotes;
        } else if (!in_quotes) {
            if (*p == '#' && !comment) {
                comment = p;
            } else if (*p == ':' && !colon && !comment) {
                // First colon outside quotes and before any comment is the label separator
                colon = p;
            }
        }
    }

    if (comment) *comment = '\0';
    
    if (colon) {
        *colon = '\0';
        char *label_name = trim(line);
        
        // Check if label already exists (e.g., from .global)
        label_t *existing = find_label(as, label_name);
        if (existing) {
            existing->address = as->current_addr;
            existing->section = as->current_section;
            existing->is_defined = true;
        } else {
            add_label(as, label_name, as->current_addr);
        }
        
        line = colon + 1;
    }
    
    line = trim(line);
    if (strlen(line) == 0) return true;
    
    char tokens[MAX_TOKENS][MAX_TOKEN_LEN];
    int num_tokens = tokenize(line, tokens);
    if (num_tokens == 0) return true;
    
    // Lowercase only mnemonic/directive (tokens[0]); keep operand/symbol case
    for (char *pp = tokens[0]; *pp; ++pp) *pp = (char)tolower((unsigned char)*pp);
    
    // Expand convenient branch aliases that the LLVM backend emits but the
    // hardware still implements via the original BLT/BGE encodings.
    bool swap_branch_regs = false;
    if (!strcmp(tokens[0], "bgt") || !strcmp(tokens[0], "ble") ||
        !strcmp(tokens[0], "bgtu") || !strcmp(tokens[0], "bleu")) {
        if (num_tokens < 4) {
            fprintf(stderr, "Assembler error: %s requires two registers and a target\n",
                    tokens[0]);
            return false;
        }

        // Swap the register operands so we can reuse the existing encoding.
        char tmp[MAX_TOKEN_LEN];
        strcpy(tmp, tokens[1]);
        strcpy(tokens[1], tokens[2]);
        strcpy(tokens[2], tmp);
        swap_branch_regs = true;

        if (!strcmp(tokens[0], "bgt")) {
            strcpy(tokens[0], "blt");
        } else if (!strcmp(tokens[0], "ble")) {
            strcpy(tokens[0], "bge");
        } else if (!strcmp(tokens[0], "bgtu")) {
            strcpy(tokens[0], "bltu");
        } else {
            strcpy(tokens[0], "bgeu");
        }
    }

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
            // Example: .section .init_array,"aw",@init_array
            if (num_tokens > 1) {
                // Check if it's a rodata variant
                if (strncmp(tokens[1], ".rodata", 7) == 0) {
                    as->current_section = SECTION_RODATA;
                    as->current_addr = as->rodata_size;
                    return true;
                }
                // Check if it's .init_array (C++ global constructors)
                if (strncmp(tokens[1], ".init_array", 11) == 0) {
                    as->current_section = SECTION_INIT_ARRAY;
                    as->current_addr = as->init_array_size;
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
        } else if ((strcmp(tokens[0], ".word") == 0 || strcmp(tokens[0], ".long") == 0) && num_tokens > 1) {
            // .word/.long - emit 32-bit words (numbers, symbols → REL_32, or label diffs)
            for (int i = 1; i < num_tokens; i++) {
                expr_result_t res = {0};
                if (!parse_expression_all(tokens[i], &res)) {
                    fprintf(stderr, ".word: failed to parse expression '%s'\n", tokens[i]);
                    return false;
                }
                if (res.has_symbol && res.has_minus_symbol) {
                    // Label difference: sym - minus_sym + val
                    // Store placeholder, resolve after all labels defined
                    if (as->num_label_diffs >= MAX_LABEL_DIFFS) {
                        fprintf(stderr, "Too many label-difference fixups\n");
                        return false;
                    }
                    ensure_instruction_capacity(as, 1);
                    int idx = as->num_instructions;
                    as->instructions[idx].instruction = 0;  // placeholder
                    as->instructions[idx].address = as->current_addr;
                    as->instructions[idx].section = as->current_section;
                    as->instructions[idx].is_data_byte = false;
                    as->num_instructions++;
                    strcpy(as->label_diffs[as->num_label_diffs].plus_symbol, res.symbol);
                    strcpy(as->label_diffs[as->num_label_diffs].minus_symbol, res.minus_symbol);
                    as->label_diffs[as->num_label_diffs].addend = res.val;
                    as->label_diffs[as->num_label_diffs].instruction_index = idx;
                    as->num_label_diffs++;
                } else if (res.has_symbol) {
                    if (res.is_hi || res.is_lo) {
                        fprintf(stderr, ".word %%hi/%%lo not supported here; use plain symbol or split words\n");
                        return false;
                    }
                    // placeholder + relocation
                    ensure_instruction_capacity(as, 1);
                    as->instructions[as->num_instructions].instruction = 0;
                    as->instructions[as->num_instructions].address = as->current_addr;
                    as->instructions[as->num_instructions].section = as->current_section;
                    as->instructions[as->num_instructions].is_data_byte = false;
                    as->num_instructions++;
                    add_relocation(as, as->current_addr, res.symbol, S32O_REL_32, res.val, as->current_section);
                    if (getenv("DEBUG_RELOC")) {
                        fprintf(stderr, "Added REL_32 relocation for symbol '%s' at offset 0x%x in section %d\n",
                                res.symbol, as->current_addr, as->current_section);
                    }
                } else {
                    ensure_instruction_capacity(as, 1);
                    as->instructions[as->num_instructions].instruction = (uint32_t)res.val;
                    as->instructions[as->num_instructions].address = as->current_addr;
                    as->instructions[as->num_instructions].section = as->current_section;
                    as->instructions[as->num_instructions].is_data_byte = false;
                    as->num_instructions++;
                }
                as->current_addr += 4;
                bump_size(as, as->current_section, 4);
            }
            return true;
        } else if ((strcmp(tokens[0], ".half") == 0 || strcmp(tokens[0], ".short") == 0) && num_tokens > 1) {
            // .half - emit 16-bit halfwords
            for (int i = 1; i < num_tokens; i++) {
                uint16_t value = parse_immediate(tokens[i]) & 0xFFFF;
                // Store as two bytes in little-endian order
                ensure_instruction_capacity(as, 2);
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
            ensure_instruction_capacity(as, padding);
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
                    ensure_instruction_capacity(as, 1);
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
                    ensure_instruction_capacity(as, 1);
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
        } else if ((strcmp(tokens[0], ".zero") == 0 || strcmp(tokens[0], ".space") == 0) && num_tokens > 1) {
            // .zero/.space N - reserve N bytes of zeros
            int num_bytes = parse_immediate(tokens[1]);
            if (num_bytes < 0) {
                fprintf(stderr, ".zero/.space requires positive byte count\n");
                return false;
            }
            ensure_instruction_capacity(as, num_bytes);
            for (int i = 0; i < num_bytes; i++) {
                as->instructions[as->num_instructions].instruction = 0x80000000;
                as->instructions[as->num_instructions].address = as->current_addr;
                as->instructions[as->num_instructions].section = as->current_section;
                as->instructions[as->num_instructions].is_data_byte = true;
                as->num_instructions++;
                as->current_addr++;
                bump_size(as, as->current_section, 1);
            }
            return true;
        } else if (strcmp(tokens[0], ".quad") == 0 && num_tokens > 1) {
            // .quad - emit 64-bit values (as two 32-bit words, little-endian)
            for (int i = 1; i < num_tokens; i++) {
                int64_t value = 0;
                if (tokens[i][0] == '0' && tokens[i][1] == 'x') {
                    value = strtoull(tokens[i] + 2, NULL, 16);
                } else if (tokens[i][0] == '-') {
                    value = strtoll(tokens[i], NULL, 10);
                } else {
                    value = strtoull(tokens[i], NULL, 10);
                }
                
                // Low 32 bits
                ensure_instruction_capacity(as, 2);
                as->instructions[as->num_instructions].instruction = (uint32_t)(value & 0xFFFFFFFF);
                as->instructions[as->num_instructions].address = as->current_addr;
                as->instructions[as->num_instructions].section = as->current_section;
                as->instructions[as->num_instructions].is_data_byte = false;
                as->num_instructions++;
                as->current_addr += 4;
                bump_size(as, as->current_section, 4);
                
                // High 32 bits
                as->instructions[as->num_instructions].instruction = (uint32_t)((value >> 32) & 0xFFFFFFFF);
                as->instructions[as->num_instructions].address = as->current_addr;
                as->instructions[as->num_instructions].section = as->current_section;
                as->instructions[as->num_instructions].is_data_byte = false;
                as->num_instructions++;
                as->current_addr += 4;
                bump_size(as, as->current_section, 4);
            }
            return true;
        } else if ((strcmp(tokens[0], ".equ") == 0 || strcmp(tokens[0], ".set") == 0) && num_tokens >= 3) {
            // .equ/.set symbol, value - define a constant
            char *symbol = tokens[1];
            int value = parse_immediate(tokens[2]);
            
            // Add as a label with the constant value
            label_t *label = find_label(as, symbol);
            if (label) {
                // Update existing label
                label->address = value;
                label->is_defined = true;
            } else {
                // Add new label
                add_label(as, symbol, value);
                as->labels[as->num_labels - 1].is_defined = true;
            }
            return true;
        } else if (strcmp(tokens[0], ".comm") == 0 && num_tokens >= 3) {
            // .comm symbol, size[, alignment] - declare common symbol
            char *symbol = tokens[1];
            int size = parse_immediate(tokens[2]);
            
            // Add to BSS section
            section_t saved_section = as->current_section;
            uint32_t saved_addr = as->current_addr;
            
            as->current_section = SECTION_BSS;
            as->current_addr = as->bss_size;
            
            // Add alignment if specified
            if (num_tokens > 3) {
                int align = parse_immediate(tokens[3]);
                int align_mask = align - 1;
                if (as->current_addr & align_mask) {
                    int padding = align - (as->current_addr & align_mask);
                    as->current_addr += padding;
                    as->bss_size += padding;
                }
            }
            
            // Add label for the symbol
            add_label(as, symbol, as->current_addr);
            label_t *label = &as->labels[as->num_labels - 1];
            label->section = SECTION_BSS;
            label->is_global = true;
            label->is_defined = true;
            
            // Reserve space
            as->bss_size += size;
            
            // Restore section
            as->current_section = saved_section;
            as->current_addr = saved_addr;
            return true;
        }
        // Ignore other directives
        return true;
    }
    
    // Handle pseudo-instructions before real instructions
    if (strcmp(tokens[0], "li") == 0) {
        scanner_t s_op = { .p = line };
        scanner_next(&s_op); // skip 'li' (mnemonic)
        scanner_next(&s_op); // load first operand
        int rd = parse_register_scanner(&s_op);
        if (s_op.curr.type == TOK_COMMA) scanner_next(&s_op);
        
        expr_result_t res;
        if (!parse_operand_scanner(&s_op, &res)) return false;
        
        int32_t imm = res.val;
        // If it was a symbol, it's a relocation (treated as large imm for now)
        bool is_symbol = res.has_symbol;
        
        if (!is_symbol && imm >= -2048 && imm < 2048) {
            // Small immediate - use ADDI
            ensure_instruction_capacity(as, 1);
            instruction_t *inst = &as->instructions[as->num_instructions];
            inst->opcode = 0x10;  // ADDI
            inst->address = as->current_addr;
            inst->section = as->current_section;
            inst->instruction = encode_i(0x10, rd, 0, imm);
            as->num_instructions++;
            as->current_addr += 4;
            bump_size(as, as->current_section, 4);
        } else {
            // Large immediate or symbol - use LUI + ORI
            uint32_t upper = ((uint32_t)imm >> 12) & 0xFFFFF;
            uint32_t lower = imm & 0xFFF;
            
            // LUI rd, upper
            ensure_instruction_capacity(as, 2);
            instruction_t *inst1 = &as->instructions[as->num_instructions];
            inst1->opcode = 0x20;  // LUI
            inst1->address = as->current_addr;
            inst1->section = as->current_section;
            if (is_symbol) {
                inst1->has_symbol_ref = true;
                inst1->symbol_is_hi = true;
                strcpy(inst1->symbol_ref, res.symbol);
                inst1->symbol_addend = res.val;
            }
            inst1->instruction = encode_u(0x20, rd, upper);
            as->num_instructions++;
            as->current_addr += 4;
            bump_size(as, as->current_section, 4);
            
            // ORI rd, rd, lower
            instruction_t *inst2 = &as->instructions[as->num_instructions];
            inst2->opcode = 0x11;  // ORI
            inst2->address = as->current_addr;
            inst2->section = as->current_section;
            if (is_symbol) {
                inst2->has_symbol_ref = true;
                inst2->symbol_is_lo = true;
                strcpy(inst2->symbol_ref, res.symbol);
                inst2->symbol_addend = res.val;
            }
            inst2->instruction = encode_i(0x11, rd, rd, lower);
            as->num_instructions++;
            as->current_addr += 4;
            bump_size(as, as->current_section, 4);
        }
        return true;
    } else if (strcmp(tokens[0], "ret") == 0) {
        // ret - return (jr r31)
        ensure_instruction_capacity(as, 1);
        instruction_t *inst = &as->instructions[as->num_instructions];
        inst->opcode = 0x41;  // JALR
        inst->address = as->current_addr;
        inst->section = as->current_section;
        inst->instruction = encode_i(0x41, 0, 31, 0);  // jalr r0, r31, 0
        as->num_instructions++;
        as->current_addr += 4;
        bump_size(as, as->current_section, 4);
        return true;
    } else if (strcmp(tokens[0], "mv") == 0) {
        // mv rd, rs - move register (add rd, rs, r0)
        scanner_t s_op = { .p = line };
        scanner_next(&s_op); // skip 'mv'
        scanner_next(&s_op);
        int rd = parse_register_scanner(&s_op);
        if (s_op.curr.type == TOK_COMMA) scanner_next(&s_op);
        int rs = parse_register_scanner(&s_op);
        
        ensure_instruction_capacity(as, 1);
        instruction_t *inst = &as->instructions[as->num_instructions];
        inst->opcode = 0x00;  // ADD
        inst->address = as->current_addr;
        inst->section = as->current_section;
        inst->instruction = encode_r(0x00, rd, rs, 0);
        as->num_instructions++;
        as->current_addr += 4;
        bump_size(as, as->current_section, 4);
        return true;
    } else if (strcmp(tokens[0], "nop") == 0) {
        // nop - no operation (add r0, r0, r0)
        ensure_instruction_capacity(as, 1);
        instruction_t *inst = &as->instructions[as->num_instructions];
        inst->opcode = 0x00;  // ADD
        inst->address = as->current_addr;
        inst->section = as->current_section;
        inst->instruction = encode_r(0x00, 0, 0, 0);
        as->num_instructions++;
        as->current_addr += 4;
        bump_size(as, as->current_section, 4);
        return true;
    } else if (strcmp(tokens[0], "j") == 0) {
        // j label - unconditional jump (jal r0, label)
        scanner_t s_op = { .p = line };
        scanner_next(&s_op); // skip 'j'
        scanner_next(&s_op);
        
        ensure_instruction_capacity(as, 1);
        instruction_t *inst = &as->instructions[as->num_instructions];
        inst->opcode = 0x40;  // JAL
        inst->address = as->current_addr;
        inst->section = as->current_section;
        
        if (s_op.curr.type == TOK_IDENTIFIER) {
            inst->has_label_ref = true;
            strcpy(inst->label_ref, s_op.curr.sval);
            inst->instruction = encode_j(0x40, 0, 0);
        } else {
            expr_result_t res;
            parse_operand_scanner(&s_op, &res);
            inst->instruction = encode_j(0x40, 0, res.val);
        }
        as->num_instructions++;
        as->current_addr += 4;
        bump_size(as, as->current_section, 4);
        return true;
    } else if (strcmp(tokens[0], "call") == 0) {
        // call label - function call (jal r31, label)
        scanner_t s_op = { .p = line };
        scanner_next(&s_op); // skip 'call'
        scanner_next(&s_op);
        
        ensure_instruction_capacity(as, 1);
        instruction_t *inst = &as->instructions[as->num_instructions];
        inst->opcode = 0x40;  // JAL
        inst->address = as->current_addr;
        inst->section = as->current_section;
        
        if (s_op.curr.type == TOK_IDENTIFIER) {
            inst->has_label_ref = true;
            strcpy(inst->label_ref, s_op.curr.sval);
            inst->instruction = encode_j(0x40, 31, 0);
        } else {
            expr_result_t res;
            parse_operand_scanner(&s_op, &res);
            inst->instruction = encode_j(0x40, 31, res.val);
        }
        as->num_instructions++;
        as->current_addr += 4;
        bump_size(as, as->current_section, 4);
        return true;
    } else if (strcmp(tokens[0], "not") == 0) {
        // not rd, rs - bitwise NOT (xori rd, rs, -1)
        scanner_t s_op = { .p = line };
        scanner_next(&s_op); // skip 'not'
        scanner_next(&s_op);
        int rd = parse_register_scanner(&s_op);
        if (s_op.curr.type == TOK_COMMA) scanner_next(&s_op);
        int rs = parse_register_scanner(&s_op);
        
        ensure_instruction_capacity(as, 1);
        instruction_t *inst = &as->instructions[as->num_instructions];
        inst->opcode = 0x1E;  // XORI
        inst->address = as->current_addr;
        inst->section = as->current_section;
        inst->instruction = encode_i(0x1E, rd, rs, -1);
        as->num_instructions++;
        as->current_addr += 4;
        bump_size(as, as->current_section, 4);
        return true;
    } else if (strcmp(tokens[0], "neg") == 0) {
        // neg rd, rs - negate (sub rd, r0, rs)
        scanner_t s_op = { .p = line };
        scanner_next(&s_op); // skip 'neg'
        scanner_next(&s_op);
        int rd = parse_register_scanner(&s_op);
        if (s_op.curr.type == TOK_COMMA) scanner_next(&s_op);
        int rs = parse_register_scanner(&s_op);
        
        ensure_instruction_capacity(as, 1);
        instruction_t *inst = &as->instructions[as->num_instructions];
        inst->opcode = 0x01;  // SUB
        inst->address = as->current_addr;
        inst->section = as->current_section;
        inst->instruction = encode_r(0x01, rd, 0, rs);
        as->num_instructions++;
        as->current_addr += 4;
        bump_size(as, as->current_section, 4);
        return true;
    } else if (strcmp(tokens[0], "seqz") == 0) {
        // seqz rd, rs - set if equal to zero (seq rd, rs, r0)
        scanner_t s_op = { .p = line };
        scanner_next(&s_op); // skip 'seqz'
        scanner_next(&s_op);
        int rd = parse_register_scanner(&s_op);
        if (s_op.curr.type == TOK_COMMA) scanner_next(&s_op);
        int rs = parse_register_scanner(&s_op);
        
        ensure_instruction_capacity(as, 1);
        instruction_t *inst = &as->instructions[as->num_instructions];
        inst->opcode = 0x0E;  // SEQ
        inst->address = as->current_addr;
        inst->section = as->current_section;
        inst->instruction = encode_r(0x0E, rd, rs, 0);
        as->num_instructions++;
        as->current_addr += 4;
        bump_size(as, as->current_section, 4);
        return true;
    } else if (strcmp(tokens[0], "snez") == 0) {
        // snez rd, rs - set if not equal to zero (sne rd, rs, r0)
        scanner_t s_op = { .p = line };
        scanner_next(&s_op); // skip 'snez'
        scanner_next(&s_op);
        int rd = parse_register_scanner(&s_op);
        if (s_op.curr.type == TOK_COMMA) scanner_next(&s_op);
        int rs = parse_register_scanner(&s_op);
        
        ensure_instruction_capacity(as, 1);
        instruction_t *inst = &as->instructions[as->num_instructions];
        inst->opcode = 0x0F;  // SNE
        inst->address = as->current_addr;
        inst->section = as->current_section;
        inst->instruction = encode_r(0x0F, rd, rs, 0);
        as->num_instructions++;
        as->current_addr += 4;
        bump_size(as, as->current_section, 4);
        return true;
    } else if (strcmp(tokens[0], "la") == 0) {
        scanner_t s_op = { .p = line };
        scanner_next(&s_op); // skip 'la' (mnemonic)
        scanner_next(&s_op); // load first operand
        int rd = parse_register_scanner(&s_op);
        if (s_op.curr.type == TOK_COMMA) scanner_next(&s_op);
        
        expr_result_t res;
        if (!parse_operand_scanner(&s_op, &res)) return false;
        
        // LUI rd, %hi(symbol)
        ensure_instruction_capacity(as, 2);
        instruction_t *inst1 = &as->instructions[as->num_instructions];
        inst1->opcode = 0x20;  // LUI
        inst1->address = as->current_addr;
        inst1->section = as->current_section;
        inst1->has_symbol_ref = true;
        inst1->symbol_is_hi = true;
        strcpy(inst1->symbol_ref, res.symbol);
        inst1->symbol_addend = res.val;
        inst1->instruction = encode_u(0x20, rd, 0);
        as->num_instructions++;
        as->current_addr += 4;
        bump_size(as, as->current_section, 4);
        
        // ORI rd, rd, %lo(symbol)
        instruction_t *inst2 = &as->instructions[as->num_instructions];
        inst2->opcode = 0x11;  // ORI
        inst2->address = as->current_addr;
        inst2->section = as->current_section;
        inst2->has_symbol_ref = true;
        inst2->symbol_is_lo = true;
        strcpy(inst2->symbol_ref, res.symbol);
        inst2->symbol_addend = res.val;
        inst2->instruction = encode_i(0x11, rd, rd, 0);
        as->num_instructions++;
        as->current_addr += 4;
        bump_size(as, as->current_section, 4);
        return true;
    }
    
    instruction_def_t *inst_def = find_instruction(tokens[0]);
    if (!inst_def) {
        fprintf(stderr, "Unknown instruction: %s\n", tokens[0]);
        return false;
    }
    
    // Use scanner for operands
    scanner_t s_op = { .p = line };
    scanner_next(&s_op); // Load mnemonic
    scanner_next(&s_op); // Load first operand
    
    // Auto-align to word boundary before instructions in code section
    if (as->current_section == SECTION_CODE && (as->current_addr & 3)) {
        int padding = 4 - (as->current_addr & 3);
        ensure_instruction_capacity(as, padding);
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
    
    ensure_instruction_capacity(as, 1);
    instruction_t *inst = &as->instructions[as->num_instructions];
    inst->opcode = inst_def->opcode;
    inst->address = as->current_addr;
    inst->has_label_ref = false;
    inst->has_symbol_ref = false;
    inst->symbol_is_hi = false;
    inst->symbol_is_lo = false;
    inst->symbol_is_pcrel_hi = false;
    inst->symbol_is_pcrel_lo = false;
    inst->symbol_addend = 0;
    inst->section = as->current_section;
    inst->is_data_byte = false;
    
    switch (inst_def->format) {
        case FMT_R: {
            if (inst->opcode == 0x51 || inst->opcode == 0x52) { // YIELD, DEBUG
                int rs1 = parse_register_scanner(&s_op);
                inst->instruction = encode_r(inst->opcode, 0, rs1, 0);
            } else if (inst->opcode == 0x3F) { // ASSERT_EQ
                int rs1 = parse_register_scanner(&s_op);
                if (s_op.curr.type == TOK_COMMA) scanner_next(&s_op);
                int rs2 = parse_register_scanner(&s_op);
                inst->instruction = encode_r(inst->opcode, 0, rs1, rs2);
            } else {
                int rd = parse_register_scanner(&s_op);
                if (s_op.curr.type == TOK_COMMA) scanner_next(&s_op);
                int rs1 = parse_register_scanner(&s_op);
                if (s_op.curr.type == TOK_COMMA) scanner_next(&s_op);
                int rs2 = parse_register_scanner(&s_op);
                inst->instruction = encode_r(inst->opcode, rd, rs1, rs2);
            }
            break;
        }
        
        case FMT_I: {
            int rd = parse_register_scanner(&s_op);
            if (s_op.curr.type == TOK_COMMA) scanner_next(&s_op);
            
            int rs1 = 0;
            int imm = 0;
            
            // Check if next token is register (for ALU ops like addi r1, r2, 4)
            // or if it's immediate/symbol (for Load like ldw r1, 4(r2))
            
            // We need to peek or try parsing
            // parse_register_scanner consumes if match.
            // If it fails, it returns -1 and doesn't consume (except looking at current).
            // Actually my parse_register_scanner implementation checks s->curr.type.
            
            int potential_rs1 = -1;
            if (s_op.curr.type == TOK_NUMBER && s_op.curr.val == 0) {
                scanner_t peek = s_op;
                scanner_next(&peek);
                if (peek.curr.type != TOK_LPAREN) {
                    potential_rs1 = parse_register_scanner(&s_op);
                }
            } else {
                potential_rs1 = parse_register_scanner(&s_op);
            }
            
            if (potential_rs1 >= 0) {
                // It was a register. Format: rd, rs1, imm
                rs1 = potential_rs1;
                if (s_op.curr.type == TOK_COMMA) scanner_next(&s_op);
                
                expr_result_t res;
                if (parse_operand_scanner(&s_op, &res)) {
                    imm = res.val;
                    if (res.has_symbol) {
                        inst->has_symbol_ref = true;
                        strcpy(inst->symbol_ref, res.symbol);
                        inst->symbol_is_lo = res.is_lo;
                        inst->symbol_is_pcrel_lo = res.is_pcrel_lo;
                        inst->symbol_addend = res.val;
                        imm = 0;
                    }
                }
            } else {
                // Not a register. Format: rd, offset(rs1) OR rd, symbol
                expr_result_t res;
                if (parse_operand_scanner(&s_op, &res)) {
                    imm = res.val;
                    if (res.has_symbol) {
                        inst->has_symbol_ref = true;
                        strcpy(inst->symbol_ref, res.symbol);
                        inst->symbol_is_lo = res.is_lo;
                        inst->symbol_is_pcrel_lo = res.is_pcrel_lo;
                        inst->symbol_addend = res.val;
                        imm = 0;
                    }
                }
                
                if (s_op.curr.type == TOK_LPAREN) {
                    scanner_next(&s_op);
                    rs1 = parse_register_scanner(&s_op);
                    if (s_op.curr.type == TOK_RPAREN) scanner_next(&s_op);
                }
            }
            inst->instruction = encode_i(inst->opcode, rd, rs1, imm);
            break;
        }
        
        case FMT_S: {
            // Supported forms:
            //  - stw base+imm, data
            //  - stw base, data, imm (legacy)
            //  - stw data, imm(base) (standard)
            int rs1 = 0, rs2 = 0, imm = 0;
            expr_result_t res;
            
            int reg1 = parse_register_scanner(&s_op);
            if (reg1 < 0) return false;
            
            if (s_op.curr.type == TOK_PLUS || s_op.curr.type == TOK_MINUS) {
                // base+imm, data
                if (!parse_operand_scanner(&s_op, &res)) return false;
                rs1 = reg1;
                if (res.has_symbol) {
                    inst->has_symbol_ref = true;
                    strcpy(inst->symbol_ref, res.symbol);
                    inst->symbol_is_lo = res.is_lo;
                    inst->symbol_is_pcrel_lo = res.is_pcrel_lo;
                    inst->symbol_addend = res.val;
                    imm = 0;
                } else {
                    imm = res.val;
                }
                if (s_op.curr.type == TOK_COMMA) scanner_next(&s_op);
                rs2 = parse_register_scanner(&s_op);
                if (rs2 < 0) return false;
            } else if (s_op.curr.type == TOK_COMMA) {
                scanner_next(&s_op);
                // Peek: register => legacy base,data,imm. Otherwise data, imm(base).
                int reg2 = parse_register_scanner(&s_op);
                if (reg2 >= 0) {
                    // legacy: base, data, imm
                    rs1 = reg1;
                    rs2 = reg2;
                    if (s_op.curr.type == TOK_COMMA) scanner_next(&s_op);
                    if (parse_operand_scanner(&s_op, &res)) {
                        if (res.has_symbol) {
                            inst->has_symbol_ref = true;
                            strcpy(inst->symbol_ref, res.symbol);
                            inst->symbol_is_lo = res.is_lo;
                            inst->symbol_is_pcrel_lo = res.is_pcrel_lo;
                            inst->symbol_addend = res.val;
                            imm = 0;
                        } else {
                            imm = res.val;
                        }
                    }
                } else {
                    // standard: data, imm(base)
                    rs2 = reg1;
                    if (!parse_operand_scanner(&s_op, &res)) return false;
                    if (res.has_symbol) {
                        inst->has_symbol_ref = true;
                        strcpy(inst->symbol_ref, res.symbol);
                        inst->symbol_is_lo = res.is_lo;
                        inst->symbol_is_pcrel_lo = res.is_pcrel_lo;
                        inst->symbol_addend = res.val;
                        imm = 0;
                    } else {
                        imm = res.val;
                    }
                    if (s_op.curr.type == TOK_LPAREN) {
                        scanner_next(&s_op);
                        rs1 = parse_register_scanner(&s_op);
                        if (s_op.curr.type == TOK_RPAREN) scanner_next(&s_op);
                    } else {
                        return false;
                    }
                }
            } else {
                return false;
            }
            
            inst->instruction = encode_s(inst->opcode, rs1, rs2, imm);
            break;
        }
        
        case FMT_B: {
            int rs1 = parse_register_scanner(&s_op);
            if (s_op.curr.type == TOK_COMMA) scanner_next(&s_op);
            int rs2 = parse_register_scanner(&s_op);
            if (s_op.curr.type == TOK_COMMA) scanner_next(&s_op);
            if (swap_branch_regs) {
                int tmp = rs1;
                rs1 = rs2;
                rs2 = tmp;
            }
            
            if (s_op.curr.type == TOK_IDENTIFIER) {
                inst->has_label_ref = true;
                strcpy(inst->label_ref, s_op.curr.sval);
                inst->label_offset = 0;
                inst->instruction = encode_b(inst->opcode, rs1, rs2, 0);
                scanner_next(&s_op);
            } else {
                expr_result_t res;
                parse_operand_scanner(&s_op, &res);
                inst->instruction = encode_b(inst->opcode, rs1, rs2, res.val);
            }
            break;
        }
        
        case FMT_U: {
            int rd = parse_register_scanner(&s_op);
            if (s_op.curr.type == TOK_COMMA) scanner_next(&s_op);
            
            expr_result_t res;
            parse_operand_scanner(&s_op, &res);
            
            if (res.has_symbol) {
                inst->has_symbol_ref = true;
                strcpy(inst->symbol_ref, res.symbol);
                inst->symbol_is_hi = res.is_hi;
                inst->symbol_is_pcrel_hi = res.is_pcrel_hi;
                inst->symbol_addend = res.val;
                inst->instruction = encode_u(inst->opcode, rd, 0);
            } else {
                inst->instruction = encode_u(inst->opcode, rd, res.val);
            }
            break;
        }
        
        case FMT_J: {
            int rd = 31;
            // JAL can be `jal label` (rd=ra) or `jal rd, label`
            // Try to parse register
            scanner_t s_peek = s_op;
            int r = parse_register_scanner(&s_peek);
            if (r >= 0 && s_peek.curr.type == TOK_COMMA) {
                rd = r;
                s_op = s_peek;
                scanner_next(&s_op);
            }
            
            if (s_op.curr.type == TOK_IDENTIFIER) {
                inst->has_label_ref = true;
                strcpy(inst->label_ref, s_op.curr.sval);
                inst->label_offset = 0;
                inst->instruction = encode_j(inst->opcode, rd, 0);
                scanner_next(&s_op);
            } else {
                expr_result_t res;
                parse_operand_scanner(&s_op, &res);
                inst->instruction = encode_j(inst->opcode, rd, res.val);
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
static int resolve_label_diffs(assembler_t *as) {
    for (int i = 0; i < as->num_label_diffs; i++) {
        label_t *plus = find_label(as, as->label_diffs[i].plus_symbol);
        label_t *minus = find_label(as, as->label_diffs[i].minus_symbol);
        if (!plus || !plus->is_defined) {
            fprintf(stderr, "Label difference: undefined symbol '%s'\n",
                    as->label_diffs[i].plus_symbol);
            return -1;
        }
        if (!minus || !minus->is_defined) {
            fprintf(stderr, "Label difference: undefined symbol '%s'\n",
                    as->label_diffs[i].minus_symbol);
            return -1;
        }
        if (plus->section != minus->section) {
            fprintf(stderr, "Label difference: '%s' and '%s' are in different sections\n",
                    as->label_diffs[i].plus_symbol, as->label_diffs[i].minus_symbol);
            return -1;
        }
        int32_t val = (int32_t)(plus->address - minus->address) + as->label_diffs[i].addend;
        as->instructions[as->label_diffs[i].instruction_index].instruction = (uint32_t)val;
    }
    return 0;
}

static int write_object_file(assembler_t *as, const char *filename) {
    // Resolve label-difference fixups before emitting
    if (as->num_label_diffs > 0) {
        if (resolve_label_diffs(as) != 0) return -1;
    }

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
    uint32_t text_name_offset = 0, data_name_offset = 0, bss_name_offset = 0;
    uint32_t rodata_name_offset = 0, init_array_name_offset = 0;

    // Count relocations per section
    int text_relocs = 0, data_relocs = 0, rodata_relocs = 0, bss_relocs = 0, init_array_relocs = 0;
    for (int i = 0; i < as->num_relocations; i++) {
        switch (as->relocations[i].section) {
            case SECTION_CODE: text_relocs++; break;
            case SECTION_DATA: data_relocs++; break;
            case SECTION_RODATA: rodata_relocs++; break;
            case SECTION_BSS: bss_relocs++; break;
            case SECTION_INIT_ARRAY: init_array_relocs++; break;
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
    if (as->init_array_size > 0) {
        init_array_name_offset = add_string(as, ".init_array");
        num_sections++;
    }

    // Create mapping from internal section enum to output section index
    int section_map[5] = {0, 0, 0, 0, 0};  // Maps SECTION_* to output section index
    int output_section_idx = 1;  // Output sections are 1-based
    if (as->code_size > 0) section_map[SECTION_CODE] = output_section_idx++;
    if (as->rodata_size > 0) section_map[SECTION_RODATA] = output_section_idx++;
    if (as->data_size > 0) section_map[SECTION_DATA] = output_section_idx++;
    if (as->bss_size > 0) section_map[SECTION_BSS] = output_section_idx++;
    if (as->init_array_size > 0) section_map[SECTION_INIT_ARRAY] = output_section_idx++;
    
    // Build symbol table (heap-allocated to avoid ~640KB stack usage)
    int num_symbols = 0;
    s32o_symbol_t *symbols = malloc(MAX_LABELS * sizeof(s32o_symbol_t));
    if (!symbols) {
        fprintf(stderr, "Memory allocation failed for symbol table\n");
        fclose(f);
        return -1;
    }

    // 1) Add all global labels
    for (int i = 0; i < as->num_labels; i++) {
        if (!as->labels[i].is_global) continue;
        if (num_symbols >= MAX_LABELS) {
            fprintf(stderr, "Too many symbols (max %d)\n", MAX_LABELS);
            free(symbols);
            fclose(f);
            return -1;
        }

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
        
        if (num_symbols >= MAX_LABELS) {
            fprintf(stderr, "Too many symbols (max %d)\n", MAX_LABELS);
            free(symbols);
            fclose(f);
            return -1;
        }

        // Is it a defined label in this object (likely a local .L*)?
        int lbl = find_label_index(as, sym);
        if (lbl >= 0 && as->labels[lbl].is_defined) {
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
    uint32_t rodata_reloc_offset = 0, bss_reloc_offset = 0, init_array_reloc_offset = 0;

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
    if (init_array_relocs > 0) {
        init_array_reloc_offset = offset;
        offset += init_array_relocs * sizeof(s32o_reloc_t);
    }
    
    uint32_t string_table_offset = offset;
    offset += as->string_table_size;
    
    // Align to 4 bytes
    offset = (offset + 3) & ~3;
    
    // Section data offsets
    uint32_t code_offset = 0, data_offset = 0, rodata_offset = 0, init_array_offset = 0;
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
    if (as->init_array_size > 0) {
        init_array_offset = offset;
        offset += as->init_array_size;
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

    if (as->init_array_size > 0) {
        s32o_section_t section = {
            .name_offset = init_array_name_offset,
            .type = S32_SEC_DATA,  // Treat as data for linking purposes
            .flags = S32_SEC_FLAG_READ | S32_SEC_FLAG_WRITE | S32_SEC_FLAG_ALLOC,
            .size = as->init_array_size,
            .offset = init_array_offset,
            .align = 4,
            .nrelocs = init_array_relocs,
            .reloc_offset = init_array_reloc_offset
        };
        fwrite(&section, sizeof(section), 1, f);
    }

    // Write symbol table
    fwrite(symbols, sizeof(s32o_symbol_t), num_symbols, f);
    
    // Write relocations for each section
    for (int sec = 0; sec < 5; sec++) {
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

    // Write .init_array section
    if (as->init_array_size > 0) {
        fseek(f, init_array_offset, SEEK_SET);
        for (int i = 0; i < as->num_instructions; i++) {
            instruction_t *inst = &as->instructions[i];
            if (inst->section != SECTION_INIT_ARRAY) continue;

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
    free(symbols);

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
    as.instructions_capacity = INITIAL_INSTRUCTION_CAPACITY;
    as.instructions = calloc(as.instructions_capacity, sizeof(instruction_t));
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
    
    // Process all symbol references (%hi, %lo, %pcrel_hi, %pcrel_lo)
    for (int i = 0; i < as.num_instructions; i++) {
        instruction_t *inst = &as.instructions[i];
        if (inst->has_symbol_ref) {
            // Determine relocation type based on the symbol reference type
            uint32_t reloc_type;
            if (inst->symbol_is_hi) {
                reloc_type = S32O_REL_HI20;
            } else if (inst->symbol_is_lo) {
                reloc_type = S32O_REL_LO12;
            } else if (inst->symbol_is_pcrel_hi) {
                reloc_type = S32O_REL_PCREL_HI20;
            } else if (inst->symbol_is_pcrel_lo) {
                reloc_type = S32O_REL_PCREL_LO12;
            } else {
                // Default to 32-bit relocation if no specific type
                reloc_type = S32O_REL_32;
            }
            
            // Add relocation with the addend
            add_relocation(&as, inst->address, inst->symbol_ref, 
                          reloc_type, inst->symbol_addend, inst->section);
            inst->needs_relocation = true;
        }
    }
    
    // Process all label references
    bool label_error = false;
    for (int i = 0; i < as.num_instructions && !label_error; i++) {
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
                    // Check if offset fits in 12-bit signed immediate (even values only)
                    if (offset < -4096 || offset > 4094) {
                        fprintf(stderr, "Error: Branch offset out of range at address 0x%08X\n", inst->address);
                        fprintf(stderr, "       Target label '%s' at 0x%08X is %d bytes away\n",
                                inst->label_ref, label_addr, offset);
                        fprintf(stderr, "       Branch instructions can only reach +/-4096 bytes\n");
                        label_error = true;
                        break;
                    }
                    inst->instruction = (inst->instruction & 0x01FFF07F) |
                        (encode_b(0, 0, 0, offset) & 0xFE000F80);
                    break;
                case FMT_J:
                    // JAL is PC relative (not PC+4)
                    offset = label_addr - inst->address;
                    // Check if offset fits in 20-bit signed immediate (even values only)
                    if (offset < -1048576 || offset > 1048574) {
                        fprintf(stderr, "Error: JAL offset out of range at address 0x%08X\n", inst->address);
                        fprintf(stderr, "       Target label '%s' at 0x%08X is %d bytes away\n",
                                inst->label_ref, label_addr, offset);
                        fprintf(stderr, "       JAL instructions can only reach +/-1MB\n");
                        label_error = true;
                        break;
                    }
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
    
    if (label_error) {
        free(as.instructions);
        return 1;
    }

    int result = write_object_file(&as, output_file);
    free(as.instructions);
    return result;
}
