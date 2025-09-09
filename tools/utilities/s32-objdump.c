// SLOW-32 Object File Dumper
// Displays contents of .s32o object files for debugging and verification

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <ctype.h>
#include "../../common/s32_formats.h"

// Command-line options
typedef struct {
    bool show_header;
    bool show_sections;
    bool show_symbols;
    bool show_relocations;
    bool show_hex;
    bool show_all;
    bool verbose;
} options_t;

// Global state
typedef struct {
    FILE *file;
    s32o_header_t header;
    s32o_section_t *sections;
    s32o_symbol_t *symbols;
    char *string_table;
} objdump_state_t;

static const char *get_string(objdump_state_t *state, uint32_t offset) {
    if (offset >= state->header.str_size) {
        return "<invalid>";
    }
    return &state->string_table[offset];
}

static const char *section_type_name(uint32_t type) {
    switch (type) {
        case S32_SEC_NULL:    return "NULL";
        case S32_SEC_CODE:    return "CODE";
        case S32_SEC_DATA:    return "DATA";
        case S32_SEC_BSS:     return "BSS";
        case S32_SEC_RODATA:  return "RODATA";
        case S32_SEC_EVT:     return "EVT";
        case S32_SEC_TSR:     return "TSR";
        case S32_SEC_DEBUG:   return "DEBUG";
        case S32_SEC_SYMTAB:  return "SYMTAB";
        case S32_SEC_STRTAB:  return "STRTAB";
        default:              return "UNKNOWN";
    }
}

static const char *symbol_type_name(uint8_t type) {
    switch (type) {
        case S32O_SYM_NOTYPE:  return "NOTYPE";
        case S32O_SYM_FUNC:    return "FUNC";
        case S32O_SYM_OBJECT:  return "OBJECT";
        case S32O_SYM_SECTION: return "SECTION";
        default:               return "UNKNOWN";
    }
}

static const char *symbol_binding_name(uint8_t binding) {
    switch (binding) {
        case S32O_BIND_LOCAL:  return "LOCAL";
        case S32O_BIND_GLOBAL: return "GLOBAL";
        case S32O_BIND_WEAK:   return "WEAK";
        default:               return "UNKNOWN";
    }
}

static const char *reloc_type_name(uint32_t type) {
    switch (type) {
        case S32O_REL_NONE:   return "NONE";
        case S32O_REL_32:     return "32";
        case S32O_REL_HI20:   return "HI20";
        case S32O_REL_LO12:   return "LO12";
        case S32O_REL_BRANCH: return "BRANCH";
        case S32O_REL_JAL:    return "JAL";
        case S32O_REL_CALL:   return "CALL";
        default:              return "UNKNOWN";
    }
}

static void print_header(objdump_state_t *state) {
    printf("\nObject File Header:\n");
    printf("  Magic:        0x%08X (%c%c%c%c)\n", state->header.magic,
           (char)(state->header.magic & 0xFF),
           (char)((state->header.magic >> 8) & 0xFF),
           (char)((state->header.magic >> 16) & 0xFF),
           (char)((state->header.magic >> 24) & 0xFF));
    printf("  Version:      %d\n", state->header.version);
    printf("  Endianness:   %s\n", state->header.endian == S32_ENDIAN_LITTLE ? "Little" : "Big");
    printf("  Machine:      0x%02X (SLOW-32)\n", state->header.machine);
    printf("  Flags:        0x%08X", state->header.flags);
    if (state->header.flags & S32O_FLAG_PIC) printf(" PIC");
    if (state->header.flags & S32O_FLAG_DEBUG) printf(" DEBUG");
    if (state->header.flags & S32O_FLAG_STRIPPED) printf(" STRIPPED");
    printf("\n");
    printf("  Sections:     %d at offset 0x%X\n", state->header.nsections, state->header.sec_offset);
    printf("  Symbols:      %d at offset 0x%X\n", state->header.nsymbols, state->header.sym_offset);
    printf("  String Table: %d bytes at offset 0x%X\n", state->header.str_size, state->header.str_offset);
    printf("  Checksum:     0x%08X\n", state->header.checksum);
}

static void print_sections(objdump_state_t *state) {
    printf("\nSections:\n");
    printf("Idx Name              Type     Size     Offset   Align  Relocs  Flags\n");
    printf("--- ----------------- -------- -------- -------- ------ ------- -------\n");
    
    for (uint32_t i = 0; i < state->header.nsections; i++) {
        s32o_section_t *sec = &state->sections[i];
        const char *name = get_string(state, sec->name_offset);
        
        printf("%3d %-17s %-8s %8d %08X %6d %7d ",
               i, name, section_type_name(sec->type),
               sec->size, sec->offset, sec->align, sec->nrelocs);
        
        if (sec->flags & S32_SEC_FLAG_EXEC) printf("X");
        if (sec->flags & S32_SEC_FLAG_WRITE) printf("W");
        if (sec->flags & S32_SEC_FLAG_READ) printf("R");
        if (sec->flags & S32_SEC_FLAG_ALLOC) printf("A");
        printf("\n");
    }
}

static void print_symbols(objdump_state_t *state) {
    printf("\nSymbol Table:\n");
    printf("Num Value    Size     Type     Bind     Sec  Name\n");
    printf("--- -------- -------- -------- -------- ---- -----------------\n");
    
    for (uint32_t i = 0; i < state->header.nsymbols; i++) {
        s32o_symbol_t *sym = &state->symbols[i];
        const char *name = get_string(state, sym->name_offset);
        
        printf("%3d %08X %8d %-8s %-8s ", 
               i, sym->value, sym->size,
               symbol_type_name(sym->type),
               symbol_binding_name(sym->binding));
        
        if (sym->section == 0) {
            printf("UND  ");
        } else if (sym->section == 0xFFFF) {
            printf("ABS  ");
        } else {
            printf("%4d ", sym->section - 1);
        }
        
        printf("%s\n", name);
    }
}

static void print_relocations(objdump_state_t *state) {
    for (uint32_t i = 0; i < state->header.nsections; i++) {
        s32o_section_t *sec = &state->sections[i];
        if (sec->nrelocs == 0) continue;
        
        const char *sec_name = get_string(state, sec->name_offset);
        printf("\nRelocation section for '%s':\n", sec_name);
        printf("Offset   Type     Symbol   Addend   Symbol Name\n");
        printf("-------- -------- -------- -------- -----------------\n");
        
        // Read relocations for this section
        fseek(state->file, sec->reloc_offset, SEEK_SET);
        for (uint32_t j = 0; j < sec->nrelocs; j++) {
            s32o_reloc_t reloc;
            fread(&reloc, sizeof(s32o_reloc_t), 1, state->file);
            
            const char *sym_name = "???";
            if (reloc.symbol < state->header.nsymbols) {
                sym_name = get_string(state, state->symbols[reloc.symbol].name_offset);
            }
            
            printf("%08X %-8s %8d %8d %s\n",
                   reloc.offset, reloc_type_name(reloc.type),
                   reloc.symbol, reloc.addend, sym_name);
        }
    }
}

static void hex_dump(const uint8_t *data, uint32_t size, uint32_t base_addr) {
    for (uint32_t i = 0; i < size; i += 16) {
        printf("  %08X: ", base_addr + i);
        
        // Hex bytes
        for (uint32_t j = 0; j < 16; j++) {
            if (i + j < size) {
                printf("%02X ", data[i + j]);
            } else {
                printf("   ");
            }
            if (j == 7) printf(" ");
        }
        
        printf(" |");
        
        // ASCII representation
        for (uint32_t j = 0; j < 16 && i + j < size; j++) {
            uint8_t c = data[i + j];
            if (isprint(c)) {
                printf("%c", c);
            } else {
                printf(".");
            }
        }
        
        printf("|\n");
    }
}

static void print_section_contents(objdump_state_t *state) {
    for (uint32_t i = 0; i < state->header.nsections; i++) {
        s32o_section_t *sec = &state->sections[i];
        const char *name = get_string(state, sec->name_offset);
        
        // Skip empty sections and BSS
        if (sec->size == 0 || sec->type == S32_SEC_BSS) continue;
        
        printf("\nContents of section %s:\n", name);
        
        uint8_t *data = malloc(sec->size);
        if (!data) {
            fprintf(stderr, "Error: Out of memory\n");
            continue;
        }
        
        fseek(state->file, sec->offset, SEEK_SET);
        fread(data, 1, sec->size, state->file);
        
        hex_dump(data, sec->size, 0);
        
        free(data);
    }
}

static bool load_object_file(objdump_state_t *state, const char *filename) {
    state->file = fopen(filename, "rb");
    if (!state->file) {
        fprintf(stderr, "Error: Cannot open file '%s'\n", filename);
        return false;
    }
    
    // Read header
    if (fread(&state->header, sizeof(s32o_header_t), 1, state->file) != 1) {
        fprintf(stderr, "Error: Cannot read object file header\n");
        fclose(state->file);
        return false;
    }
    
    // Verify magic
    if (state->header.magic != S32O_MAGIC) {
        fprintf(stderr, "Error: Not a valid .s32o file (bad magic: 0x%08X)\n", state->header.magic);
        fclose(state->file);
        return false;
    }
    
    // Read sections
    state->sections = calloc(state->header.nsections, sizeof(s32o_section_t));
    fseek(state->file, state->header.sec_offset, SEEK_SET);
    fread(state->sections, sizeof(s32o_section_t), state->header.nsections, state->file);
    
    // Read symbols
    state->symbols = calloc(state->header.nsymbols, sizeof(s32o_symbol_t));
    fseek(state->file, state->header.sym_offset, SEEK_SET);
    fread(state->symbols, sizeof(s32o_symbol_t), state->header.nsymbols, state->file);
    
    // Read string table
    state->string_table = malloc(state->header.str_size);
    fseek(state->file, state->header.str_offset, SEEK_SET);
    fread(state->string_table, 1, state->header.str_size, state->file);
    
    return true;
}

static void cleanup_state(objdump_state_t *state) {
    if (state->file) fclose(state->file);
    if (state->sections) free(state->sections);
    if (state->symbols) free(state->symbols);
    if (state->string_table) free(state->string_table);
}

static void print_usage(const char *prog) {
    fprintf(stderr, "Usage: %s [options] file.s32o\n", prog);
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "  -h    Display file header\n");
    fprintf(stderr, "  -S    Display section headers\n");
    fprintf(stderr, "  -s    Display symbol table\n");
    fprintf(stderr, "  -r    Display relocations\n");
    fprintf(stderr, "  -x    Display section contents as hex\n");
    fprintf(stderr, "  -a    Display all information\n");
    fprintf(stderr, "  -v    Verbose output\n");
    fprintf(stderr, "  --help Show this help\n");
}

int main(int argc, char *argv[]) {
    options_t opts = { 0 };
    const char *filename = NULL;
    
    // Parse command line
    for (int i = 1; i < argc; i++) {
        if (argv[i][0] == '-') {
            if (strcmp(argv[i], "--help") == 0) {
                print_usage(argv[0]);
                return 0;
            }
            for (int j = 1; argv[i][j]; j++) {
                switch (argv[i][j]) {
                    case 'h': opts.show_header = true; break;
                    case 'S': opts.show_sections = true; break;
                    case 's': opts.show_symbols = true; break;
                    case 'r': opts.show_relocations = true; break;
                    case 'x': opts.show_hex = true; break;
                    case 'a': opts.show_all = true; break;
                    case 'v': opts.verbose = true; break;
                    default:
                        fprintf(stderr, "Unknown option: -%c\n", argv[i][j]);
                        return 1;
                }
            }
        } else {
            if (filename) {
                fprintf(stderr, "Error: Multiple files specified\n");
                return 1;
            }
            filename = argv[i];
        }
    }
    
    if (!filename) {
        fprintf(stderr, "Error: No input file specified\n");
        print_usage(argv[0]);
        return 1;
    }
    
    // Default to showing everything if no options
    if (!opts.show_header && !opts.show_sections && !opts.show_symbols && 
        !opts.show_relocations && !opts.show_hex && !opts.show_all) {
        opts.show_all = true;
    }
    
    if (opts.show_all) {
        opts.show_header = true;
        opts.show_sections = true;
        opts.show_symbols = true;
        opts.show_relocations = true;
        opts.show_hex = true;
    }
    
    // Load the object file
    objdump_state_t state = { 0 };
    if (!load_object_file(&state, filename)) {
        return 1;
    }
    
    printf("\n%s:     file format s32o-slow32\n", filename);
    
    // Display requested information
    if (opts.show_header) print_header(&state);
    if (opts.show_sections) print_sections(&state);
    if (opts.show_symbols) print_symbols(&state);
    if (opts.show_relocations) print_relocations(&state);
    if (opts.show_hex) print_section_contents(&state);
    
    cleanup_state(&state);
    return 0;
}