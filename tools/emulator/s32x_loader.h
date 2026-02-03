// SLOW-32 Executable (.s32x) Loader
// Shared by all emulator implementations

#ifndef S32X_LOADER_H
#define S32X_LOADER_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "../../common/s32_formats.h"

// Callback for writing data to emulator memory
// Returns 0 on success, non-zero on failure
typedef int (*s32x_write_cb_t)(void *user_data, uint32_t addr, const void *data, uint32_t size);

// Loader configuration
typedef struct {
    s32x_write_cb_t write_cb;
    void *user_data;
    uint32_t mem_size;
    int verbose;
} s32x_loader_config_t;

// Load result
typedef struct {
    int success;
    uint32_t entry_point;
    uint32_t stack_base;
    uint32_t stack_end;
    uint32_t code_limit;
    uint32_t rodata_limit;
    uint32_t data_limit;
    uint32_t heap_base;
    uint32_t mmio_base;
    uint32_t mem_size;
    uint32_t flags;
    int has_wxorx;
    int has_mmio;
    char error_msg[256];
} s32x_load_result_t;

// Load .s32x header only (no section loading)
// Use this to get metadata before setting up memory, then call load_s32x_file() to load sections.
static s32x_load_result_t load_s32x_header(const char *filename) {
    s32x_load_result_t result = {0};

    FILE *f = fopen(filename, "rb");
    if (!f) {
        snprintf(result.error_msg, sizeof(result.error_msg),
                 "Cannot open file: %s", filename);
        return result;
    }

    s32x_header_t header;
    if (fread(&header, sizeof(header), 1, f) != 1) {
        snprintf(result.error_msg, sizeof(result.error_msg),
                 "Cannot read header");
        fclose(f);
        return result;
    }
    fclose(f);

    // Verify magic
    if (header.magic != S32X_MAGIC) {
        snprintf(result.error_msg, sizeof(result.error_msg),
                 "Invalid magic: 0x%08X (expected 0x%08X)",
                 header.magic, S32X_MAGIC);
        return result;
    }

    // Verify version
    if (header.version != 1) {
        snprintf(result.error_msg, sizeof(result.error_msg),
                 "Unsupported version: %d", header.version);
        return result;
    }

    // Verify machine type
    if (header.machine != S32_MACHINE_SLOW32) {
        snprintf(result.error_msg, sizeof(result.error_msg),
                 "Wrong machine type: 0x%02X", header.machine);
        return result;
    }

    // Validate code limit
    if (header.code_limit > 0x100000) {
        snprintf(result.error_msg, sizeof(result.error_msg),
                 "Code limit exceeds 1MB boundary: 0x%08X", header.code_limit);
        return result;
    }

    result.success = 1;
    result.entry_point = header.entry;
    result.stack_base = header.stack_base;
    result.stack_end = header.stack_end;
    result.code_limit = header.code_limit;
    result.rodata_limit = header.rodata_limit;
    result.data_limit = header.data_limit;
    result.heap_base = header.heap_base;
    result.mmio_base = header.mmio_base;
    result.mem_size = header.mem_size;
    result.flags = header.flags;
    result.has_wxorx = (header.flags & S32X_FLAG_W_XOR_X) != 0;
    result.has_mmio = (header.flags & S32X_FLAG_MMIO) != 0;

    return result;
}

// Load .s32x executable
static s32x_load_result_t load_s32x_file(const char *filename, s32x_loader_config_t *config) {
    s32x_load_result_t result = {0};
    
    FILE *f = fopen(filename, "rb");
    if (!f) {
        snprintf(result.error_msg, sizeof(result.error_msg), 
                 "Cannot open file: %s", filename);
        return result;
    }
    
    // Read header
    s32x_header_t header;
    if (fread(&header, sizeof(header), 1, f) != 1) {
        snprintf(result.error_msg, sizeof(result.error_msg), 
                 "Cannot read header");
        fclose(f);
        return result;
    }
    
    // Verify magic
    if (header.magic != S32X_MAGIC) {
        snprintf(result.error_msg, sizeof(result.error_msg), 
                 "Invalid magic: 0x%08X (expected 0x%08X)", 
                 header.magic, S32X_MAGIC);
        fclose(f);
        return result;
    }
    
    // Verify version
    if (header.version != 1) {
        snprintf(result.error_msg, sizeof(result.error_msg), 
                 "Unsupported version: %d", header.version);
        fclose(f);
        return result;
    }
    
    // Verify machine type
    if (header.machine != S32_MACHINE_SLOW32) {
        snprintf(result.error_msg, sizeof(result.error_msg), 
                 "Wrong machine type: 0x%02X", header.machine);
        fclose(f);
        return result;
    }
    
    // Check memory requirements
    if (header.mem_size > config->mem_size) {
        snprintf(result.error_msg, sizeof(result.error_msg), 
                 "Executable requires %u bytes, emulator has %u", 
                 header.mem_size, config->mem_size);
        fclose(f);
        return result;
    }
    
    // Validate memory layout
    if (header.code_limit > 0x100000) {
        snprintf(result.error_msg, sizeof(result.error_msg), 
                 "Code limit exceeds 1MB boundary: 0x%08X", header.code_limit);
        fclose(f);
        return result;
    }
    
    if (header.data_limit > config->mem_size || 
        header.stack_base > config->mem_size) {
        snprintf(result.error_msg, sizeof(result.error_msg), 
                 "Memory layout exceeds available memory");
        fclose(f);
        return result;
    }
    
    // Read string table
    char *strtab = malloc(header.str_size);
    if (!strtab) {
        snprintf(result.error_msg, sizeof(result.error_msg), 
                 "Out of memory for string table");
        fclose(f);
        return result;
    }
    
    if (fseek(f, header.str_offset, SEEK_SET) != 0 ||
        fread(strtab, 1, header.str_size, f) != header.str_size) {
        snprintf(result.error_msg, sizeof(result.error_msg), 
                 "Cannot read string table");
        free(strtab);
        fclose(f);
        return result;
    }
    
    // Read and load sections
    if (fseek(f, header.sec_offset, SEEK_SET) != 0) {
        snprintf(result.error_msg, sizeof(result.error_msg), 
                 "Cannot seek to section table");
        free(strtab);
        fclose(f);
        return result;
    }

    // Allocate buffer for section data
    uint32_t max_sec_size = 0;
    long sec_table_pos = ftell(f);
    
    // First pass: find max section size to allocate buffer
    for (uint32_t i = 0; i < header.nsections; i++) {
        s32x_section_t section;
        if (fread(&section, sizeof(section), 1, f) != 1) break;
        if (section.size > max_sec_size) max_sec_size = section.size;
    }
    
    uint8_t *sec_buf = malloc(max_sec_size > 0 ? max_sec_size : 1);
    if (!sec_buf) {
        snprintf(result.error_msg, sizeof(result.error_msg), 
                 "Out of memory for section buffer");
        free(strtab);
        fclose(f);
        return result;
    }
    
    // Second pass: load sections
    fseek(f, sec_table_pos, SEEK_SET);
    
    for (uint32_t i = 0; i < header.nsections; i++) {
        s32x_section_t section;
        if (fread(&section, sizeof(section), 1, f) != 1) {
            snprintf(result.error_msg, sizeof(result.error_msg), 
                     "Cannot read section %u", i);
            free(sec_buf);
            free(strtab);
            fclose(f);
            return result;
        }
        
        const char *name = (section.name_offset < header.str_size) ?
                           &strtab[section.name_offset] : "?";

        // Skip non-loadable sections (SYMTAB, STRTAB with mem_size=0)
        if (section.mem_size == 0) {
            if (config->verbose) {
                printf("Skipping non-loadable section '%s'\n", name);
            }
            continue;
        }

        if (config->verbose) {
            printf("Loading section '%s': %u bytes at 0x%08X\n",
                   name, section.mem_size, section.vaddr);
        }

        // Validate section address
        if (section.vaddr / 4 >= config->mem_size / 4) {
            snprintf(result.error_msg, sizeof(result.error_msg), 
                     "Section '%s' address out of bounds: 0x%08X", 
                     name, section.vaddr);
            free(sec_buf);
            free(strtab);
            fclose(f);
            return result;
        }
        
        // Load section data (if not BSS)
        if (section.size > 0 && section.offset > 0) {
            long current_pos = ftell(f);
            fseek(f, section.offset, SEEK_SET);
            
            if (fread(sec_buf, 1, section.size, f) != section.size) {
                snprintf(result.error_msg, sizeof(result.error_msg), 
                         "Cannot read section '%s' data", name);
                free(sec_buf);
                free(strtab);
                fclose(f);
                return result;
            }
            
            // Write to emulator memory via callback
            if (config->write_cb(config->user_data, section.vaddr, sec_buf, section.size) != 0) {
                snprintf(result.error_msg, sizeof(result.error_msg), 
                         "Failed to write section '%s' to memory", name);
                free(sec_buf);
                free(strtab);
                fclose(f);
                return result;
            }
            
            fseek(f, current_pos, SEEK_SET);
        }
        
        // Zero-fill BSS sections and padding
        // Note: The callback interface doesn't support "memset", so we write zeros in chunks if needed
        // But typically, memory is already zeroed by the emulator. 
        // If strict correctness is required, we should explicitly zero it.
        if (section.mem_size > section.size) {
            // For efficiency, we assume the emulator zeroes memory on init.
            // If explicit zeroing is needed, we'd loop writing zeros here.
            // Since we don't want to allocate a huge zero buffer, we skip explicit zeroing
            // relying on the emulator's initialization (calloc/mmap).
        }
    }
    
    free(sec_buf);
    free(strtab);
    fclose(f);
    
    // Set up return values
    result.success = 1;
    result.entry_point = header.entry;
    result.stack_base = header.stack_base;
    result.stack_end = header.stack_end;
    result.code_limit = header.code_limit;
    result.rodata_limit = header.rodata_limit;
    result.data_limit = header.data_limit;
    result.heap_base = header.heap_base;
    result.mmio_base = header.mmio_base;
    result.mem_size = header.mem_size;
    result.flags = header.flags;
    result.has_wxorx = (header.flags & S32X_FLAG_W_XOR_X) != 0;
    result.has_mmio = (header.flags & S32X_FLAG_MMIO) != 0;

    if (config->verbose) {
        printf("Loaded .s32x executable:\n");
        printf("  Entry point: 0x%08X\n", result.entry_point);
        printf("  Stack base:  0x%08X\n", result.stack_base);
        printf("  Code limit:  0x%08X\n", result.code_limit);
        printf("  Data limit:  0x%08X\n", result.data_limit);
        printf("  W^X: %s\n", result.has_wxorx ? "enabled" : "disabled");
    }
    
    return result;
}

// ============================================================================
// Symbol table lookup from .s32x files
// ============================================================================

// A single resolved symbol
typedef struct {
    const char *name;       // Points into sym_strtab (caller must keep alive)
    uint32_t value;         // Resolved address
    uint8_t type;           // Symbol type (S32O_SYM_*)
} s32x_symbol_t;

// Result of symbol table loading
typedef struct {
    int success;
    s32x_symbol_t *symbols;  // Heap-allocated array (caller frees)
    char *sym_strtab;        // Heap-allocated string table (caller frees)
    uint32_t num_symbols;
    char error_msg[256];
} s32x_symtab_result_t;

// Load symbol table from a .s32x file
// Returns symbols array and string table; caller must free both.
static s32x_symtab_result_t load_s32x_symtab(const char *filename) {
    s32x_symtab_result_t result = {0};

    FILE *f = fopen(filename, "rb");
    if (!f) {
        snprintf(result.error_msg, sizeof(result.error_msg),
                 "Cannot open file: %s", filename);
        return result;
    }

    // Read header
    s32x_header_t header;
    if (fread(&header, sizeof(header), 1, f) != 1 ||
        header.magic != S32X_MAGIC) {
        snprintf(result.error_msg, sizeof(result.error_msg),
                 "Invalid s32x file");
        fclose(f);
        return result;
    }

    // Read section name string table
    char *strtab = malloc(header.str_size);
    if (!strtab) { fclose(f); return result; }
    fseek(f, header.str_offset, SEEK_SET);
    if (fread(strtab, 1, header.str_size, f) != header.str_size) {
        free(strtab);
        fclose(f);
        return result;
    }

    // Scan sections for SYMTAB and STRTAB
    uint32_t symtab_offset = 0, symtab_size = 0;
    uint32_t sym_strtab_offset = 0, sym_strtab_size = 0;

    fseek(f, header.sec_offset, SEEK_SET);
    for (uint32_t i = 0; i < header.nsections; i++) {
        s32x_section_t sec;
        if (fread(&sec, sizeof(sec), 1, f) != 1) break;
        if (sec.type == S32_SEC_SYMTAB) {
            symtab_offset = sec.offset;
            symtab_size = sec.size;
        } else if (sec.type == S32_SEC_STRTAB) {
            // Distinguish symbol STRTAB from section name STRTAB
            // Symbol STRTAB has vaddr=0 and mem_size=0
            if (sec.vaddr == 0 && sec.mem_size == 0 &&
                sec.offset != header.str_offset) {
                sym_strtab_offset = sec.offset;
                sym_strtab_size = sec.size;
            }
        }
    }

    free(strtab);

    if (symtab_offset == 0 || sym_strtab_offset == 0 || symtab_size == 0) {
        // No symbol table present - not an error, just empty
        result.success = 1;
        result.num_symbols = 0;
        fclose(f);
        return result;
    }

    // Read symbol string table
    char *sym_str = malloc(sym_strtab_size);
    if (!sym_str) { fclose(f); return result; }
    fseek(f, sym_strtab_offset, SEEK_SET);
    if (fread(sym_str, 1, sym_strtab_size, f) != sym_strtab_size) {
        free(sym_str);
        fclose(f);
        return result;
    }

    // Read symbol table entries
    uint32_t nsyms = symtab_size / sizeof(s32o_symbol_t);
    s32o_symbol_t *raw_syms = malloc(symtab_size);
    if (!raw_syms) { free(sym_str); fclose(f); return result; }
    fseek(f, symtab_offset, SEEK_SET);
    if (fread(raw_syms, sizeof(s32o_symbol_t), nsyms, f) != nsyms) {
        free(raw_syms);
        free(sym_str);
        fclose(f);
        return result;
    }

    fclose(f);

    // Build result array
    s32x_symbol_t *syms = calloc(nsyms, sizeof(s32x_symbol_t));
    if (!syms) { free(raw_syms); free(sym_str); return result; }

    for (uint32_t i = 0; i < nsyms; i++) {
        if (raw_syms[i].name_offset < sym_strtab_size) {
            syms[i].name = &sym_str[raw_syms[i].name_offset];
        } else {
            syms[i].name = "";
        }
        syms[i].value = raw_syms[i].value;
        syms[i].type = raw_syms[i].type;
    }

    free(raw_syms);

    result.success = 1;
    result.symbols = syms;
    result.sym_strtab = sym_str;
    result.num_symbols = nsyms;
    return result;
}

// Look up a symbol by name in a loaded symbol table
// Returns the symbol value (address), or 0 if not found
static uint32_t s32x_symtab_lookup(s32x_symtab_result_t *st, const char *name) {
    if (!st->symbols) return 0;
    for (uint32_t i = 0; i < st->num_symbols; i++) {
        if (st->symbols[i].name && strcmp(st->symbols[i].name, name) == 0) {
            return st->symbols[i].value;
        }
    }
    return 0;
}

// Free resources from symbol table loading
static void s32x_symtab_free(s32x_symtab_result_t *st) {
    free(st->symbols);
    st->symbols = NULL;
    free(st->sym_strtab);
    st->sym_strtab = NULL;
    st->num_symbols = 0;
}

#endif // S32X_LOADER_H
