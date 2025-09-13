// SLOW-32 Linker
// Links multiple .s32o object files into a single .s32x executable

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include "../../common/s32_formats.h"
#include "../../common/s32a_format.h"

// Configuration
#define MAX_INPUT_FILES 100
#define MAX_SECTIONS 1000
#define MAX_SYMBOLS 10000
#define MAX_RELOCATIONS 10000
#define STRING_TABLE_SIZE (64 * 1024)
#define MAX_LIB_PATHS 32

// Default memory layout
#define DEFAULT_CODE_BASE    0x00000000
#define DEFAULT_CODE_SIZE    0x00100000  // 1MB for code
#define DEFAULT_RODATA_SIZE  0x00100000  // 1MB for rodata
#define DEFAULT_DATA_SIZE    0x00100000  // 1MB for data
#define DEFAULT_STACK_SIZE   0x00010000  // 64KB stack
#define DEFAULT_STACK_BASE   0x0FFFFFF0
#define DEFAULT_MEM_SIZE     0x10000000  // 256MB total

// Compact memory layout (4KB pages)
#define COMPACT_PAGE_SIZE    0x00001000  // 4KB
#define COMPACT_STACK_SIZE   0x00001000  // 4KB stack

// Input file information
typedef struct {
    const char *filename;
    FILE *file;
    s32o_header_t header;
    s32o_section_t *sections;
    s32o_symbol_t *symbols;
    s32o_reloc_t *relocations;
    char *string_table;
    uint32_t section_base[MAX_SECTIONS];  // Where each section gets placed
    uint8_t **section_data;  // Section data for archive members (NULL for regular files)
} input_file_t;

// Combined section information
typedef struct {
    char name[64];
    uint32_t type;
    uint32_t flags;
    uint32_t vaddr;      // Virtual address in executable
    uint32_t size;       // Total size
    uint32_t align;      // Required alignment
    uint8_t *data;       // Combined section data
    
    // Track which parts come from which files
    struct {
        int file_idx;
        int section_idx;
        uint32_t offset;  // Offset within combined section
        uint32_t size;
    } parts[MAX_INPUT_FILES];
    int num_parts;
} combined_section_t;

// Symbol table entry
typedef struct {
    char name[256];
    uint32_t value;
    uint32_t size;
    uint8_t type;
    uint8_t binding;
    int defined_in_file;    // -1 if undefined
    int section_idx;        // Index in combined sections
    bool is_weak;
    bool is_used;
} symbol_entry_t;

// Relocation entry
typedef struct {
    int file_idx;
    int section_idx;        // Original section in file
    int combined_section;   // Combined section index
    uint32_t offset;        // Offset in original section
    uint32_t symbol_idx;    // Index in combined symbol table
    uint32_t type;
    int32_t addend;
} relocation_entry_t;

// Linker state
typedef struct {
    input_file_t input_files[MAX_INPUT_FILES];
    int num_input_files;
    
    combined_section_t sections[MAX_SECTIONS];
    int num_sections;
    
    symbol_entry_t symbols[MAX_SYMBOLS];
    int num_symbols;
    
    relocation_entry_t relocations[MAX_RELOCATIONS];
    int num_relocations;
    
    char string_table[STRING_TABLE_SIZE];
    uint32_t string_table_size;
    
    // Memory layout
    uint32_t entry_point;
    uint32_t code_base;
    uint32_t code_size;     // Configurable size
    uint32_t code_limit;    // Computed: code_base + code_size
    uint32_t rodata_base;
    uint32_t rodata_size;   // Configurable size
    uint32_t rodata_limit;  // Computed: rodata_base + rodata_size
    uint32_t data_base;
    uint32_t data_size;     // Configurable size
    uint32_t data_limit;    // Computed: data_base + data_size
    uint32_t bss_base;
    uint32_t bss_limit;
    uint32_t stack_base;
    uint32_t stack_size;    // Configurable stack size
    uint32_t heap_base;
    uint32_t mmio_base;
    uint32_t mmio_size;
    
    // Options
    bool verbose;
    bool strip_symbols;
    bool enable_wxorx;
    bool print_map;         // Generate memory map file
    bool pack_sections;     // Pack sections tightly
    bool compact_mode;      // Ultra-compact 4KB page mode
    const char *output_file;
    const char *entry_symbol;
    const char *map_file;   // Memory map output file
    
    // Library search paths
    const char *lib_paths[MAX_LIB_PATHS];
    int num_lib_paths;
} linker_state_t;

// String table management
static uint32_t add_string(linker_state_t *ld, const char *str) {
    if (!str || !*str) return 0;
    
    // Check if string already exists
    uint32_t offset = 1;
    while (offset < ld->string_table_size) {
        if (strcmp(&ld->string_table[offset], str) == 0) {
            return offset;
        }
        offset += strlen(&ld->string_table[offset]) + 1;
    }
    
    // Add new string
    uint32_t new_offset = ld->string_table_size;
    size_t len = strlen(str) + 1;
    if (new_offset + len > STRING_TABLE_SIZE) {
        fprintf(stderr, "Error: String table overflow\n");
        exit(1);
    }
    
    strcpy(&ld->string_table[new_offset], str);
    ld->string_table_size += len;
    return new_offset;
}

// Forward declaration
static bool load_object_file(linker_state_t *ld, const char *filename);

// Load an object file from memory (for archive members)
static bool load_object_from_memory(linker_state_t *ld, const char *filename, 
                                   uint8_t *data, size_t size) {
    if (ld->num_input_files >= MAX_INPUT_FILES) {
        fprintf(stderr, "Error: Too many input files\n");
        return false;
    }
    
    input_file_t *inf = &ld->input_files[ld->num_input_files];
    inf->filename = strdup(filename);
    inf->file = NULL;  // No file handle for archive members
    
    // Verify size
    if (size < sizeof(s32o_header_t)) {
        fprintf(stderr, "Error: Archive member '%s' too small\n", filename);
        return false;
    }
    
    // Copy header
    memcpy(&inf->header, data, sizeof(s32o_header_t));
    
    // Verify magic
    if (inf->header.magic != S32O_MAGIC) {
        fprintf(stderr, "Error: Archive member '%s' is not a valid .s32o file\n", filename);
        return false;
    }
    
    // Allocate and copy sections
    inf->sections = calloc(inf->header.nsections, sizeof(s32o_section_t));
    memcpy(inf->sections, data + inf->header.sec_offset, 
           inf->header.nsections * sizeof(s32o_section_t));
    
    // Allocate and copy symbols
    inf->symbols = calloc(inf->header.nsymbols, sizeof(s32o_symbol_t));
    memcpy(inf->symbols, data + inf->header.sym_offset,
           inf->header.nsymbols * sizeof(s32o_symbol_t));
    
    // Copy string table
    inf->string_table = malloc(inf->header.str_size);
    memcpy(inf->string_table, data + inf->header.str_offset, inf->header.str_size);
    
    // Store section data for archive members
    inf->section_data = calloc(inf->header.nsections, sizeof(uint8_t*));
    for (uint32_t i = 0; i < inf->header.nsections; i++) {
        if (inf->sections[i].size > 0 && inf->sections[i].offset > 0) {
            inf->section_data[i] = malloc(inf->sections[i].size);
            memcpy(inf->section_data[i], data + inf->sections[i].offset, inf->sections[i].size);
        }
    }
    
    // Count and copy relocations
    uint32_t total_relocs = 0;
    for (uint32_t i = 0; i < inf->header.nsections; i++) {
        total_relocs += inf->sections[i].nrelocs;
    }
    if (total_relocs > 0) {
        inf->relocations = calloc(total_relocs, sizeof(s32o_reloc_t));
        uint32_t reloc_idx = 0;
        for (uint32_t i = 0; i < inf->header.nsections; i++) {
            if (inf->sections[i].nrelocs > 0) {
                memcpy(&inf->relocations[reloc_idx], 
                       data + inf->sections[i].reloc_offset,
                       inf->sections[i].nrelocs * sizeof(s32o_reloc_t));
                reloc_idx += inf->sections[i].nrelocs;
            }
        }
    }
    
    if (ld->verbose) {
        printf("Loaded '%s' from archive: %d sections, %d symbols\n", 
               filename, inf->header.nsections, inf->header.nsymbols);
    }
    
    ld->num_input_files++;
    return true;
}

// Check if a symbol is undefined in the current link
static bool is_symbol_undefined(linker_state_t *ld, const char *symbol_name) {
    // Look through all undefined symbols in loaded objects
    for (int i = 0; i < ld->num_input_files; i++) {
        input_file_t *inf = &ld->input_files[i];
        for (uint32_t j = 0; j < inf->header.nsymbols; j++) {
            if (inf->symbols[j].binding == S32O_BIND_GLOBAL && 
                inf->symbols[j].section == 0) {
                const char *name = inf->string_table + inf->symbols[j].name_offset;
                if (strcmp(name, symbol_name) == 0) {
                    // Check if it's defined elsewhere
                    bool defined = false;
                    for (int k = 0; k < ld->num_input_files; k++) {
                        input_file_t *inf2 = &ld->input_files[k];
                        for (uint32_t l = 0; l < inf2->header.nsymbols; l++) {
                            if (inf2->symbols[l].binding == S32O_BIND_GLOBAL && 
                                inf2->symbols[l].section != 0) {
                                const char *name2 = inf2->string_table + inf2->symbols[l].name_offset;
                                if (strcmp(name2, symbol_name) == 0) {
                                    defined = true;
                                    break;
                                }
                            }
                        }
                        if (defined) break;
                    }
                    if (!defined) return true;
                }
            }
        }
    }
    return false;
}

// Find a library file using search paths
static char* find_library_file(linker_state_t *ld, const char *filename) {
    static char path_buffer[1024];
    
    // First try as-is (absolute path or relative to current dir)
    FILE *f = fopen(filename, "rb");
    if (f) {
        fclose(f);
        return (char*)filename;
    }
    
    // If filename starts with -l, convert to lib<name>.s32a
    char libname[256];
    if (strncmp(filename, "-l", 2) == 0) {
        snprintf(libname, sizeof(libname), "lib%s.s32a", filename + 2);
    } else {
        // Already a filename, just use it
        strncpy(libname, filename, sizeof(libname) - 1);
        libname[sizeof(libname) - 1] = '\0';
    }
    
    // Try each library search path
    for (int i = 0; i < ld->num_lib_paths; i++) {
        snprintf(path_buffer, sizeof(path_buffer), "%s/%s", ld->lib_paths[i], libname);
        f = fopen(path_buffer, "rb");
        if (f) {
            fclose(f);
            return path_buffer;
        }
    }
    
    // Not found
    return NULL;
}

// Load an archive file, extracting only needed members
static bool load_archive_file(linker_state_t *ld, const char *filename) {
    // Try to find the file using library search paths
    const char *actual_path = find_library_file(ld, filename);
    if (!actual_path) {
        fprintf(stderr, "Error: Cannot find library '%s'\n", filename);
        if (ld->num_lib_paths > 0) {
            fprintf(stderr, "Searched in:\n");
            for (int i = 0; i < ld->num_lib_paths; i++) {
                fprintf(stderr, "  %s\n", ld->lib_paths[i]);
            }
        }
        return false;
    }
    
    FILE *f = fopen(actual_path, "rb");
    if (!f) {
        fprintf(stderr, "Error: Cannot open '%s'\n", actual_path);
        return false;
    }
    
    // Read archive header
    s32a_header_t hdr;
    if (fread(&hdr, sizeof(hdr), 1, f) != 1) {
        fprintf(stderr, "Error: Cannot read archive header from '%s'\n", filename);
        fclose(f);
        return false;
    }
    
    // Verify magic
    if (hdr.magic != S32A_MAGIC) {
        // Not an archive, try as object file
        fclose(f);
        return load_object_file(ld, filename);
    }
    
    if (ld->verbose) {
        printf("Processing archive '%s': %d members, %d symbols\n", 
               filename, hdr.nmembers, hdr.nsymbols);
    }
    
    // Read string table
    char *strings = malloc(hdr.str_size);
    fseek(f, hdr.str_offset, SEEK_SET);
    fread(strings, 1, hdr.str_size, f);
    
    // Read symbol index
    s32a_symbol_t *symbols = malloc(hdr.nsymbols * sizeof(s32a_symbol_t));
    fseek(f, hdr.sym_offset, SEEK_SET);
    fread(symbols, sizeof(s32a_symbol_t), hdr.nsymbols, f);
    
    // Read member table
    s32a_member_t *members = malloc(hdr.nmembers * sizeof(s32a_member_t));
    fseek(f, hdr.mem_offset, SEEK_SET);
    fread(members, sizeof(s32a_member_t), hdr.nmembers, f);
    
    // Track which members we've loaded
    bool *loaded = calloc(hdr.nmembers, sizeof(bool));
    
    // Iteratively load members that resolve undefined symbols
    bool added_any;
    do {
        added_any = false;
        
        // Check each symbol in the index
        for (uint32_t i = 0; i < hdr.nsymbols; i++) {
            const char *sym_name = strings + symbols[i].name_offset;
            uint32_t member_idx = symbols[i].member_index;
            
            // Skip if we've already loaded this member
            if (loaded[member_idx]) continue;
            
            // Check if this symbol is undefined
            if (is_symbol_undefined(ld, sym_name)) {
                // Load this member
                s32a_member_t *member = &members[member_idx];
                const char *member_name = strings + member->name_offset;
                
                if (ld->verbose) {
                    printf("  Loading member '%s' for symbol '%s'\n", 
                           member_name, sym_name);
                }
                
                // Read member data
                uint8_t *data = malloc(member->size);
                fseek(f, member->offset, SEEK_SET);
                fread(data, 1, member->size, f);
                
                // Load the object
                char full_name[256];
                snprintf(full_name, sizeof(full_name), "%s(%s)", filename, member_name);
                if (!load_object_from_memory(ld, full_name, data, member->size)) {
                    free(data);
                    free(loaded);
                    free(members);
                    free(symbols);
                    free(strings);
                    fclose(f);
                    return false;
                }
                
                free(data);
                loaded[member_idx] = true;
                added_any = true;
            }
        }
    } while (added_any);  // Keep going until no new members added
    
    // Cleanup
    free(loaded);
    free(members);
    free(symbols);
    free(strings);
    fclose(f);
    
    return true;
}

// Load a single object file
static bool load_object_file(linker_state_t *ld, const char *filename) {
    if (ld->num_input_files >= MAX_INPUT_FILES) {
        fprintf(stderr, "Error: Too many input files\n");
        return false;
    }
    
    input_file_t *inf = &ld->input_files[ld->num_input_files];
    inf->filename = filename;
    
    inf->file = fopen(filename, "rb");
    if (!inf->file) {
        fprintf(stderr, "Error: Cannot open '%s'\n", filename);
        return false;
    }
    
    // Read header
    if (fread(&inf->header, sizeof(s32o_header_t), 1, inf->file) != 1) {
        fprintf(stderr, "Error: Cannot read header from '%s'\n", filename);
        fclose(inf->file);
        return false;
    }
    
    // Verify magic
    if (inf->header.magic != S32O_MAGIC) {
        fprintf(stderr, "Error: '%s' is not a valid .s32o file\n", filename);
        fclose(inf->file);
        return false;
    }
    
    // Allocate and read sections
    inf->sections = calloc(inf->header.nsections, sizeof(s32o_section_t));
    fseek(inf->file, inf->header.sec_offset, SEEK_SET);
    if (fread(inf->sections, sizeof(s32o_section_t), inf->header.nsections, inf->file) != inf->header.nsections) {
        fprintf(stderr, "Error: Failed to read sections from %s\n", filename);
        return false;
    }
    
    // Allocate and read symbols
    inf->symbols = calloc(inf->header.nsymbols, sizeof(s32o_symbol_t));
    fseek(inf->file, inf->header.sym_offset, SEEK_SET);
    if (fread(inf->symbols, sizeof(s32o_symbol_t), inf->header.nsymbols, inf->file) != inf->header.nsymbols) {
        fprintf(stderr, "Error: Failed to read symbols from %s\n", filename);
        return false;
    }
    
    // Read string table
    inf->string_table = malloc(inf->header.str_size);
    fseek(inf->file, inf->header.str_offset, SEEK_SET);
    if (fread(inf->string_table, 1, inf->header.str_size, inf->file) != inf->header.str_size) {
        fprintf(stderr, "Error: Failed to read string table from %s\n", filename);
        return false;
    }
    
    // For regular files, section data is read on demand
    inf->section_data = NULL;
    
    // Count total relocations and allocate
    uint32_t total_relocs = 0;
    for (uint32_t i = 0; i < inf->header.nsections; i++) {
        total_relocs += inf->sections[i].nrelocs;
    }
    if (total_relocs > 0) {
        inf->relocations = calloc(total_relocs, sizeof(s32o_reloc_t));
        uint32_t reloc_idx = 0;
        for (uint32_t i = 0; i < inf->header.nsections; i++) {
            if (inf->sections[i].nrelocs > 0) {
                fseek(inf->file, inf->sections[i].reloc_offset, SEEK_SET);
                if (fread(&inf->relocations[reloc_idx], sizeof(s32o_reloc_t), 
                          inf->sections[i].nrelocs, inf->file) != inf->sections[i].nrelocs) {
                    fprintf(stderr, "Error: Failed to read relocations from %s\n", filename);
                    return false;
                }
                reloc_idx += inf->sections[i].nrelocs;
            }
        }
    }
    
    if (ld->verbose) {
        printf("Loaded '%s': %d sections, %d symbols\n", 
               filename, inf->header.nsections, inf->header.nsymbols);
    }
    
    ld->num_input_files++;
    return true;
}

// Find or create a combined section
static int find_or_create_section(linker_state_t *ld, const char *name, 
                                   uint32_t type, uint32_t flags, uint32_t align) {
    // Look for existing section with same name
    for (int i = 0; i < ld->num_sections; i++) {
        if (strcmp(ld->sections[i].name, name) == 0) {
            // Verify compatible type and flags
            if (ld->sections[i].type != type) {
                fprintf(stderr, "Error: Section '%s' has conflicting types\n", name);
                exit(1);
            }
            // Update alignment to maximum required
            if (align > ld->sections[i].align) {
                ld->sections[i].align = align;
            }
            return i;
        }
    }
    
    // Create new section
    if (ld->num_sections >= MAX_SECTIONS) {
        fprintf(stderr, "Error: Too many sections\n");
        exit(1);
    }
    
    combined_section_t *sec = &ld->sections[ld->num_sections];
    strncpy(sec->name, name, sizeof(sec->name) - 1);
    sec->type = type;
    sec->flags = flags;
    sec->align = align;
    sec->size = 0;
    sec->num_parts = 0;
    sec->data = NULL;
    
    return ld->num_sections++;
}

// Merge sections from all input files
static void merge_sections(linker_state_t *ld) {
    for (int f = 0; f < ld->num_input_files; f++) {
        input_file_t *inf = &ld->input_files[f];
        
        for (uint32_t s = 0; s < inf->header.nsections; s++) {
            s32o_section_t *isec = &inf->sections[s];
            
            // Skip empty sections
            if (isec->size == 0 && isec->type != S32_SEC_BSS) continue;
            
            const char *name = &inf->string_table[isec->name_offset];
            int combined_idx = find_or_create_section(ld, name, isec->type, 
                                                       isec->flags, isec->align);
            combined_section_t *csec = &ld->sections[combined_idx];
            
            // Align offset for this part
            uint32_t aligned_offset = csec->size;
            if (isec->align > 1) {
                aligned_offset = (aligned_offset + isec->align - 1) & ~(isec->align - 1);
            }
            
            // Record where this input section goes
            inf->section_base[s] = aligned_offset;
            
            // Add to combined section
            if (csec->num_parts < MAX_INPUT_FILES) {
                csec->parts[csec->num_parts].file_idx = f;
                csec->parts[csec->num_parts].section_idx = s;
                csec->parts[csec->num_parts].offset = aligned_offset;
                csec->parts[csec->num_parts].size = isec->size;
                csec->num_parts++;
            }
            
            // Update size
            csec->size = aligned_offset + isec->size;
        }
    }
    
    if (ld->verbose) {
        printf("Merged into %d sections:\n", ld->num_sections);
        for (int i = 0; i < ld->num_sections; i++) {
            printf("  %-10s: %d bytes from %d files\n", 
                   ld->sections[i].name, ld->sections[i].size, 
                   ld->sections[i].num_parts);
        }
    }
}

// Forward declarations for helper functions
static bool symbol_referenced_in_file(const input_file_t *inf, uint32_t sym_index);
static int find_combined_section_index_by_name(combined_section_t *sections, int nsecs, const char *name);
static int find_reloc_at(linker_state_t *ld, int file_idx, int section_idx, uint32_t offset);

// Build combined symbol table
static void build_symbol_table(linker_state_t *ld) {
    // First pass: collect all symbols
    for (int f = 0; f < ld->num_input_files; f++) {
        input_file_t *inf = &ld->input_files[f];
        
        for (uint32_t s = 0; s < inf->header.nsymbols; s++) {
            s32o_symbol_t *isym = &inf->symbols[s];
            const char *name = &inf->string_table[isym->name_offset];
            
            // Keep LOCAL symbols if they are referenced by any relocation, even with -s
            if (isym->binding == S32O_BIND_LOCAL && ld->strip_symbols) {
                if (!symbol_referenced_in_file(inf, s)) {
                    continue;
                }
            }
            
            // Look for existing symbol
            int existing = -1;
            for (int i = 0; i < ld->num_symbols; i++) {
                if (strcmp(ld->symbols[i].name, name) == 0) {
                    existing = i;
                    break;
                }
            }
            
            if (existing >= 0) {
                // Symbol already exists - check for conflicts
                symbol_entry_t *esym = &ld->symbols[existing];
                
                if (isym->section != 0) {  // This is a definition
                    if (esym->defined_in_file >= 0 && !esym->is_weak) {
                        // Multiple definitions
                        if (isym->binding != S32O_BIND_WEAK) {
                            fprintf(stderr, "Error: Multiple definitions of '%s'\n", name);
                            fprintf(stderr, "  First defined in %s\n", 
                                    ld->input_files[esym->defined_in_file].filename);
                            fprintf(stderr, "  Also defined in %s\n", inf->filename);
                            exit(1);
                        }
                        // This is weak, existing is strong - keep existing
                    } else {
                        // Update with this definition
                        if (ld->verbose) {
                            printf("Updating existing symbol '%s': isym->value=0x%08X section_base[%d]=0x%08X -> combined=0x%08X\n",
                                   name, isym->value, isym->section - 1, inf->section_base[isym->section - 1],
                                   isym->value + inf->section_base[isym->section - 1]);
                        }
                        esym->defined_in_file = f;
                        esym->value = isym->value + inf->section_base[isym->section - 1];
                        esym->size = isym->size;
                        esym->type = isym->type;
                        esym->is_weak = (isym->binding == S32O_BIND_WEAK);
                        
                        // Find combined section
                        if (isym->section > 0 && isym->section <= inf->header.nsections) {
                            const char *sec_name = &inf->string_table[
                                inf->sections[isym->section - 1].name_offset];
                            for (int cs = 0; cs < ld->num_sections; cs++) {
                                if (strcmp(ld->sections[cs].name, sec_name) == 0) {
                                    esym->section_idx = cs;
                                    break;
                                }
                            }
                        }
                    }
                }
            } else {
                // New symbol
                if (ld->num_symbols >= MAX_SYMBOLS) {
                    fprintf(stderr, "Error: Too many symbols\n");
                    exit(1);
                }
                
                symbol_entry_t *nsym = &ld->symbols[ld->num_symbols];
                
                // For local symbols, create unique name to prevent collisions
                if (isym->binding == S32O_BIND_LOCAL) {
                    snprintf(nsym->name, sizeof(nsym->name) - 1, "%s@%d", name, f);
                } else {
                    strncpy(nsym->name, name, sizeof(nsym->name) - 1);
                }
                nsym->type = isym->type;
                nsym->binding = isym->binding;
                nsym->size = isym->size;
                nsym->is_weak = (isym->binding == S32O_BIND_WEAK);
                
                if (isym->section == 0) {
                    // Undefined symbol
                    nsym->defined_in_file = -1;
                    nsym->value = 0;
                    nsym->section_idx = -1;
                } else {
                    // Defined symbol
                    nsym->defined_in_file = f;
                    if (ld->verbose) {
                        printf("Adding symbol '%s': isym->value=0x%08X section_base[%d]=0x%08X -> combined=0x%08X\n",
                               name, isym->value, isym->section - 1, inf->section_base[isym->section - 1],
                               isym->value + inf->section_base[isym->section - 1]);
                    }
                    nsym->value = isym->value + inf->section_base[isym->section - 1];
                    
                    // Find combined section
                    if (isym->section <= inf->header.nsections) {
                        const char *sec_name = &inf->string_table[
                            inf->sections[isym->section - 1].name_offset];
                        if (ld->verbose) {
                            printf("  Symbol section %d -> section name '%s'\n", isym->section, sec_name);
                        }
                        for (int cs = 0; cs < ld->num_sections; cs++) {
                            if (strcmp(ld->sections[cs].name, sec_name) == 0) {
                                nsym->section_idx = cs;
                                if (ld->verbose) {
                                    printf("  -> combined section %d\n", cs);
                                }
                                break;
                            }
                        }
                    }
                }
                
                ld->num_symbols++;
            }
        }
    }
    
    if (ld->verbose) {
        printf("Symbol table: %d symbols\n", ld->num_symbols);
        int undefined = 0;
        for (int i = 0; i < ld->num_symbols; i++) {
            if (ld->symbols[i].defined_in_file < 0) undefined++;
        }
        if (undefined > 0) {
            printf("  Warning: %d undefined symbols\n", undefined);
        }
    }
}

// Assign memory addresses to sections
static void layout_sections(linker_state_t *ld) {
    // First, calculate actual sizes needed
    uint32_t code_size_needed = 0;
    uint32_t rodata_size_needed = 0;
    uint32_t data_size_needed = 0;
    uint32_t bss_size_needed = 0;
    
    for (int i = 0; i < ld->num_sections; i++) {
        combined_section_t *sec = &ld->sections[i];
        switch (sec->type) {
            case S32_SEC_CODE:
                code_size_needed += sec->size;
                break;
            case S32_SEC_RODATA:
                rodata_size_needed += sec->size;
                break;
            case S32_SEC_DATA:
                data_size_needed += sec->size;
                break;
            case S32_SEC_BSS:
                bss_size_needed += sec->size;
                break;
        }
    }
    
    // Automatic compact layout if sizes are small enough
    bool auto_compact = false;
    if (ld->compact_mode || 
        (code_size_needed <= COMPACT_PAGE_SIZE && 
         rodata_size_needed <= COMPACT_PAGE_SIZE && 
         data_size_needed + bss_size_needed <= COMPACT_PAGE_SIZE)) {
        auto_compact = true;
    }
    
    if (auto_compact || ld->pack_sections) {
        // Automatic compact/packed layout
        uint32_t current_addr = ld->code_base;
        
        // Place code at base
        ld->code_base = current_addr;
        
        // Layout code sections
        for (int i = 0; i < ld->num_sections; i++) {
            if (ld->sections[i].type == S32_SEC_CODE) {
                current_addr += ld->sections[i].size;
            }
        }
        
        // Align to page boundary for protection
        current_addr = (current_addr + 0xFFF) & ~0xFFF;
        ld->code_limit = current_addr;
        
        // Place rodata after code
        ld->rodata_base = current_addr;
        for (int i = 0; i < ld->num_sections; i++) {
            if (ld->sections[i].type == S32_SEC_RODATA) {
                current_addr += ld->sections[i].size;
            }
        }
        
        // Align to page boundary for protection
        current_addr = (current_addr + 0xFFF) & ~0xFFF;
        ld->rodata_limit = current_addr;
        
        // Place data/bss after rodata
        ld->data_base = current_addr;
        for (int i = 0; i < ld->num_sections; i++) {
            if (ld->sections[i].type == S32_SEC_DATA || 
                ld->sections[i].type == S32_SEC_BSS) {
                current_addr += ld->sections[i].size;
            }
        }
        
        // Align to page boundary
        current_addr = (current_addr + 0xFFF) & ~0xFFF;
        ld->data_limit = current_addr;
        
        // Set heap and stack
        ld->heap_base = current_addr;
        
        // If stack size explicitly set or in compact mode, place stack close
        if (ld->stack_size != DEFAULT_STACK_SIZE || auto_compact) {
            // Place stack after a small heap gap
            uint32_t heap_gap = auto_compact ? COMPACT_PAGE_SIZE : 0x10000; // 4KB or 64KB heap
            ld->stack_base = current_addr + heap_gap + ld->stack_size;
            ld->stack_base = (ld->stack_base + 0xF) & ~0xF; // 16-byte align
        }
        // Otherwise use default high stack
        
        if (ld->verbose && auto_compact) {
            printf("Using automatic compact layout (total: %u bytes)\n", current_addr);
        }
    } else {
        // Traditional fixed-size layout
        ld->code_limit = ld->code_base + ld->code_size;
        ld->rodata_base = ld->code_limit;
        ld->rodata_limit = ld->rodata_base + ld->rodata_size;
        ld->data_base = ld->rodata_limit;
        ld->data_limit = ld->data_base + ld->data_size;
    }
    
    uint32_t code_addr = ld->code_base;
    uint32_t rodata_addr = ld->rodata_base;
    uint32_t data_addr = ld->data_base;
    uint32_t bss_addr = ld->data_base;  // BSS follows data
    
    // First pass: assign addresses based on section type
    for (int i = 0; i < ld->num_sections; i++) {
        combined_section_t *sec = &ld->sections[i];
        
        if (ld->verbose) {
            printf("Assigning vaddr to section '%s' (idx=%d)\n", sec->name, i);
        }
        
        switch (sec->type) {
            case S32_SEC_CODE:
                code_addr = (code_addr + sec->align - 1) & ~(sec->align - 1);
                sec->vaddr = code_addr;
                code_addr += sec->size;
                break;
                
            case S32_SEC_RODATA:
                rodata_addr = (rodata_addr + sec->align - 1) & ~(sec->align - 1);
                sec->vaddr = rodata_addr;
                rodata_addr += sec->size;
                break;
                
            case S32_SEC_DATA:
                data_addr = (data_addr + sec->align - 1) & ~(sec->align - 1);
                sec->vaddr = data_addr;
                data_addr += sec->size;
                if (data_addr > bss_addr) bss_addr = data_addr;
                break;
                
            case S32_SEC_BSS:
                bss_addr = (bss_addr + sec->align - 1) & ~(sec->align - 1);
                sec->vaddr = bss_addr;
                bss_addr += sec->size;
                break;
                
            default:
                // Other sections go after BSS
                bss_addr = (bss_addr + sec->align - 1) & ~(sec->align - 1);
                sec->vaddr = bss_addr;
                bss_addr += sec->size;
                break;
        }
    }
    
    if (ld->verbose) {
        printf("After layout_sections, symbol values:\n");
        for (int i = 0; i < ld->num_symbols; i++) {
            printf("  Symbol '%s': value=0x%08X section_idx=%d\n", 
                   ld->symbols[i].name, ld->symbols[i].value, ld->symbols[i].section_idx);
        }
    }
    
    // Record actual end addresses
    ld->bss_limit = bss_addr;
    
    // Check for overflow if not in automatic mode
    if (!ld->pack_sections && !ld->compact_mode) {
        if (code_addr > ld->code_limit) {
            fprintf(stderr, "Error: Code section overflow (0x%X > 0x%X)\n", code_addr, ld->code_limit);
            fprintf(stderr, "Hint: Use --code-size to increase limit or --pack-sections for automatic layout\n");
            exit(1);
        }
        if (rodata_addr > ld->rodata_limit) {
            fprintf(stderr, "Error: Rodata section overflow (0x%X > 0x%X)\n", rodata_addr, ld->rodata_limit);
            fprintf(stderr, "Hint: Use --rodata-size to increase limit or --pack-sections for automatic layout\n");
            exit(1);
        }
        if (bss_addr > ld->data_limit) {
            fprintf(stderr, "Error: Data/BSS section overflow (0x%X > 0x%X)\n", bss_addr, ld->data_limit);
            fprintf(stderr, "Hint: Use --data-size to increase limit or --pack-sections for automatic layout\n");
            exit(1);
        }
    }
    
    // Set heap base after BSS
    ld->heap_base = (ld->bss_limit + 0xFFF) & ~0xFFF;  // Page align
    
    // Set MMIO region after heap (if configured)
    if (ld->mmio_size > 0) {
        // Leave some space for heap growth
        ld->mmio_base = ld->heap_base + 0x100000;  // 1MB heap space by default
        ld->mmio_base = (ld->mmio_base + 0xFFF) & ~0xFFF;  // Page align
    } else {
        ld->mmio_base = 0;
    }
    
    if (ld->verbose) {
        printf("Memory layout:\n");
        printf("  Code:   0x%08X - 0x%08X\n", ld->code_base, ld->code_limit);
        printf("  ROData: 0x%08X - 0x%08X\n", ld->rodata_base, ld->rodata_limit);
        printf("  Data:   0x%08X - 0x%08X\n", ld->data_base, ld->data_limit);
        printf("  BSS:    0x%08X - 0x%08X\n", ld->data_limit, ld->bss_limit);
        printf("  Heap:   0x%08X\n", ld->heap_base);
        if (ld->mmio_size > 0) {
            printf("  MMIO:   0x%08X - 0x%08X (%u bytes)\n", 
                   ld->mmio_base, ld->mmio_base + ld->mmio_size, ld->mmio_size);
        }
        printf("  Stack:  0x%08X\n", ld->stack_base);
    }
}

// Inject memory map symbols for runtime use
static void inject_memory_map_symbols(linker_state_t *ld) {
    // Add special symbols that programs can reference
    struct {
        const char *name;
        uint32_t value;
    } special_symbols[] = {
        {"__code_start", ld->code_base},
        {"__code_end", ld->code_limit},
        {"__rodata_start", ld->rodata_base},
        {"__rodata_end", ld->rodata_limit},
        {"__data_start", ld->data_base},
        {"__data_end", ld->data_limit},
        {"__bss_start", ld->data_limit},  // BSS starts where data ends
        {"__bss_end", ld->bss_limit},
        {"__heap_start", ld->heap_base},
        {"__heap_end", ld->mmio_size > 0 ? ld->mmio_base : ld->stack_base},    // Heap ends at MMIO or stack if no MMIO
        {"__mmio_base", ld->mmio_base},
        {"__mmio_end", ld->mmio_base + ld->mmio_size},
        {"__stack_base", ld->stack_base},
        {"__stack_top", ld->stack_base},  // Stack grows down
        {NULL, 0}
    };
    
    for (int i = 0; special_symbols[i].name != NULL; i++) {
        // Check if symbol already exists (user might have defined it)
        bool found = false;
        for (int s = 0; s < ld->num_symbols; s++) {
            if (strcmp(ld->symbols[s].name, special_symbols[i].name) == 0) {
                // Update existing symbol if it was undefined
                if (ld->symbols[s].defined_in_file < 0) {
                    ld->symbols[s].value = special_symbols[i].value;
                    ld->symbols[s].defined_in_file = MAX_INPUT_FILES;  // Special marker for linker-defined
                    ld->symbols[s].type = 0;  // NOTYPE
                    ld->symbols[s].binding = 1;  // GLOBAL
                }
                found = true;
                break;
            }
        }
        
        // Add new symbol if not found
        if (!found && ld->num_symbols < MAX_SYMBOLS) {
            symbol_entry_t *sym = &ld->symbols[ld->num_symbols++];
            strcpy(sym->name, special_symbols[i].name);
            sym->value = special_symbols[i].value;
            sym->size = 0;
            sym->type = 0;  // NOTYPE
            sym->binding = 1;  // GLOBAL
            sym->defined_in_file = MAX_INPUT_FILES;  // Linker-defined
            sym->section_idx = -1;
            sym->is_weak = false;
            sym->is_used = false;  // Will be set true if referenced
        }
    }
    
    if (ld->verbose) {
        printf("Injected memory map symbols\n");
    }
}

// Collect all relocations
static void collect_relocations(linker_state_t *ld) {
    for (int f = 0; f < ld->num_input_files; f++) {
        input_file_t *inf = &ld->input_files[f];
        uint32_t reloc_idx = 0;
        
        for (uint32_t s = 0; s < inf->header.nsections; s++) {
            s32o_section_t *isec = &inf->sections[s];
            
            for (uint32_t r = 0; r < isec->nrelocs; r++) {
                if (ld->num_relocations >= MAX_RELOCATIONS) {
                    fprintf(stderr, "Error: Too many relocations\n");
                    exit(1);
                }
                
                relocation_entry_t *rel = &ld->relocations[ld->num_relocations];
                s32o_reloc_t *irel = &inf->relocations[reloc_idx++];
                
                rel->file_idx = f;
                rel->section_idx = s;
                rel->offset = irel->offset;
                rel->type = irel->type;
                rel->addend = irel->addend;
                
                // Find symbol in combined table
                rel->symbol_idx = UINT32_MAX; // sentinel
                if (irel->symbol < inf->header.nsymbols) {
                    const char *sym_name = &inf->string_table[
                        inf->symbols[irel->symbol].name_offset];
                    
                    // For local symbols, create the same unique name used in build_symbol_table
                    char lookup_name[256];
                    if (inf->symbols[irel->symbol].binding == S32O_BIND_LOCAL) {
                        snprintf(lookup_name, sizeof(lookup_name), "%s@%d", sym_name, f);
                    } else {
                        strncpy(lookup_name, sym_name, sizeof(lookup_name) - 1);
                        lookup_name[sizeof(lookup_name) - 1] = '\0';
                    }
                    
                    for (int cs = 0; cs < ld->num_symbols; cs++) {
                        if (strcmp(ld->symbols[cs].name, lookup_name) == 0) {
                            rel->symbol_idx = cs;
                            ld->symbols[cs].is_used = true;
                            break;
                        }
                    }
                    
                    // If not found (e.g., stripped local), synthesize a combined symbol
                    if (rel->symbol_idx == UINT32_MAX) {
                        if (ld->num_symbols >= MAX_SYMBOLS) {
                            fprintf(stderr, "Error: Too many symbols (while synthesizing '%s')\n", sym_name);
                            exit(1);
                        }
                        s32o_symbol_t *isym = &inf->symbols[irel->symbol];
                        symbol_entry_t *nsym = &ld->symbols[ld->num_symbols];
                        snprintf(nsym->name, sizeof(nsym->name), "%s@%d", sym_name, f);
                        nsym->binding = isym->binding;   // likely LOCAL
                        nsym->type = isym->type;
                        nsym->size = isym->size;
                        nsym->is_weak = (isym->binding == S32O_BIND_WEAK);
                        if (isym->section == 0) {
                            // undefined
                            nsym->defined_in_file = -1;
                            nsym->value = 0;
                            nsym->section_idx = -1;
                        } else {
                            nsym->defined_in_file = f;
                            nsym->value = isym->value + inf->section_base[isym->section - 1];
                            const char *sec_name = &inf->string_table[inf->sections[isym->section - 1].name_offset];
                            nsym->section_idx = find_combined_section_index_by_name(ld->sections, ld->num_sections, sec_name);
                        }
                        rel->symbol_idx = ld->num_symbols++;
                    }
                }
                if (rel->symbol_idx == UINT32_MAX) {
                    fprintf(stderr, "Error: relocation in %s refers to unknown symbol idx=%u ('%s')\n",
                            inf->filename, irel->symbol,
                            (irel->symbol < inf->header.nsymbols)
                                ? &inf->string_table[inf->symbols[irel->symbol].name_offset]
                                : "<out-of-range>");
                    exit(1);
                }
                
                // Find combined section
                const char *sec_name = &inf->string_table[isec->name_offset];
                rel->combined_section = -1;
                for (int cs = 0; cs < ld->num_sections; cs++) {
                    if (strcmp(ld->sections[cs].name, sec_name) == 0) {
                        rel->combined_section = cs;
                        break;
                    }
                }
                if (rel->combined_section < 0) {
                    fprintf(stderr, "Error: could not map input section '%s' to a combined section (file %s)\n",
                            sec_name, inf->filename);
                    exit(1);
                }
                
                ld->num_relocations++;
            }
        }
    }
    
    if (ld->verbose) {
        printf("Collected %d relocations\n", ld->num_relocations);
    }
}

// Load section data
static void load_section_data(linker_state_t *ld) {
    for (int i = 0; i < ld->num_sections; i++) {
        combined_section_t *sec = &ld->sections[i];
        
        // BSS has no data
        if (sec->type == S32_SEC_BSS) continue;
        
        // Allocate data buffer
        sec->data = calloc(1, sec->size);
        if (!sec->data) {
            fprintf(stderr, "Error: Out of memory for section '%s'\n", sec->name);
            exit(1);
        }
        
        // Load data from each contributing file
        for (int p = 0; p < sec->num_parts; p++) {
            input_file_t *inf = &ld->input_files[sec->parts[p].file_idx];
            s32o_section_t *isec = &inf->sections[sec->parts[p].section_idx];
            
            if (isec->size > 0 && isec->offset > 0) {
                if (inf->section_data != NULL) {
                    // Archive member - data already loaded
                    memcpy(sec->data + sec->parts[p].offset, 
                           inf->section_data[sec->parts[p].section_idx], 
                           isec->size);
                } else {
                    // Regular file - read from disk
                    fseek(inf->file, isec->offset, SEEK_SET);
                    if (fread(sec->data + sec->parts[p].offset, 1, isec->size, inf->file) != isec->size) {
                        fprintf(stderr, "Error: Failed to read section data from %s\n", inf->filename);
                        exit(1);
                    }
                }
            }
        }
    }
}

// Update symbol values after sections have been assigned addresses
static void update_symbol_values(linker_state_t *ld) {
    if (ld->verbose) {
        printf("Before updating symbol values:\n");
        for (int i = 0; i < ld->num_symbols; i++) {
            printf("  Symbol '%s': value=0x%08X section_idx=%d\n",
                   ld->symbols[i].name, ld->symbols[i].value, ld->symbols[i].section_idx);
        }
    }
    for (int i = 0; i < ld->num_symbols; i++) {
        symbol_entry_t *sym = &ld->symbols[i];
        if (sym->defined_in_file >= 0 && sym->section_idx >= 0) {
            // Symbol value is currently section-relative, convert to absolute
            combined_section_t *sec = &ld->sections[sym->section_idx];
            uint32_t old_value = sym->value;
            sym->value = sec->vaddr + sym->value;
            if (ld->verbose) {
                printf("Updating symbol '%s': section='%s' relative=0x%08X -> absolute=0x%08X\n",
                       sym->name, sec->name, old_value, sym->value);
            }
        }
    }
}

// Apply relocations
static void apply_relocations(linker_state_t *ld) {
    int unresolved = 0;
    
    for (int i = 0; i < ld->num_relocations; i++) {
        relocation_entry_t *rel = &ld->relocations[i];
        symbol_entry_t *sym = &ld->symbols[rel->symbol_idx];
        
        // Check if symbol is resolved
        if (sym->defined_in_file < 0) {
            fprintf(stderr, "Error: Undefined symbol '%s'\n", sym->name);
            unresolved++;
            continue;
        }
        
        // Find section data
        combined_section_t *sec = &ld->sections[rel->combined_section];
        if (!sec->data) continue;  // BSS or other non-data section
        
        // Calculate relocation offset in combined section
        input_file_t *inf = &ld->input_files[rel->file_idx];
        uint32_t combined_offset = inf->section_base[rel->section_idx] + rel->offset;
        
        if (combined_offset + 4 > sec->size) {
            fprintf(stderr, "Error: Relocation offset out of bounds\n");
            continue;
        }
        
        uint32_t *target = (uint32_t *)(sec->data + combined_offset);
        uint32_t pc = sec->vaddr + combined_offset;
        uint32_t value = sym->value + rel->addend;
        
        switch (rel->type) {
            case S32O_REL_32:
                // Direct 32-bit reference
                *target = value;
                break;
                
            case S32O_REL_HI20:
                // High 20 bits for LUI (RISC-V style with bit 11 adjustment)
                {
                    uint32_t lo12 = value & 0xFFF;
                    uint32_t hi20 = (value >> 12) & 0xFFFFF;
                    
                    // If bit 11 of lo12 is set, ADDI will sign-extend it
                    // We need to increment hi20 to compensate
                    if (lo12 & 0x800) {
                        hi20 = (hi20 + 1) & 0xFFFFF;
                    }
                    
                    if (ld->verbose) {
                        printf("Applying HI20 relocation: symbol=%s value=0x%08X hi20=0x%05X lo12=0x%03X at offset=0x%08X\n", 
                               sym->name, value, hi20, lo12, combined_offset);
                    }
                    *target = (*target & 0xFFF) | (hi20 << 12);
                }
                break;
                
            case S32O_REL_LO12:
                // Low 12 bits for ADDI or load/store instructions
                {
                    uint32_t opcode = *target & 0x7F;
                    uint32_t imm = value & 0xFFF;  // Just take the lower 12 bits
                    
                    // Check if paired with a HI20 to same symbol anywhere in the same section
                    // HI20 and LO12 pairs don't have to be adjacent
                    int paired = 0;
                    for (int i = 0; i < ld->num_relocations; i++) {
                        relocation_entry_t *other = &ld->relocations[i];
                        if (other->type == S32O_REL_HI20 && 
                            other->symbol_idx == rel->symbol_idx &&
                            other->file_idx == rel->file_idx &&
                            other->section_idx == rel->section_idx) {
                            paired = 1;
                            break;
                        }
                    }
                    if (!paired) {
                        int32_t sval = (int32_t)value;
                        if (sval < -2048 || sval > 2047) {
                            fprintf(stderr, "Error: LO12 relocation appears unpaired and value 0x%08X out of ±2048 at PC=0x%08X\n",
                                    value, pc);
                            continue;
                        }
                    }
                    
                    if (ld->verbose) {
                        printf("Applying LO12 relocation: symbol=%s value=0x%08X imm=0x%03X opcode=0x%02X at offset=0x%08X\n", 
                               sym->name, value, imm, opcode, combined_offset);
                    }
                    
                    // Check if it's a store instruction (S-type)
                    // SLOW-32 store opcodes: STB=0x38, STH=0x39, STW=0x3A
                    if (opcode == 0x38 || opcode == 0x39 || opcode == 0x3A) {
                        // S-type: imm[11:5] at bits 31:25, imm[4:0] at bits 11:7
                        *target = (*target & 0xFE000F80)
                                | ((imm & 0x1F) << 7)
                                | (((imm >> 5) & 0x7F) << 25);
                    } else {
                        // I-type: imm[11:0] at bits 31:20
                        *target = (*target & 0x000FFFFF) | (imm << 20);
                    }
                }
                break;
                
            case S32O_REL_BRANCH:
                // B-format: PC+4 relative branch
                {
                    int32_t off = value - (pc + 4);  // Branches are PC+4
                    if (off & 1) {
                        fprintf(stderr, "Error: Branch target misaligned (odd offset) at 0x%08X\n", pc);
                        continue;
                    }
                    if (off < -4096 || off > 4094) { // even offsets only (LSB dropped)
                        fprintf(stderr, "Error: Branch offset out of range at 0x%08X\n", pc);
                        continue;
                    }
                    // B-format bit packing: imm[12|10:5] at 31:25, imm[4:1|11] at 11:7
                    uint32_t imm12   = (off >> 12) & 1;
                    uint32_t imm11   = (off >> 11) & 1;
                    uint32_t imm10_5 = (off >> 5)  & 0x3F;
                    uint32_t imm4_1  = (off >> 1)  & 0xF;
                    
                    *target = (*target & 0x01FFF07F)  // Keep rd/op/rs* fields
                            | (imm11   << 7)
                            | (imm4_1  << 8)
                            | (imm10_5 << 25)
                            | (imm12   << 31);
                }
                break;
                
            case S32O_REL_JAL:
                // J-format: PC relative (not PC+4)
                {
                    int32_t off = value - pc;  // JAL is PC-relative (not PC+4)
                    if (off & 1) {
                        fprintf(stderr, "Error: JAL target misaligned (odd offset) at 0x%08X\n", pc);
                        continue;
                    }
                    if (off < -1048576 || off > 1048574) { // even offsets only
                        fprintf(stderr, "Error: JAL offset out of range at 0x%08X\n", pc);
                        continue;
                    }
                    // J-format bit packing: imm[20|10:1|11|19:12] at 31:12
                    uint32_t imm20    = (off >> 20) & 1;
                    uint32_t imm19_12 = (off >> 12) & 0xFF;
                    uint32_t imm11    = (off >> 11) & 1;
                    uint32_t imm10_1  = (off >> 1)  & 0x3FF;
                    
                    *target = (*target & 0x00000FFF)  // Keep rd/opcode
                            | (imm19_12 << 12)
                            | (imm11    << 20)
                            | (imm10_1  << 21)
                            | (imm20    << 31);
                }
                break;
                
            case S32O_REL_CALL:
                // Treat like a JAL to 'value' (PC-relative). If out of range,
                // the assembler should have emitted an LUI/ADDI+JALR pair instead.
                {
                    int32_t off = value - pc;
                    if (off & 1) {
                        fprintf(stderr, "Error: CALL target misaligned (odd offset) at 0x%08X\n", pc);
                        continue;
                    }
                    if (off < -1048576 || off > 1048574) {
                        fprintf(stderr, "Error: CALL offset out of range at 0x%08X (use HI20/LO12+JALR sequence)\n", pc);
                        continue;
                    }
                    uint32_t imm20    = (off >> 20) & 1;
                    uint32_t imm19_12 = (off >> 12) & 0xFF;
                    uint32_t imm11    = (off >> 11) & 1;
                    uint32_t imm10_1  = (off >> 1)  & 0x3FF;
                    *target = (*target & 0x00000FFF)
                            | (imm19_12 << 12)
                            | (imm11    << 20)
                            | (imm10_1  << 21)
                            | (imm20    << 31);
                }
                break;
                
            case S32O_REL_PCREL_HI20:
                // PC-relative high 20 bits (for AUIPC)
                {
                    // Calculate PC-relative offset (pc already defined above)
                    int32_t offset = (int32_t)(value - pc);
                    
                    // Extract high 20 bits with bit 11 adjustment (same as HI20)
                    uint32_t lo12 = offset & 0xFFF;
                    uint32_t hi20 = (offset >> 12) & 0xFFFFF;
                    
                    // If bit 11 of lo12 is set, increment hi20
                    if (lo12 & 0x800) {
                        hi20 = (hi20 + 1) & 0xFFFFF;
                    }
                    
                    // Clear upper 20 bits and set new value
                    *target = (*target & 0xFFF) | (hi20 << 12);
                    
                    if (ld->verbose) {
                        printf("  PCREL_HI20: PC=0x%x, target=0x%x, offset=0x%x, hi20=0x%x\n",
                               pc, value, offset, hi20);
                    }
                }
                break;
                
            case S32O_REL_PCREL_LO12:
                // PC-relative low 12 bits (paired with PCREL_HI20)
                // This is tricky: it needs to reference the address of the PCREL_HI20
                // For now, we'll use the symbol value directly (may need adjustment)
                {
                    uint32_t imm = value & 0xFFF;
                    uint32_t opcode = *target & 0x7F;
                    
                    // Different instruction formats need different handling
                    if (opcode == 0x10 || opcode == 0x11 || opcode == 0x12 || opcode == 0x1E ||
                        opcode == 0x13 || opcode == 0x14 || opcode == 0x15) {
                        // I-type: ADDI, ORI, ANDI, XORI, etc.
                        *target = (*target & 0xFFFFF) | (imm << 20);
                    } else if (opcode >= 0x30 && opcode <= 0x34) {
                        // Load instructions (I-type format)
                        *target = (*target & 0xFFFFF) | (imm << 20);
                    } else if (opcode >= 0x38 && opcode <= 0x3A) {
                        // Store instructions (S-type format)
                        uint32_t imm11_5 = (imm >> 5) & 0x7F;
                        uint32_t imm4_0 = imm & 0x1F;
                        *target = (*target & 0xFE000F80) | (imm11_5 << 25) | (imm4_0 << 7);
                    }
                    
                    if (ld->verbose) {
                        printf("  PCREL_LO12: value=0x%x, imm=0x%x\n", value, imm);
                    }
                }
                break;
                
            default:
                fprintf(stderr, "Warning: Unknown relocation type %d\n", rel->type);
                break;
        }
    }
    
    if (unresolved > 0) {
        fprintf(stderr, "Error: %d unresolved symbols\n", unresolved);
        exit(1);
    }
}

// Find entry point
static uint32_t find_entry_point(linker_state_t *ld) {
    // Look for entry symbol (default: _start)
    const char *entry_name = ld->entry_symbol ? ld->entry_symbol : "_start";
    
    for (int i = 0; i < ld->num_symbols; i++) {
        if (strcmp(ld->symbols[i].name, entry_name) == 0) {
            if (ld->symbols[i].defined_in_file < 0) {
                fprintf(stderr, "Error: Entry point '%s' is undefined\n", entry_name);
                exit(1);
            }
            if (ld->verbose) {
                printf("Entry point: %s at 0x%08X\n", entry_name, ld->symbols[i].value);
            }
            return ld->symbols[i].value;
        }
    }
    
    // If no symbol found, use start of code section
    for (int i = 0; i < ld->num_sections; i++) {
        if (ld->sections[i].type == S32_SEC_CODE) {
            if (ld->verbose) {
                printf("Warning: No entry symbol found, using start of .text\n");
            }
            return ld->sections[i].vaddr;
        }
    }
    
    return 0;  // Default to address 0
}

// Write memory map file
static void write_memory_map(linker_state_t *ld) {
    if (!ld->print_map) return;
    
    // Generate .map filename from output filename
    char map_filename[256];
    snprintf(map_filename, sizeof(map_filename), "%s.map", ld->output_file);
    
    FILE *f = fopen(map_filename, "w");
    if (!f) {
        fprintf(stderr, "Warning: Cannot create map file '%s'\n", map_filename);
        return;
    }
    
    fprintf(f, "Memory Map for %s\n", ld->output_file);
    fprintf(f, "================================================================================\n\n");
    
    fprintf(f, "Memory Configuration:\n");
    fprintf(f, "  Code:     0x%08X - 0x%08X (%u bytes, %.1f KB)\n", 
            ld->code_base, ld->code_limit, ld->code_limit - ld->code_base,
            (ld->code_limit - ld->code_base) / 1024.0);
    fprintf(f, "  Rodata:   0x%08X - 0x%08X (%u bytes, %.1f KB)\n",
            ld->rodata_base, ld->rodata_limit, ld->rodata_limit - ld->rodata_base,
            (ld->rodata_limit - ld->rodata_base) / 1024.0);
    fprintf(f, "  Data:     0x%08X - 0x%08X (%u bytes, %.1f KB)\n",
            ld->data_base, ld->data_limit, ld->data_limit - ld->data_base,
            (ld->data_limit - ld->data_base) / 1024.0);
    fprintf(f, "  BSS:      0x%08X - 0x%08X (%u bytes, %.1f KB)\n",
            ld->data_limit, ld->bss_limit, ld->bss_limit - ld->data_limit,
            (ld->bss_limit - ld->data_limit) / 1024.0);
    fprintf(f, "  Heap:     0x%08X\n", ld->heap_base);
    fprintf(f, "  Stack:    0x%08X (size: %u bytes, %.1f KB)\n", 
            ld->stack_base, ld->stack_size, ld->stack_size / 1024.0);
    if (ld->mmio_size > 0) {
        fprintf(f, "  MMIO:     0x%08X - 0x%08X (%u bytes)\n",
                ld->mmio_base, ld->mmio_base + ld->mmio_size, ld->mmio_size);
    }
    fprintf(f, "\nEntry Point: 0x%08X\n", ld->entry_point);
    fprintf(f, "\n================================================================================\n\n");
    
    // Section details
    fprintf(f, "Sections:\n");
    fprintf(f, "  Address    Size       Name         Type\n");
    fprintf(f, "  ---------  ---------  -----------  --------\n");
    
    for (int i = 0; i < ld->num_sections; i++) {
        combined_section_t *sec = &ld->sections[i];
        const char *type_str = "OTHER";
        switch (sec->type) {
            case S32_SEC_CODE:   type_str = "CODE"; break;
            case S32_SEC_RODATA: type_str = "RODATA"; break;
            case S32_SEC_DATA:   type_str = "DATA"; break;
            case S32_SEC_BSS:    type_str = "BSS"; break;
        }
        fprintf(f, "  0x%08X 0x%08X %-12s %s\n",
                sec->vaddr, sec->size, sec->name, type_str);
    }
    
    fprintf(f, "\n================================================================================\n\n");
    
    // Global symbols
    fprintf(f, "Global Symbols:\n");
    fprintf(f, "  Address    Size       Name\n");
    fprintf(f, "  ---------  ---------  -----------\n");
    
    for (int i = 0; i < ld->num_symbols; i++) {
        symbol_entry_t *sym = &ld->symbols[i];
        if (sym->binding == S32O_BIND_GLOBAL && sym->defined_in_file >= 0) {
            fprintf(f, "  0x%08X 0x%08X %s\n", sym->value, sym->size, sym->name);
        }
    }
    
    fprintf(f, "\n================================================================================\n");
    fprintf(f, "Total memory usage: %u bytes (%.1f KB)\n",
            ld->bss_limit, ld->bss_limit / 1024.0);
    
    fclose(f);
    
    if (ld->verbose) {
        printf("Memory map written to %s\n", map_filename);
    }
}

// Write executable file
static void write_executable(linker_state_t *ld) {
    FILE *f = fopen(ld->output_file, "wb");
    if (!f) {
        fprintf(stderr, "Error: Cannot create output file '%s'\n", ld->output_file);
        exit(1);
    }
    
    // Initialize string table
    ld->string_table[0] = '\0';
    ld->string_table_size = 1;
    
    // Add section names to string table
    uint32_t section_name_offsets[MAX_SECTIONS];
    for (int i = 0; i < ld->num_sections; i++) {
        section_name_offsets[i] = add_string(ld, ld->sections[i].name);
    }
    
    // Calculate file layout
    uint32_t offset = sizeof(s32x_header_t);
    uint32_t section_table_offset = offset;
    offset += ld->num_sections * sizeof(s32x_section_t);
    
    uint32_t string_table_offset = offset;
    offset += ld->string_table_size;
    
    // Align to 16 bytes for section data
    offset = (offset + 15) & ~15;
    
    // Assign file offsets to sections
    uint32_t section_offsets[MAX_SECTIONS];
    for (int i = 0; i < ld->num_sections; i++) {
        if (ld->sections[i].type == S32_SEC_BSS) {
            section_offsets[i] = 0;  // BSS has no file data
        } else if (ld->sections[i].size > 0) {
            section_offsets[i] = offset;
            offset += ld->sections[i].size;
            offset = (offset + 3) & ~3;  // Align to 4 bytes
        } else {
            section_offsets[i] = 0;
        }
    }
    
    // Write header
    uint32_t flags = 0;
    if (ld->enable_wxorx) flags |= S32X_FLAG_W_XOR_X;
    if (ld->mmio_size > 0) flags |= S32X_FLAG_MMIO;
    
    s32x_header_t header = {
        .magic = S32X_MAGIC,
        .version = 1,
        .endian = S32_ENDIAN_LITTLE,
        .machine = S32_MACHINE_SLOW32,
        .entry = ld->entry_point,
        .nsections = ld->num_sections,
        .sec_offset = section_table_offset,
        .str_offset = string_table_offset,
        .str_size = ld->string_table_size,
        .flags = flags,
        .code_limit = ld->code_limit,
        .rodata_limit = ld->rodata_limit,
        .data_limit = ld->bss_limit,
        .stack_base = ld->stack_base,
        .mem_size = DEFAULT_MEM_SIZE,
        .heap_base = ld->heap_base,
        .checksum = 0,
        .mmio_base = ld->mmio_base
    };
    
    fwrite(&header, sizeof(header), 1, f);
    
    // Write section table
    for (int i = 0; i < ld->num_sections; i++) {
        combined_section_t *csec = &ld->sections[i];
        s32x_section_t section = {
            .name_offset = section_name_offsets[i],
            .type = csec->type,
            .vaddr = csec->vaddr,
            .offset = section_offsets[i],
            .size = (csec->type == S32_SEC_BSS) ? 0 : csec->size,
            .mem_size = csec->size,
            .flags = csec->flags
        };
        fwrite(&section, sizeof(section), 1, f);
    }
    
    // Write string table
    fwrite(ld->string_table, 1, ld->string_table_size, f);
    
    // Align to 16 bytes
    while (ftell(f) % 16) {
        fputc(0, f);
    }
    
    // Write section data
    for (int i = 0; i < ld->num_sections; i++) {
        if (section_offsets[i] > 0 && ld->sections[i].data) {
            fseek(f, section_offsets[i], SEEK_SET);
            fwrite(ld->sections[i].data, 1, ld->sections[i].size, f);
        }
    }
    
    fclose(f);
    
    if (ld->verbose) {
        printf("Generated executable: %s\n", ld->output_file);
        printf("  Entry point: 0x%08X\n", ld->entry_point);
        printf("  File size: %d bytes\n", offset);
    }
}

// Clean up resources
static void cleanup(linker_state_t *ld) {
    for (int i = 0; i < ld->num_input_files; i++) {
        input_file_t *inf = &ld->input_files[i];
        if (inf->file) fclose(inf->file);
        if (inf->sections) free(inf->sections);
        if (inf->symbols) free(inf->symbols);
        if (inf->relocations) free(inf->relocations);
        if (inf->string_table) free(inf->string_table);
    }
    
    for (int i = 0; i < ld->num_sections; i++) {
        if (ld->sections[i].data) free(ld->sections[i].data);
    }
}

// Parse size with optional K/M suffix
static uint32_t parse_size(const char *str) {
    char *end;
    unsigned long val = strtoul(str, &end, 0);
    
    if (*end == 'K' || *end == 'k') {
        val *= 1024;
    } else if (*end == 'M' || *end == 'm') {
        val *= 1024 * 1024;
    }
    
    return (uint32_t)val;
}

// Print usage
static void print_usage(const char *prog) {
    fprintf(stderr, "Usage: %s [options] file.s32o [...] -o output.s32x\n", prog);
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "  -o FILE           Output file (required)\n");
    fprintf(stderr, "  -e SYMBOL         Entry point symbol (default: _start)\n");
    fprintf(stderr, "  -L PATH           Add library search path\n");
    fprintf(stderr, "  -l NAME           Link with library libNAME.s32a\n");
    fprintf(stderr, "  -s                Strip symbols\n");
    fprintf(stderr, "  -v                Verbose output\n");
    fprintf(stderr, "  --wxorx           Enable W^X protection\n");
    fprintf(stderr, "  --no-wxorx        Disable W^X protection\n");
    fprintf(stderr, "Memory layout options:\n");
    fprintf(stderr, "  --code-size SIZE  Max code segment size (default: 1MB)\n");
    fprintf(stderr, "  --rodata-size SIZE Max rodata size (default: 1MB)\n");
    fprintf(stderr, "  --data-size SIZE  Max data+bss size (default: 1MB)\n");
    fprintf(stderr, "  --stack-size SIZE Stack size (default: 64KB)\n");
    fprintf(stderr, "  --mmio SIZE       Reserve SIZE bytes for MMIO region\n");
    fprintf(stderr, "  --pack-sections   Pack sections tightly to minimize gaps\n");
    fprintf(stderr, "  --compact         Ultra-compact mode (4KB pages, minimal memory)\n");
    fprintf(stderr, "  --print-map       Generate memory map file (.map)\n");
    fprintf(stderr, "  --help            Show this help\n");
    fprintf(stderr, "\nSizes can use suffixes: K=1024, M=1024*1024\n");
}

int main(int argc, char *argv[]) {
    linker_state_t ld = {0};
    
    // Initialize defaults
    ld.code_base = DEFAULT_CODE_BASE;
    ld.code_size = DEFAULT_CODE_SIZE;
    ld.rodata_size = DEFAULT_RODATA_SIZE;
    ld.data_size = DEFAULT_DATA_SIZE;
    ld.stack_size = DEFAULT_STACK_SIZE;
    ld.stack_base = DEFAULT_STACK_BASE;
    ld.enable_wxorx = true;  // Default to secure
    
    // Parse command line
    int num_input_files = 0;
    const char *input_files[MAX_INPUT_FILES];
    
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-o") == 0 && i + 1 < argc) {
            ld.output_file = argv[++i];
        } else if (strcmp(argv[i], "-e") == 0 && i + 1 < argc) {
            ld.entry_symbol = argv[++i];
        } else if (strcmp(argv[i], "-L") == 0 && i + 1 < argc) {
            if (ld.num_lib_paths >= MAX_LIB_PATHS) {
                fprintf(stderr, "Error: Too many library paths (max %d)\n", MAX_LIB_PATHS);
                return 1;
            }
            ld.lib_paths[ld.num_lib_paths++] = argv[++i];
        } else if (strncmp(argv[i], "-l", 2) == 0) {
            // Handle both -lname and -l name formats
            const char *libname;
            if (argv[i][2] != '\0') {
                // -lname format
                libname = argv[i];
            } else if (i + 1 < argc) {
                // -l name format
                char *temp = malloc(strlen(argv[i+1]) + 3);
                sprintf(temp, "-l%s", argv[++i]);
                libname = temp;
            } else {
                fprintf(stderr, "Error: -l requires a library name\n");
                return 1;
            }
            if (num_input_files >= MAX_INPUT_FILES) {
                fprintf(stderr, "Error: Too many input files\n");
                return 1;
            }
            input_files[num_input_files++] = libname;
        } else if (strcmp(argv[i], "-s") == 0) {
            ld.strip_symbols = true;
        } else if (strcmp(argv[i], "-v") == 0) {
            ld.verbose = true;
        } else if (strcmp(argv[i], "--wxorx") == 0) {
            ld.enable_wxorx = true;
        } else if (strcmp(argv[i], "--no-wxorx") == 0) {
            ld.enable_wxorx = false;
        } else if (strcmp(argv[i], "--code-size") == 0 && i + 1 < argc) {
            ld.code_size = parse_size(argv[++i]);
        } else if (strcmp(argv[i], "--rodata-size") == 0 && i + 1 < argc) {
            ld.rodata_size = parse_size(argv[++i]);
        } else if (strcmp(argv[i], "--data-size") == 0 && i + 1 < argc) {
            ld.data_size = parse_size(argv[++i]);
        } else if (strcmp(argv[i], "--stack-size") == 0 && i + 1 < argc) {
            ld.stack_size = parse_size(argv[++i]);
        } else if (strcmp(argv[i], "--mmio") == 0 && i + 1 < argc) {
            ld.mmio_size = parse_size(argv[++i]);
        } else if (strcmp(argv[i], "--pack-sections") == 0) {
            ld.pack_sections = true;
        } else if (strcmp(argv[i], "--compact") == 0) {
            ld.compact_mode = true;
            ld.pack_sections = true;  // Compact implies packing
        } else if (strcmp(argv[i], "--print-map") == 0) {
            ld.print_map = true;
        } else if (strcmp(argv[i], "--help") == 0) {
            print_usage(argv[0]);
            return 0;
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "Unknown option: %s\n", argv[i]);
            print_usage(argv[0]);
            return 1;
        } else {
            if (num_input_files >= MAX_INPUT_FILES) {
                fprintf(stderr, "Error: Too many input files\n");
                return 1;
            }
            input_files[num_input_files++] = argv[i];
        }
    }
    
    if (num_input_files == 0) {
        fprintf(stderr, "Error: No input files\n");
        print_usage(argv[0]);
        return 1;
    }
    
    if (!ld.output_file) {
        fprintf(stderr, "Error: No output file specified\n");
        print_usage(argv[0]);
        return 1;
    }
    
    // Load all input files
    for (int i = 0; i < num_input_files; i++) {
        if (!load_archive_file(&ld, input_files[i])) {
            cleanup(&ld);
            return 1;
        }
    }
    
    // Link everything
    merge_sections(&ld);
    build_symbol_table(&ld);
    
    layout_sections(&ld);
    
    update_symbol_values(&ld);  // Update symbol values after sections have addresses
    inject_memory_map_symbols(&ld);  // Add memory map symbols after layout
    collect_relocations(&ld);
    load_section_data(&ld);
    apply_relocations(&ld);
    
    // Find entry point
    ld.entry_point = find_entry_point(&ld);
    
    // Write output
    write_executable(&ld);
    write_memory_map(&ld);
    
    // Clean up
    cleanup(&ld);
    
    return 0;
}

// Return true if sym_index appears in any relocation of this input file
static bool symbol_referenced_in_file(const input_file_t *inf, uint32_t sym_index) {
    if (!inf->relocations) return false;
    uint32_t reloc_idx = 0;
    for (uint32_t si = 0; si < inf->header.nsections; si++) {
        s32o_section_t *isec = &inf->sections[si];
        for (uint32_t r = 0; r < isec->nrelocs; r++) {
            s32o_reloc_t *irel = &inf->relocations[reloc_idx++];
            if (irel->symbol == sym_index) return true;
        }
    }
    return false;
}

// Find combined section index by name
static int find_combined_section_index_by_name(combined_section_t *secs, int nsecs, const char *name) {
    for (int i = 0; i < nsecs; i++) {
        if (strcmp(secs[i].name, name) == 0) return i;
    }
    return -1;
}

// Linear lookup (small N): find relocation index for (file,section,offset)
static int find_reloc_at(linker_state_t *ld, int file_idx, int section_idx, uint32_t offset) {
    for (int i = 0; i < ld->num_relocations; i++) {
        relocation_entry_t *r = &ld->relocations[i];
        if (r->file_idx == file_idx && r->section_idx == section_idx && r->offset == offset) {
            return i;
        }
    }
    return -1;
}