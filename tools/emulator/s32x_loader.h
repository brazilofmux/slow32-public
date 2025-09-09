// SLOW-32 Executable (.s32x) Loader
// Shared by all emulator implementations

#ifndef S32X_LOADER_H
#define S32X_LOADER_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "../../common/s32_formats.h"

// Loader configuration
typedef struct {
    uint32_t *memory;
    uint32_t mem_size;
    uint32_t *pc;
    uint32_t *sp;
    uint32_t *registers;  // For setting up initial register state
    int verbose;
} s32x_loader_config_t;

// Load result
typedef struct {
    int success;
    uint32_t entry_point;
    uint32_t stack_base;
    uint32_t code_limit;
    uint32_t rodata_limit;
    uint32_t data_limit;
    uint32_t heap_base;
    int has_wxorx;
    char error_msg[256];
} s32x_load_result_t;

// S32X files are always loaded - no need to check extension
// The loader will verify the magic number

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
    
    fseek(f, header.str_offset, SEEK_SET);
    fread(strtab, 1, header.str_size, f);
    
    // Read and load sections
    fseek(f, header.sec_offset, SEEK_SET);
    for (uint32_t i = 0; i < header.nsections; i++) {
        s32x_section_t section;
        if (fread(&section, sizeof(section), 1, f) != 1) {
            snprintf(result.error_msg, sizeof(result.error_msg), 
                     "Cannot read section %u", i);
            free(strtab);
            fclose(f);
            return result;
        }
        
        const char *name = (section.name_offset < header.str_size) ? 
                           &strtab[section.name_offset] : "?";
        
        if (config->verbose) {
            printf("Loading section '%s': %u bytes at 0x%08X\n", 
                   name, section.mem_size, section.vaddr);
        }
        
        // Validate section address
        if (section.vaddr / 4 >= config->mem_size / 4) {
            snprintf(result.error_msg, sizeof(result.error_msg), 
                     "Section '%s' address out of bounds: 0x%08X", 
                     name, section.vaddr);
            free(strtab);
            fclose(f);
            return result;
        }
        if (section.vaddr > config->mem_size || 
            section.mem_size > config->mem_size - section.vaddr) {
            snprintf(result.error_msg, sizeof(result.error_msg),
                     "Section '%s' exceeds memory: vaddr=0x%08X size=%u",
                     name, section.vaddr, section.mem_size);
            free(strtab);
            fclose(f);
            return result;
        }
        
        // Load section data (if not BSS)
        if (section.size > 0 && section.offset > 0) {
            long current_pos = ftell(f);
            fseek(f, section.offset, SEEK_SET);
            
            // Read directly into memory (assuming little-endian host)
            uint8_t *dest = (uint8_t *)config->memory + section.vaddr;
            if (fread(dest, 1, section.size, f) != section.size) {
                snprintf(result.error_msg, sizeof(result.error_msg), 
                         "Cannot read section '%s' data", name);
                free(strtab);
                fclose(f);
                return result;
            }
            
            fseek(f, current_pos, SEEK_SET);
        }
        
        // Zero-fill BSS sections and padding
        if (section.mem_size > section.size) {
            uint8_t *dest = (uint8_t *)config->memory + section.vaddr + section.size;
            memset(dest, 0, section.mem_size - section.size);
        }
    }
    
    free(strtab);
    fclose(f);
    
    // Set up return values
    result.success = 1;
    result.entry_point = header.entry;
    result.stack_base = header.stack_base;
    result.code_limit = header.code_limit;
    result.rodata_limit = header.rodata_limit;
    result.data_limit = header.data_limit;
    result.heap_base = header.heap_base;
    result.has_wxorx = (header.flags & S32X_FLAG_W_XOR_X) != 0;
    
    // Initialize PC and SP
    if (config->pc) *config->pc = header.entry;
    if (config->sp && config->registers) {
        config->registers[29] = header.stack_base;  // r29 = sp
    }
    
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

// Load legacy .bin format (flat binary)
static int load_bin_file(const char *filename, s32x_loader_config_t *config) {
    FILE *f = fopen(filename, "rb");
    if (!f) {
        fprintf(stderr, "Cannot open file: %s\n", filename);
        return 0;
    }
    
    // Determine file size
    fseek(f, 0, SEEK_END);
    long file_size = ftell(f);
    fseek(f, 0, SEEK_SET);
    
    if (file_size > (long)config->mem_size) {
        fprintf(stderr, "Program too large: %ld bytes (max %u)\n", 
                file_size, config->mem_size);
        fclose(f);
        return 0;
    }
    
    // Read directly into memory
    size_t words = (file_size + 3) / 4;
    if (fread(config->memory, 4, words, f) != words) {
        fprintf(stderr, "Failed to read program\n");
        fclose(f);
        return 0;
    }
    
    fclose(f);
    
    // Initialize PC to 0 and SP to top of memory
    if (config->pc) *config->pc = 0;
    if (config->sp && config->registers) {
        config->registers[29] = 0x0FFFFFF0;  // Default stack
    }
    
    if (config->verbose) {
        printf("Loaded %ld bytes in legacy .bin format\n", file_size);
    }
    
    return 1;
}

#endif // S32X_LOADER_H