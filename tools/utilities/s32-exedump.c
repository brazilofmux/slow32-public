// SLOW-32 Executable Dumper
// Displays contents of .s32x executable files

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include "../../common/s32_formats.h"

static void print_header(s32x_header_t *header) {
    printf("Executable Header:\n");
    printf("  Magic:        0x%08X (%c%c%c%c)\n", header->magic,
           (char)(header->magic & 0xFF),
           (char)((header->magic >> 8) & 0xFF),
           (char)((header->magic >> 16) & 0xFF),
           (char)((header->magic >> 24) & 0xFF));
    printf("  Version:      %d\n", header->version);
    printf("  Machine:      0x%02X (SLOW-32)\n", header->machine);
    printf("  Entry Point:  0x%08X\n", header->entry);
    printf("  Sections:     %d\n", header->nsections);
    printf("  Flags:        0x%08X", header->flags);
    if (header->flags & S32X_FLAG_W_XOR_X) printf(" W^X");
    if (header->flags & S32X_FLAG_HAS_EVT) printf(" EVT");
    if (header->flags & S32X_FLAG_HAS_TSR) printf(" TSR");
    if (header->flags & S32X_FLAG_HAS_DEBUG) printf(" DEBUG");
    printf("\n");
    printf("  Memory Layout:\n");
    printf("    Code limit:   0x%08X\n", header->code_limit);
    printf("    ROData limit: 0x%08X\n", header->rodata_limit);
    printf("    Data limit:   0x%08X\n", header->data_limit);
    printf("    Stack base:   0x%08X\n", header->stack_base);
    printf("    Stack end:    0x%08X\n", header->stack_end);
    printf("    Heap base:    0x%08X\n", header->heap_base);
    printf("    Memory size:  0x%08X\n", header->mem_size);
}

static const char *section_type_name(uint32_t type) {
    switch (type) {
        case S32_SEC_CODE:    return "CODE";
        case S32_SEC_DATA:    return "DATA";
        case S32_SEC_BSS:     return "BSS";
        case S32_SEC_RODATA:  return "RODATA";
        default:              return "UNKNOWN";
    }
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s file.s32x\n", argv[0]);
        return 1;
    }
    
    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        fprintf(stderr, "Error: Cannot open '%s'\n", argv[1]);
        return 1;
    }
    
    s32x_header_t header;
    if (fread(&header, sizeof(header), 1, f) != 1) {
        fprintf(stderr, "Error: Cannot read header\n");
        fclose(f);
        return 1;
    }
    
    if (header.magic != S32X_MAGIC) {
        fprintf(stderr, "Error: Not a valid .s32x file\n");
        fclose(f);
        return 1;
    }
    
    printf("\n%s:     file format s32x-slow32\n\n", argv[1]);
    print_header(&header);
    
    // Read string table
    char *strtab = malloc(header.str_size);
    fseek(f, header.str_offset, SEEK_SET);
    fread(strtab, 1, header.str_size, f);
    
    // Read and display sections
    printf("\nSections:\n");
    printf("Name              Type     VAddr      Size     Offset   Flags\n");
    printf("----------------- -------- ---------- -------- -------- -----\n");
    
    fseek(f, header.sec_offset, SEEK_SET);
    for (uint32_t i = 0; i < header.nsections; i++) {
        s32x_section_t section;
        fread(&section, sizeof(section), 1, f);
        
        const char *name = &strtab[section.name_offset];
        printf("%-17s %-8s 0x%08X %8d %08X ",
               name, section_type_name(section.type),
               section.vaddr, section.mem_size, section.offset);
        
        if (section.flags & S32_SEC_FLAG_EXEC) printf("X");
        if (section.flags & S32_SEC_FLAG_WRITE) printf("W");
        if (section.flags & S32_SEC_FLAG_READ) printf("R");
        printf("\n");
    }
    
    free(strtab);
    fclose(f);
    return 0;
}