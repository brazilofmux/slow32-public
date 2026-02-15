// SLOW32 Dump Utility
// Unified dumper for .s32o object files and .s32x executables

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <ctype.h>
#include <s32_formats.h>

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

static bool read_fully(void *dst, size_t size, FILE *f) {
    return fread(dst, 1, size, f) == size;
}

static long file_size_for(FILE *f) {
    long pos = ftell(f);
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, pos, SEEK_SET);
    return size;
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
        case S32O_REL_PCREL_HI20: return "PCREL_HI20";
        case S32O_REL_PCREL_LO12: return "PCREL_LO12";
        default:              return "UNKNOWN";
    }
}

static void hex_dump(const uint8_t *data, uint32_t size, uint32_t base_addr) {
    for (uint32_t i = 0; i < size; i += 16) {
        printf("  %08X: ", base_addr + i);

        for (uint32_t j = 0; j < 16; j++) {
            if (i + j < size) {
                printf("%02X ", data[i + j]);
            } else {
                printf("   ");
            }
            if (j == 7) printf(" ");
        }

        printf(" |");
        for (uint32_t j = 0; j < 16 && i + j < size; j++) {
            uint8_t c = data[i + j];
            printf("%c", isprint(c) ? c : '.');
        }
        printf("|\n");
    }
}

static bool dump_s32o(const char *filename, FILE *f, long file_size, options_t *opts) {
    s32o_header_t header;
    if (!read_fully(&header, sizeof(header), f)) {
        fprintf(stderr, "Error: Cannot read object file header\n");
        return false;
    }

    if (header.magic != S32O_MAGIC) {
        fprintf(stderr, "Error: Not a valid .s32o file (bad magic: 0x%08X)\n", header.magic);
        return false;
    }

    if (header.sec_offset + header.nsections * sizeof(s32o_section_t) > (uint32_t)file_size ||
        header.sym_offset + header.nsymbols * sizeof(s32o_symbol_t) > (uint32_t)file_size ||
        header.str_offset + header.str_size > (uint32_t)file_size) {
        fprintf(stderr, "Error: Object file tables out of bounds\n");
        return false;
    }

    s32o_section_t *sections = calloc(header.nsections, sizeof(s32o_section_t));
    s32o_symbol_t *symbols = calloc(header.nsymbols, sizeof(s32o_symbol_t));
    char *strtab = malloc(header.str_size);
    if (!sections || !symbols || !strtab) {
        fprintf(stderr, "Error: Out of memory\n");
        free(sections);
        free(symbols);
        free(strtab);
        return false;
    }

    fseek(f, header.sec_offset, SEEK_SET);
    if (!read_fully(sections, header.nsections * sizeof(s32o_section_t), f)) {
        fprintf(stderr, "Error: Cannot read section table\n");
        free(sections);
        free(symbols);
        free(strtab);
        return false;
    }

    fseek(f, header.sym_offset, SEEK_SET);
    if (!read_fully(symbols, header.nsymbols * sizeof(s32o_symbol_t), f)) {
        fprintf(stderr, "Error: Cannot read symbol table\n");
        free(sections);
        free(symbols);
        free(strtab);
        return false;
    }

    fseek(f, header.str_offset, SEEK_SET);
    if (!read_fully(strtab, header.str_size, f)) {
        fprintf(stderr, "Error: Cannot read string table\n");
        free(sections);
        free(symbols);
        free(strtab);
        return false;
    }

    printf("\n%s:     file format s32o-slow32\n", filename);

    if (opts->show_header) {
        printf("\nObject File Header:\n");
        printf("  Magic:        0x%08X (%c%c%c%c)\n", header.magic,
               (char)(header.magic & 0xFF),
               (char)((header.magic >> 8) & 0xFF),
               (char)((header.magic >> 16) & 0xFF),
               (char)((header.magic >> 24) & 0xFF));
        printf("  Version:      %d\n", header.version);
        printf("  Endianness:   %s\n", header.endian == S32_ENDIAN_LITTLE ? "Little" : "Big");
        printf("  Machine:      0x%02X (SLOW-32)\n", header.machine);
        printf("  Flags:        0x%08X", header.flags);
        if (header.flags & S32O_FLAG_PIC) printf(" PIC");
        if (header.flags & S32O_FLAG_DEBUG) printf(" DEBUG");
        if (header.flags & S32O_FLAG_STRIPPED) printf(" STRIPPED");
        printf("\n");
        printf("  Sections:     %d at offset 0x%X\n", header.nsections, header.sec_offset);
        printf("  Symbols:      %d at offset 0x%X\n", header.nsymbols, header.sym_offset);
        printf("  String Table: %d bytes at offset 0x%X\n", header.str_size, header.str_offset);
        printf("  Checksum:     0x%08X\n", header.checksum);
    }

    if (opts->show_sections) {
        printf("\nSections:\n");
        printf("Idx Name              Type     Size     Offset   Align  Relocs  Flags\n");
        printf("--- ----------------- -------- -------- -------- ------ ------- -------\n");
        for (uint32_t i = 0; i < header.nsections; i++) {
            s32o_section_t *sec = &sections[i];
            const char *name = (sec->name_offset < header.str_size) ? &strtab[sec->name_offset] : "<invalid>";
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

    if (opts->show_symbols) {
        printf("\nSymbol Table:\n");
        printf("Num Value    Size     Type     Bind     Sec  Name\n");
        printf("--- -------- -------- -------- -------- ---- -----------------\n");
        for (uint32_t i = 0; i < header.nsymbols; i++) {
            s32o_symbol_t *sym = &symbols[i];
            const char *name = (sym->name_offset < header.str_size) ? &strtab[sym->name_offset] : "<invalid>";
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

    if (opts->show_relocations) {
        for (uint32_t i = 0; i < header.nsections; i++) {
            s32o_section_t *sec = &sections[i];
            if (sec->nrelocs == 0) continue;
            if (sec->reloc_offset + sec->nrelocs * sizeof(s32o_reloc_t) > (uint32_t)file_size) {
                fprintf(stderr, "Error: Relocation table out of bounds\n");
                break;
            }
            const char *sec_name = (sec->name_offset < header.str_size) ? &strtab[sec->name_offset] : "<invalid>";
            printf("\nRelocation section for '%s':\n", sec_name);
            printf("Offset   Type     Symbol   Addend   Symbol Name\n");
            printf("-------- -------- -------- -------- -----------------\n");
            fseek(f, sec->reloc_offset, SEEK_SET);
            for (uint32_t j = 0; j < sec->nrelocs; j++) {
                s32o_reloc_t reloc;
                if (!read_fully(&reloc, sizeof(reloc), f)) {
                    fprintf(stderr, "Error: Cannot read relocation\n");
                    break;
                }
                const char *sym_name = "???";
                if (reloc.symbol < header.nsymbols) {
                    sym_name = (symbols[reloc.symbol].name_offset < header.str_size)
                        ? &strtab[symbols[reloc.symbol].name_offset]
                        : "<invalid>";
                }
                printf("%08X %-8s %8d %8d %s\n",
                       reloc.offset, reloc_type_name(reloc.type),
                       reloc.symbol, reloc.addend, sym_name);
            }
        }
    }

    if (opts->show_hex) {
        for (uint32_t i = 0; i < header.nsections; i++) {
            s32o_section_t *sec = &sections[i];
            const char *name = (sec->name_offset < header.str_size) ? &strtab[sec->name_offset] : "<invalid>";
            if (sec->size == 0 || sec->type == S32_SEC_BSS) continue;
            if (sec->offset + sec->size > (uint32_t)file_size) {
                fprintf(stderr, "Error: Section data out of bounds\n");
                break;
            }
            printf("\nContents of section %s:\n", name);
            uint8_t *data = malloc(sec->size);
            if (!data) {
                fprintf(stderr, "Error: Out of memory\n");
                continue;
            }
            fseek(f, sec->offset, SEEK_SET);
            if (!read_fully(data, sec->size, f)) {
                fprintf(stderr, "Error: Cannot read section data\n");
                free(data);
                break;
            }
            hex_dump(data, sec->size, 0);
            free(data);
        }
    }

    free(sections);
    free(symbols);
    free(strtab);
    return true;
}

static bool dump_s32x(const char *filename, FILE *f, long file_size, options_t *opts) {
    s32x_header_t header;
    if (!read_fully(&header, sizeof(header), f)) {
        fprintf(stderr, "Error: Cannot read executable header\n");
        return false;
    }

    if (header.magic != S32X_MAGIC) {
        fprintf(stderr, "Error: Not a valid .s32x file (magic: 0x%08X)\n", header.magic);
        return false;
    }

    if (header.sec_offset + header.nsections * sizeof(s32x_section_t) > (uint32_t)file_size ||
        header.str_offset + header.str_size > (uint32_t)file_size) {
        fprintf(stderr, "Error: Executable tables out of bounds\n");
        return false;
    }

    char *strtab = malloc(header.str_size);
    if (!strtab) {
        fprintf(stderr, "Error: Out of memory\n");
        return false;
    }
    fseek(f, header.str_offset, SEEK_SET);
    if (!read_fully(strtab, header.str_size, f)) {
        fprintf(stderr, "Error: Cannot read string table\n");
        free(strtab);
        return false;
    }

    printf("\n%s:     file format s32x-slow32\n", filename);

    if (opts->show_header) {
        printf("\nExecutable Header:\n");
        printf("  Magic:        0x%08X (%c%c%c%c)\n", header.magic,
               (char)(header.magic & 0xFF),
               (char)((header.magic >> 8) & 0xFF),
               (char)((header.magic >> 16) & 0xFF),
               (char)((header.magic >> 24) & 0xFF));
        printf("  Version:      %d\n", header.version);
        printf("  Machine:      0x%02X (SLOW-32)\n", header.machine);
        printf("  Entry Point:  0x%08X\n", header.entry);
        printf("  Sections:     %d\n", header.nsections);
        printf("  Flags:        0x%08X", header.flags);
        if (header.flags & S32X_FLAG_W_XOR_X) printf(" W^X");
        if (header.flags & S32X_FLAG_HAS_EVT) printf(" EVT");
        if (header.flags & S32X_FLAG_HAS_TSR) printf(" TSR");
        if (header.flags & S32X_FLAG_HAS_DEBUG) printf(" DEBUG");
        printf("\n");
        printf("  Memory Layout:\n");
        printf("    Code limit:   0x%08X\n", header.code_limit);
        printf("    ROData limit: 0x%08X\n", header.rodata_limit);
        printf("    Data limit:   0x%08X\n", header.data_limit);
        printf("    Stack base:   0x%08X\n", header.stack_base);
        printf("    Stack end:    0x%08X\n", header.stack_end);
        printf("    Heap base:    0x%08X\n", header.heap_base);
        printf("    Memory size:  0x%08X\n", header.mem_size);
    }

    if (opts->show_sections) {
        printf("\nSections:\n");
        printf("Name              Type     VAddr      Size     Offset   Flags\n");
        printf("----------------- -------- ---------- -------- -------- -----\n");
        fseek(f, header.sec_offset, SEEK_SET);
        for (uint32_t i = 0; i < header.nsections; i++) {
            s32x_section_t section;
            if (!read_fully(&section, sizeof(section), f)) {
                fprintf(stderr, "Error: Cannot read section table\n");
                break;
            }
            const char *name = (section.name_offset < header.str_size) ? &strtab[section.name_offset] : "<invalid>";
            printf("%-17s %-8s 0x%08X %8d %08X ",
                   name, section_type_name(section.type),
                   section.vaddr, section.mem_size, section.offset);
            if (section.flags & S32_SEC_FLAG_EXEC) printf("X");
            if (section.flags & S32_SEC_FLAG_WRITE) printf("W");
            if (section.flags & S32_SEC_FLAG_READ) printf("R");
            printf("\n");
        }
    }

    if (opts->show_symbols || opts->show_relocations) {
        fprintf(stderr, "Note: symbols/relocations are not stored in .s32x files\n");
    }

    if (opts->show_hex) {
        fseek(f, header.sec_offset, SEEK_SET);
        for (uint32_t i = 0; i < header.nsections; i++) {
            s32x_section_t section;
            if (!read_fully(&section, sizeof(section), f)) {
                fprintf(stderr, "Error: Cannot read section table\n");
                break;
            }
            if (section.size == 0 || section.type == S32_SEC_BSS) continue;
            if (section.offset + section.size > (uint32_t)file_size) {
                fprintf(stderr, "Error: Section data out of bounds\n");
                break;
            }
            const char *name = (section.name_offset < header.str_size) ? &strtab[section.name_offset] : "<invalid>";
            printf("\nContents of section %s:\n", name);
            uint8_t *data = malloc(section.size);
            if (!data) {
                fprintf(stderr, "Error: Out of memory\n");
                continue;
            }
            long saved = ftell(f);
            fseek(f, section.offset, SEEK_SET);
            if (!read_fully(data, section.size, f)) {
                fprintf(stderr, "Error: Cannot read section data\n");
                free(data);
                fseek(f, saved, SEEK_SET);
                break;
            }
            hex_dump(data, section.size, section.vaddr);
            free(data);
            fseek(f, saved, SEEK_SET);
        }
    }

    free(strtab);
    return true;
}

static void print_usage(const char *prog) {
    fprintf(stderr, "Usage: %s [options] file.s32o|file.s32x\n", prog);
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "  -h    Display file header\n");
    fprintf(stderr, "  -S    Display section headers\n");
    fprintf(stderr, "  -s    Display symbol table (s32o only)\n");
    fprintf(stderr, "  -r    Display relocations (s32o only)\n");
    fprintf(stderr, "  -x    Display section contents as hex\n");
    fprintf(stderr, "  -a    Display all information\n");
    fprintf(stderr, "  -v    Verbose output\n");
    fprintf(stderr, "  --help Show this help\n");
}

int main(int argc, char *argv[]) {
    options_t opts = { 0 };
    const char *filename = NULL;

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

    FILE *f = fopen(filename, "rb");
    if (!f) {
        perror("Cannot open file");
        return 1;
    }

    long size = file_size_for(f);
    if (size < (long)sizeof(uint32_t)) {
        fprintf(stderr, "Error: File too small\n");
        fclose(f);
        return 1;
    }

    uint32_t magic = 0;
    if (!read_fully(&magic, sizeof(magic), f)) {
        fprintf(stderr, "Error: Cannot read file magic\n");
        fclose(f);
        return 1;
    }

    fseek(f, 0, SEEK_SET);

    bool ok = false;
    if (magic == S32O_MAGIC) {
        ok = dump_s32o(filename, f, size, &opts);
    } else if (magic == S32X_MAGIC) {
        ok = dump_s32x(filename, f, size, &opts);
    } else {
        fprintf(stderr, "Error: Unknown file magic 0x%08X\n", magic);
        fclose(f);
        return 1;
    }

    fclose(f);
    return ok ? 0 : 1;
}
