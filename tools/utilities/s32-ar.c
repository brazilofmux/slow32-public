#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>
#include "../../common/s32a_format.h"
#include "../../common/s32_formats.h"

typedef struct {
    char *name;
    uint8_t *data;
    size_t size;
    time_t timestamp;
} member_t;

// Global state for building archive
typedef struct {
    member_t *members;
    size_t nmembers;
    size_t members_capacity;
} archive_state_t;

typedef struct {
    char *name;
    uint32_t member_idx;
} symbol_entry_t;

typedef struct {
    char *strings;
    size_t str_size;
    size_t str_capacity;
} string_table_t;

static uint32_t add_string(string_table_t *table, const char *str) {
    size_t len = strlen(str) + 1;
    
    // Check if string already exists
    size_t offset = 0;
    while (offset < table->str_size) {
        if (strcmp(table->strings + offset, str) == 0) {
            return offset;
        }
        offset += strlen(table->strings + offset) + 1;
    }
    
    // Add new string
    if (table->str_size + len > table->str_capacity) {
        table->str_capacity = (table->str_capacity + len) * 2;
        table->strings = realloc(table->strings, table->str_capacity);
    }
    
    uint32_t result = table->str_size;
    strcpy(table->strings + table->str_size, str);
    table->str_size += len;
    return result;
}

static const char *basename_simple(const char *path) {
    const char *slash = strrchr(path, '/');
    const char *bslash = strrchr(path, '\\');
    const char *base = path;
    if (slash && bslash) {
        base = (slash > bslash) ? slash + 1 : bslash + 1;
    } else if (slash) {
        base = slash + 1;
    } else if (bslash) {
        base = bslash + 1;
    }
    return (*base != '\0') ? base : path;
}

static bool read_fully(void *dst, size_t size, FILE *f) {
    return fread(dst, 1, size, f) == size;
}

static long file_size_for(FILE *f) {
    struct stat st;
    if (fstat(fileno(f), &st) != 0) return -1;
    return st.st_size;
}

static bool validate_s32o(const char *name, uint8_t *data, size_t size) {
    if (size < sizeof(s32o_header_t)) {
        fprintf(stderr, "Error: '%s' is too small to be a valid object file\n", name);
        return false;
    }
    s32o_header_t *hdr = (s32o_header_t *)data;
    if (hdr->magic != S32O_MAGIC) {
        fprintf(stderr, "Error: '%s' is not a valid SLOW-32 object file\n", name);
        return false;
    }
    return true;
}

static int find_member_index(archive_state_t *state, const char *name) {
    for (size_t i = 0; i < state->nmembers; i++) {
        if (strcmp(state->members[i].name, name) == 0) return (int)i;
    }
    return -1;
}

static bool add_member_data(archive_state_t *state, const char *name, uint8_t *data, size_t size, time_t ts) {
    if (!validate_s32o(name, data, size)) {
        free(data);
        return false;
    }
    
    if (state->nmembers >= state->members_capacity) {
        state->members_capacity = state->members_capacity * 2 + 1;
        state->members = realloc(state->members, state->members_capacity * sizeof(member_t));
    }
    
    member_t *member = &state->members[state->nmembers];
    member->name = strdup(name);
    member->data = data;
    member->size = size;
    member->timestamp = ts;
    state->nmembers++;
    return true;
}

static bool add_or_replace_member(archive_state_t *state, const char *filename) {
    FILE *f = fopen(filename, "rb");
    if (!f) {
        fprintf(stderr, "Error: Cannot open '%s'\n", filename);
        return false;
    }
    
    struct stat st;
    if (fstat(fileno(f), &st) != 0) {
        fclose(f);
        fprintf(stderr, "Error: Cannot stat '%s'\n", filename);
        return false;
    }
    
    if (st.st_size == 0) {
        fclose(f);
        fprintf(stderr, "Error: '%s' is empty\n", filename);
        return false;
    }
    
    uint8_t *data = malloc(st.st_size);
    if (!read_fully(data, st.st_size, f)) {
        fclose(f);
        free(data);
        fprintf(stderr, "Error: Cannot read '%s'\n", filename);
        return false;
    }
    fclose(f);
    
    const char *base = basename_simple(filename);
    if (!validate_s32o(base, data, st.st_size)) {
        free(data);
        return false;
    }
    int existing = find_member_index(state, base);
    if (existing >= 0) {
        member_t *member = &state->members[existing];
        free(member->data);
        free(member->name);
        member->name = strdup(base);
        member->data = data;
        member->size = st.st_size;
        member->timestamp = st.st_mtime;
        return true;
    }
    
    return add_member_data(state, base, data, st.st_size, st.st_mtime);
}

static symbol_entry_t *build_symbol_index(archive_state_t *state, size_t *out_count) {
    size_t cap = 128;
    size_t count = 0;
    symbol_entry_t *symbols = malloc(cap * sizeof(symbol_entry_t));
    
    for (size_t m = 0; m < state->nmembers; m++) {
        member_t *member = &state->members[m];
        if (!member->data || member->size < sizeof(s32o_header_t)) continue;
        s32o_header_t *hdr = (s32o_header_t *)member->data;
        if (hdr->magic != S32O_MAGIC) continue;
        if (hdr->sym_offset + hdr->nsymbols * sizeof(s32o_symbol_t) > member->size) continue;
        if (hdr->str_offset + hdr->str_size > member->size) continue;
        
        s32o_symbol_t *symbols_in = (s32o_symbol_t *)(member->data + hdr->sym_offset);
        char *obj_strings = (char *)(member->data + hdr->str_offset);
        
        for (uint32_t i = 0; i < hdr->nsymbols; i++) {
            if (symbols_in[i].binding == S32O_BIND_GLOBAL && symbols_in[i].section != 0) {
                if (count >= cap) {
                    cap = cap * 2 + 1;
                    symbols = realloc(symbols, cap * sizeof(symbol_entry_t));
                }
                symbols[count].name = strdup(obj_strings + symbols_in[i].name_offset);
                symbols[count].member_idx = (uint32_t)m;
                count++;
            }
        }
    }
    
    *out_count = count;
    return symbols;
}

static bool load_archive_members(const char *filename, archive_state_t *state) {
    FILE *f = fopen(filename, "rb");
    if (!f) {
        return false;
    }
    
    long archive_size = file_size_for(f);
    if (archive_size < (long)sizeof(s32a_header_t)) {
        fclose(f);
        fprintf(stderr, "Error: '%s' is not a valid SLOW-32 archive\n", filename);
        return false;
    }
    
    s32a_header_t hdr;
    if (!read_fully(&hdr, sizeof(hdr), f)) {
        fclose(f);
        fprintf(stderr, "Error: Cannot read archive header\n");
        return false;
    }
    
    if (hdr.magic != S32A_MAGIC) {
        fclose(f);
        fprintf(stderr, "Error: '%s' is not a valid SLOW-32 archive\n", filename);
        return false;
    }
    
    if (hdr.str_offset + hdr.str_size > (uint32_t)archive_size) {
        fclose(f);
        fprintf(stderr, "Error: Archive string table out of bounds\n");
        return false;
    }
    if (hdr.mem_offset + hdr.nmembers * sizeof(s32a_member_t) > (uint32_t)archive_size) {
        fclose(f);
        fprintf(stderr, "Error: Archive member table out of bounds\n");
        return false;
    }
    
    char *strings = malloc(hdr.str_size);
    fseek(f, hdr.str_offset, SEEK_SET);
    if (!read_fully(strings, hdr.str_size, f)) {
        free(strings);
        fclose(f);
        fprintf(stderr, "Error: Cannot read archive string table\n");
        return false;
    }
    
    fseek(f, hdr.mem_offset, SEEK_SET);
    for (uint32_t i = 0; i < hdr.nmembers; i++) {
        s32a_member_t mem;
        if (!read_fully(&mem, sizeof(mem), f)) {
            free(strings);
            fclose(f);
            fprintf(stderr, "Error: Cannot read archive member table\n");
            return false;
        }
        if (mem.name_offset >= hdr.str_size) {
            free(strings);
            fclose(f);
            fprintf(stderr, "Error: Invalid member name offset\n");
            return false;
        }
        if (mem.offset + mem.size > (uint32_t)archive_size) {
            free(strings);
            fclose(f);
            fprintf(stderr, "Error: Archive member data out of bounds\n");
            return false;
        }
        
        uint8_t *data = malloc(mem.size);
        long saved_pos = ftell(f);
        fseek(f, mem.offset, SEEK_SET);
        if (!read_fully(data, mem.size, f)) {
            free(data);
            free(strings);
            fclose(f);
            fprintf(stderr, "Error: Cannot read archive member data\n");
            return false;
        }
        fseek(f, saved_pos, SEEK_SET);
        
        const char *raw_name = strings + mem.name_offset;
        const char *safe_name = basename_simple(raw_name);
        if (!add_member_data(state, safe_name, data, mem.size, (time_t)mem.timestamp)) {
            free(strings);
            fclose(f);
            return false;
        }
    }
    
    free(strings);
    fclose(f);
    return true;
}

static void write_archive(archive_state_t *state, const char *output) {
    FILE *f = fopen(output, "wb");
    if (!f) {
        fprintf(stderr, "Error: Cannot create '%s'\n", output);
        return;
    }
    
    // Build string table for archive
    string_table_t archive_strings = {0};
    archive_strings.strings = malloc(4096);
    archive_strings.str_capacity = 4096;
    
    // Add empty string at offset 0
    add_string(&archive_strings, "");
    
    // Add member names
    uint32_t *member_name_offsets = malloc(state->nmembers * sizeof(uint32_t));
    for (size_t i = 0; i < state->nmembers; i++) {
        member_name_offsets[i] = add_string(&archive_strings, state->members[i].name);
    }
    
    size_t nsymbols = 0;
    symbol_entry_t *symbols = build_symbol_index(state, &nsymbols);

    // Add symbol names
    uint32_t *symbol_name_offsets = malloc(nsymbols * sizeof(uint32_t));
    for (size_t i = 0; i < nsymbols; i++) {
        symbol_name_offsets[i] = add_string(&archive_strings, symbols[i].name);
    }
    
    // Calculate offsets
    uint32_t offset = sizeof(s32a_header_t);
    uint32_t sym_offset = offset;
    offset += nsymbols * sizeof(s32a_symbol_t);
    uint32_t mem_offset = offset;
    offset += state->nmembers * sizeof(s32a_member_t);
    uint32_t str_offset = offset;
    offset += archive_strings.str_size;
    
    // Align to 4 bytes for member data
    offset = (offset + 3) & ~3;
    
    // Calculate member data offsets
    uint32_t *member_offsets = malloc(state->nmembers * sizeof(uint32_t));
    for (size_t i = 0; i < state->nmembers; i++) {
        member_offsets[i] = offset;
        offset += state->members[i].size;
        offset = (offset + 3) & ~3;  // Align each member
    }
    
    // Write header
    s32a_header_t hdr = {
        .magic = S32A_MAGIC,
        .version = 1,
        .endian = 0x01,
        .reserved = 0,
        .nmembers = state->nmembers,
        .mem_offset = mem_offset,
        .nsymbols = nsymbols,
        .sym_offset = sym_offset,
        .str_offset = str_offset,
        .str_size = archive_strings.str_size
    };
    fwrite(&hdr, sizeof(hdr), 1, f);
    
    // Write symbol index
    for (size_t i = 0; i < nsymbols; i++) {
        s32a_symbol_t sym = {
            .name_offset = symbol_name_offsets[i],
            .member_index = symbols[i].member_idx
        };
        fwrite(&sym, sizeof(sym), 1, f);
    }
    
    // Write member table
    for (size_t i = 0; i < state->nmembers; i++) {
        s32a_member_t mem = {
            .name_offset = member_name_offsets[i],
            .offset = member_offsets[i],
            .size = state->members[i].size,
            .timestamp = state->members[i].timestamp,
            .uid = getuid(),
            .gid = getgid()
        };
        fwrite(&mem, sizeof(mem), 1, f);
    }
    
    // Write string table
    fwrite(archive_strings.strings, 1, archive_strings.str_size, f);
    
    // Align to 4 bytes
    uint32_t current = ftell(f);
    uint32_t aligned = (current + 3) & ~3;
    while (current < aligned) {
        fputc(0, f);
        current++;
    }
    
    // Write member data
    for (size_t i = 0; i < state->nmembers; i++) {
        fwrite(state->members[i].data, 1, state->members[i].size, f);
        
        // Align to 4 bytes
        current = ftell(f);
        aligned = (current + 3) & ~3;
        while (current < aligned) {
            fputc(0, f);
            current++;
        }
    }
    
    fclose(f);
    
    // Cleanup
    free(member_name_offsets);
    free(symbol_name_offsets);
    free(member_offsets);
    free(archive_strings.strings);
    for (size_t i = 0; i < nsymbols; i++) {
        free(symbols[i].name);
    }
    free(symbols);
}

static void list_archive(const char *filename) {
    FILE *f = fopen(filename, "rb");
    if (!f) {
        fprintf(stderr, "Error: Cannot open '%s'\n", filename);
        return;
    }

    long archive_size = file_size_for(f);
    
    s32a_header_t hdr;
    if (!read_fully(&hdr, sizeof(hdr), f)) {
        fprintf(stderr, "Error: Cannot read archive header\n");
        fclose(f);
        return;
    }
    
    if (hdr.magic != S32A_MAGIC) {
        fprintf(stderr, "Error: '%s' is not a valid SLOW-32 archive\n", filename);
        fclose(f);
        return;
    }
    
    if (hdr.str_offset + hdr.str_size > (uint32_t)archive_size ||
        hdr.mem_offset + hdr.nmembers * sizeof(s32a_member_t) > (uint32_t)archive_size) {
        fprintf(stderr, "Error: Archive table out of bounds\n");
        fclose(f);
        return;
    }

    // Read string table
    char *strings = malloc(hdr.str_size);
    fseek(f, hdr.str_offset, SEEK_SET);
    if (!read_fully(strings, hdr.str_size, f)) {
        fprintf(stderr, "Error: Cannot read archive string table\n");
        free(strings);
        fclose(f);
        return;
    }
    
    // Read and display members
    fseek(f, hdr.mem_offset, SEEK_SET);
    for (uint32_t i = 0; i < hdr.nmembers; i++) {
        s32a_member_t mem;
        if (!read_fully(&mem, sizeof(mem), f)) {
            fprintf(stderr, "Error: Cannot read member table\n");
            free(strings);
            fclose(f);
            return;
        }
        if (mem.name_offset >= hdr.str_size ||
            mem.offset + mem.size > (uint32_t)archive_size) {
            fprintf(stderr, "Error: Archive member out of bounds\n");
            free(strings);
            fclose(f);
            return;
        }
        
        char timestamp[32];
        time_t ts = (time_t)mem.timestamp;
        strftime(timestamp, sizeof(timestamp), "%b %d %H:%M %Y", 
                localtime(&ts));
        
        printf("%8u %s %s\n", mem.size, timestamp, strings + mem.name_offset);
    }
    
    free(strings);
    fclose(f);
}

static void extract_archive(const char *archive, const char *member_name) {
    FILE *f = fopen(archive, "rb");
    if (!f) {
        fprintf(stderr, "Error: Cannot open '%s'\n", archive);
        return;
    }

    long archive_size = file_size_for(f);
    
    s32a_header_t hdr;
    if (!read_fully(&hdr, sizeof(hdr), f)) {
        fprintf(stderr, "Error: Cannot read archive header\n");
        fclose(f);
        return;
    }
    
    if (hdr.magic != S32A_MAGIC) {
        fprintf(stderr, "Error: '%s' is not a valid SLOW-32 archive\n", archive);
        fclose(f);
        return;
    }
    
    if (hdr.str_offset + hdr.str_size > (uint32_t)archive_size ||
        hdr.mem_offset + hdr.nmembers * sizeof(s32a_member_t) > (uint32_t)archive_size) {
        fprintf(stderr, "Error: Archive table out of bounds\n");
        fclose(f);
        return;
    }

    // Read string table
    char *strings = malloc(hdr.str_size);
    fseek(f, hdr.str_offset, SEEK_SET);
    if (!read_fully(strings, hdr.str_size, f)) {
        fprintf(stderr, "Error: Cannot read archive string table\n");
        free(strings);
        fclose(f);
        return;
    }
    
    // Find and extract member(s)
    fseek(f, hdr.mem_offset, SEEK_SET);
    int found = 0;
    
    for (uint32_t i = 0; i < hdr.nmembers; i++) {
        s32a_member_t mem;
        if (!read_fully(&mem, sizeof(mem), f)) {
            fprintf(stderr, "Error: Cannot read member table\n");
            free(strings);
            fclose(f);
            return;
        }
        
        if (mem.name_offset >= hdr.str_size ||
            mem.offset + mem.size > (uint32_t)archive_size) {
            fprintf(stderr, "Error: Archive member out of bounds\n");
            free(strings);
            fclose(f);
            return;
        }

        const char *name = strings + mem.name_offset;
        const char *safe_name = basename_simple(name);
        
        if (member_name == NULL || strcmp(name, member_name) == 0 || strcmp(safe_name, member_name) == 0) {
            // Extract this member
            FILE *out = fopen(safe_name, "wb");
            if (!out) {
                fprintf(stderr, "Error: Cannot create '%s'\n", safe_name);
                continue;
            }
            
            // Read and write member data
            uint8_t *data = malloc(mem.size);
            long saved_pos = ftell(f);
            fseek(f, mem.offset, SEEK_SET);
            if (!read_fully(data, mem.size, f)) {
                fprintf(stderr, "Error: Cannot read member data\n");
                free(data);
                fclose(out);
                fseek(f, saved_pos, SEEK_SET);
                continue;
            }
            fwrite(data, 1, mem.size, out);
            fclose(out);
            free(data);
            
            // Restore file position
            fseek(f, saved_pos, SEEK_SET);
            
            printf("x - %s\n", safe_name);
            found = 1;
            
            if (member_name != NULL) break;
        }
    }
    
    if (!found && member_name != NULL) {
        fprintf(stderr, "Error: Member '%s' not found in archive\n", member_name);
    }
    
    free(strings);
    fclose(f);
}

static void usage(const char *prog) {
    fprintf(stderr, "Usage: %s <operation> archive [files...]\n", prog);
    fprintf(stderr, "Operations:\n");
    fprintf(stderr, "  r - insert/replace files in archive\n");
    fprintf(stderr, "  t - list archive contents\n");
    fprintf(stderr, "  x - extract files from archive\n");
    fprintf(stderr, "  c - create archive (same as r)\n");
    exit(1);
}

int main(int argc, char **argv) {
    if (argc < 3) {
        usage(argv[0]);
    }
    
    const char *op = argv[1];
    const char *archive = argv[2];
    
    if (strchr(op, 't')) {
        // List archive contents
        list_archive(archive);
    } else if (strchr(op, 'x')) {
        // Extract files
        if (argc > 3) {
            for (int i = 3; i < argc; i++) {
                extract_archive(archive, argv[i]);
            }
        } else {
            extract_archive(archive, NULL);  // Extract all
        }
    } else if (strchr(op, 'r') || strchr(op, 'c')) {
        // Create/update archive
        if (argc < 4) {
            fprintf(stderr, "Error: No files specified\n");
            return 1;
        }
        
        archive_state_t state = {0};
        bool loaded = false;

        if (access(archive, F_OK) == 0) {
            loaded = load_archive_members(archive, &state);
            if (!loaded) {
                fprintf(stderr, "Error: Failed to load existing archive '%s'\n", archive);
                return 1;
            }
        }
        
        // Add all specified files
        for (int i = 3; i < argc; i++) {
            add_or_replace_member(&state, argv[i]);
        }
        
        // Write archive
        write_archive(&state, archive);
        
        // Cleanup
        for (size_t i = 0; i < state.nmembers; i++) {
            free(state.members[i].name);
            free(state.members[i].data);
        }
        free(state.members);
        
        if (loaded) {
            printf("Archive '%s' updated with %zu members\n", archive, state.nmembers);
        } else {
            printf("Archive '%s' created with %zu members\n", archive, state.nmembers);
        }
    } else {
        fprintf(stderr, "Error: Unknown operation '%s'\n", op);
        usage(argv[0]);
    }
    
    return 0;
}
