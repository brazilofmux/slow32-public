#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
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

typedef struct {
    char *name;
    uint32_t member_idx;
} symbol_entry_t;

// Global state for building archive
typedef struct {
    member_t *members;
    size_t nmembers;
    size_t members_capacity;
    
    symbol_entry_t *symbols;
    size_t nsymbols;
    size_t symbols_capacity;
    
    char *strings;
    size_t str_size;
    size_t str_capacity;
} archive_state_t;

static uint32_t add_string(archive_state_t *state, const char *str) {
    size_t len = strlen(str) + 1;
    
    // Check if string already exists
    size_t offset = 0;
    while (offset < state->str_size) {
        if (strcmp(state->strings + offset, str) == 0) {
            return offset;
        }
        offset += strlen(state->strings + offset) + 1;
    }
    
    // Add new string
    if (state->str_size + len > state->str_capacity) {
        state->str_capacity = (state->str_capacity + len) * 2;
        state->strings = realloc(state->strings, state->str_capacity);
    }
    
    uint32_t result = state->str_size;
    strcpy(state->strings + state->str_size, str);
    state->str_size += len;
    return result;
}

static void add_member(archive_state_t *state, const char *filename) {
    FILE *f = fopen(filename, "rb");
    if (!f) {
        fprintf(stderr, "Error: Cannot open '%s'\n", filename);
        return;
    }
    
    // Get file size and timestamp
    struct stat st;
    fstat(fileno(f), &st);
    
    // Read file data
    uint8_t *data = malloc(st.st_size);
    fread(data, 1, st.st_size, f);
    fclose(f);
    
    // Verify it's a valid .s32o file
    if (st.st_size < sizeof(s32o_header_t)) {
        fprintf(stderr, "Error: '%s' is too small to be a valid object file\n", filename);
        free(data);
        return;
    }
    
    s32o_header_t *hdr = (s32o_header_t *)data;
    if (hdr->magic != S32O_MAGIC) {
        fprintf(stderr, "Error: '%s' is not a valid SLOW-32 object file\n", filename);
        free(data);
        return;
    }
    
    // Add to members array
    if (state->nmembers >= state->members_capacity) {
        state->members_capacity = state->members_capacity * 2 + 1;
        state->members = realloc(state->members, state->members_capacity * sizeof(member_t));
    }
    
    member_t *member = &state->members[state->nmembers];
    member->name = strdup(filename);
    member->data = data;
    member->size = st.st_size;
    member->timestamp = st.st_mtime;
    
    // Extract symbols for index
    uint32_t member_idx = state->nmembers;
    s32o_symbol_t *symbols = (s32o_symbol_t *)(data + hdr->sym_offset);
    char *obj_strings = (char *)(data + hdr->str_offset);
    
    for (uint32_t i = 0; i < hdr->nsymbols; i++) {
        if (symbols[i].binding == S32O_BIND_GLOBAL && symbols[i].section != 0) {
            // Add to symbol index
            if (state->nsymbols >= state->symbols_capacity) {
                state->symbols_capacity = state->symbols_capacity * 2 + 1;
                state->symbols = realloc(state->symbols, state->symbols_capacity * sizeof(symbol_entry_t));
            }
            
            state->symbols[state->nsymbols].name = strdup(obj_strings + symbols[i].name_offset);
            state->symbols[state->nsymbols].member_idx = member_idx;
            state->nsymbols++;
        }
    }
    
    state->nmembers++;
}

static void write_archive(archive_state_t *state, const char *output) {
    FILE *f = fopen(output, "wb");
    if (!f) {
        fprintf(stderr, "Error: Cannot create '%s'\n", output);
        return;
    }
    
    // Build string table for archive
    archive_state_t archive_strings = {0};
    archive_strings.strings = malloc(4096);
    archive_strings.str_capacity = 4096;
    
    // Add empty string at offset 0
    add_string(&archive_strings, "");
    
    // Add member names
    uint32_t *member_name_offsets = malloc(state->nmembers * sizeof(uint32_t));
    for (size_t i = 0; i < state->nmembers; i++) {
        member_name_offsets[i] = add_string(&archive_strings, state->members[i].name);
    }
    
    // Add symbol names
    uint32_t *symbol_name_offsets = malloc(state->nsymbols * sizeof(uint32_t));
    for (size_t i = 0; i < state->nsymbols; i++) {
        symbol_name_offsets[i] = add_string(&archive_strings, state->symbols[i].name);
    }
    
    // Calculate offsets
    uint32_t offset = sizeof(s32a_header_t);
    uint32_t sym_offset = offset;
    offset += state->nsymbols * sizeof(s32a_symbol_t);
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
        .nsymbols = state->nsymbols,
        .sym_offset = sym_offset,
        .str_offset = str_offset,
        .str_size = archive_strings.str_size
    };
    fwrite(&hdr, sizeof(hdr), 1, f);
    
    // Write symbol index
    for (size_t i = 0; i < state->nsymbols; i++) {
        s32a_symbol_t sym = {
            .name_offset = symbol_name_offsets[i],
            .member_index = state->symbols[i].member_idx
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
}

static void list_archive(const char *filename) {
    FILE *f = fopen(filename, "rb");
    if (!f) {
        fprintf(stderr, "Error: Cannot open '%s'\n", filename);
        return;
    }
    
    s32a_header_t hdr;
    fread(&hdr, sizeof(hdr), 1, f);
    
    if (hdr.magic != S32A_MAGIC) {
        fprintf(stderr, "Error: '%s' is not a valid SLOW-32 archive\n", filename);
        fclose(f);
        return;
    }
    
    // Read string table
    char *strings = malloc(hdr.str_size);
    fseek(f, hdr.str_offset, SEEK_SET);
    fread(strings, 1, hdr.str_size, f);
    
    // Read and display members
    fseek(f, hdr.mem_offset, SEEK_SET);
    for (uint32_t i = 0; i < hdr.nmembers; i++) {
        s32a_member_t mem;
        fread(&mem, sizeof(mem), 1, f);
        
        char timestamp[32];
        strftime(timestamp, sizeof(timestamp), "%b %d %H:%M %Y", 
                localtime((time_t*)&mem.timestamp));
        
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
    
    s32a_header_t hdr;
    fread(&hdr, sizeof(hdr), 1, f);
    
    if (hdr.magic != S32A_MAGIC) {
        fprintf(stderr, "Error: '%s' is not a valid SLOW-32 archive\n", archive);
        fclose(f);
        return;
    }
    
    // Read string table
    char *strings = malloc(hdr.str_size);
    fseek(f, hdr.str_offset, SEEK_SET);
    fread(strings, 1, hdr.str_size, f);
    
    // Find and extract member(s)
    fseek(f, hdr.mem_offset, SEEK_SET);
    int found = 0;
    
    for (uint32_t i = 0; i < hdr.nmembers; i++) {
        s32a_member_t mem;
        fread(&mem, sizeof(mem), 1, f);
        
        const char *name = strings + mem.name_offset;
        
        if (member_name == NULL || strcmp(name, member_name) == 0) {
            // Extract this member
            FILE *out = fopen(name, "wb");
            if (!out) {
                fprintf(stderr, "Error: Cannot create '%s'\n", name);
                continue;
            }
            
            // Read and write member data
            uint8_t *data = malloc(mem.size);
            long saved_pos = ftell(f);
            fseek(f, mem.offset, SEEK_SET);
            fread(data, 1, mem.size, f);
            fwrite(data, 1, mem.size, out);
            fclose(out);
            free(data);
            
            // Restore file position
            fseek(f, saved_pos, SEEK_SET);
            
            printf("x - %s\n", name);
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
        
        // Add all specified files
        for (int i = 3; i < argc; i++) {
            add_member(&state, argv[i]);
        }
        
        // Write archive
        write_archive(&state, archive);
        
        // Cleanup
        for (size_t i = 0; i < state.nmembers; i++) {
            free(state.members[i].name);
            free(state.members[i].data);
        }
        for (size_t i = 0; i < state.nsymbols; i++) {
            free(state.symbols[i].name);
        }
        free(state.members);
        free(state.symbols);
        free(state.strings);
        
        printf("Archive '%s' created with %zu members\n", archive, state.nmembers);
    } else {
        fprintf(stderr, "Error: Unknown operation '%s'\n", op);
        usage(argv[0]);
    }
    
    return 0;
}