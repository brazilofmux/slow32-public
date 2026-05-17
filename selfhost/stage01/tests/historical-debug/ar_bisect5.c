#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <s32_formats.h>

typedef struct {
    char *name;
    uint8_t *data;
    size_t size;
    uint32_t timestamp;
} member_t;

typedef struct {
    member_t *members;
    size_t nmembers;
    size_t members_capacity;
} archive_state_t;

typedef struct {
    char *name;
    uint32_t member_idx;
} symbol_entry_t;

#define MAX_MEM 4
static member_t g_members[MAX_MEM];

static long file_size_for(FILE *f) {
    long pos = ftell(f);
    if (pos < 0) return -1;
    if (fseek(f, 0, SEEK_END) != 0) return -1;
    long size = ftell(f);
    fseek(f, pos, SEEK_SET);
    return size;
}

static char *dup_cstr(const char *s) {
    size_t n = strlen(s) + 1;
    char *p = (char *)malloc(n);
    if (!p) return NULL;
    memcpy(p, s, n);
    return p;
}

static symbol_entry_t *build_symbol_index(archive_state_t *state, size_t *out_count) {
    size_t cap = 32;
    size_t count = 0;
    symbol_entry_t *symbols = (symbol_entry_t *)malloc(cap * sizeof(symbol_entry_t));
    size_t m;

    if (!symbols) {
        *out_count = 0;
        return NULL;
    }

    for (m = 0; m < state->nmembers; m++) {
        member_t *member = &state->members[m];
        s32o_header_t hdr;
        uint32_t i;
        s32o_symbol_t *symbols_in;
        char *obj_strings;

        if (!member->data || member->size < sizeof(s32o_header_t)) continue;
        memcpy(&hdr, member->data, sizeof(hdr));
        if (!(member->data[0] == 'O' && member->data[1] == '2' && member->data[2] == '3' && member->data[3] == 'S')) continue;
        if (hdr.sym_offset + hdr.nsymbols * sizeof(s32o_symbol_t) > member->size) continue;
        if (hdr.str_offset + hdr.str_size > member->size) continue;

        symbols_in = (s32o_symbol_t *)(member->data + hdr.sym_offset);
        obj_strings = (char *)(member->data + hdr.str_offset);

        for (i = 0; i < hdr.nsymbols; i++) {
            if (symbols_in[i].binding == S32O_BIND_GLOBAL && symbols_in[i].section != 0) {
                if (count >= cap) {
                    *out_count = count;
                    return symbols;
                }
                symbols[count].name = dup_cstr(obj_strings + symbols_in[i].name_offset);
                symbols[count].member_idx = (uint32_t)m;
                count++;
            }
        }
    }

    *out_count = count;
    return symbols;
}

int main(int argc, char **argv) {
    archive_state_t st;
    FILE *f;
    long sz;
    uint8_t *data;
    size_t nsym;
    symbol_entry_t *syms;

    if (argc < 4) return 1;

    f = fopen(argv[3], "rb");
    if (!f) { printf("open fail\n"); return 1; }
    sz = file_size_for(f);
    if (sz <= 0) { fclose(f); return 1; }
    fseek(f, 0, SEEK_SET);
    data = (uint8_t *)malloc((size_t)sz);
    if (!data) { fclose(f); return 1; }
    if (fread(data, 1, (size_t)sz, f) != (size_t)sz) { fclose(f); free(data); return 1; }
    fclose(f);

    st.members = g_members;
    st.nmembers = 1;
    st.members_capacity = MAX_MEM;
    st.members[0].name = "divsi3.s32o";
    st.members[0].data = data;
    st.members[0].size = (size_t)sz;
    st.members[0].timestamp = 0;

    syms = build_symbol_index(&st, &nsym);
    printf("nsym=%d\n", (int)nsym);
    if (syms) free(syms);
    free(data);
    return 0;
}
