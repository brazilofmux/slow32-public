#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>

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

#define MAX_MEM 8
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

static void w16(FILE *f, uint16_t v) {
    fputc((int)(v & 0xFF), f);
    fputc((int)((v >> 8) & 0xFF), f);
}

static void w32(FILE *f, uint32_t v) {
    fputc((int)(v & 0xFF), f);
    fputc((int)((v >> 8) & 0xFF), f);
    fputc((int)((v >> 16) & 0xFF), f);
    fputc((int)((v >> 24) & 0xFF), f);
}

static bool add_member_data(archive_state_t *state, const char *name, uint8_t *data, size_t size, uint32_t ts) {
    member_t *m;
    if (state->nmembers >= state->members_capacity) {
        return false;
    }
    m = &state->members[state->nmembers];
    m->name = dup_cstr(name);
    if (!m->name) {
        return false;
    }
    m->data = data;
    m->size = size;
    m->timestamp = ts;
    state->nmembers++;
    return true;
}

static bool write_archive(archive_state_t *state, const char *out_name) {
    FILE *f;
    const char *n;
    uint8_t *payload;
    uint32_t timestamp;
    uint32_t msize;
    uint32_t nmembers;
    uint32_t nsymbols;
    uint32_t sym_off;
    uint32_t mem_off;
    uint32_t str_off;
    uint32_t str_sz;
    uint32_t data_off;
    uint32_t i;

    if (state->nmembers != 1) return false;
    nmembers = state->nmembers;
    nsymbols = 0;
    sym_off = 32;
    mem_off = sym_off + nsymbols * 8;
    str_off = mem_off + nmembers * 24;

    /* string table: "" + member name + "\0" */
    str_sz = 1 + (uint32_t)strlen(state->members[0].name) + 1;
    data_off = str_off + str_sz;

    f = fopen(out_name, "wb");
    if (!f) return false;

    /* s32a header */
    fputc('A', f); fputc('2', f); fputc('3', f); fputc('S', f);
    w16(f, 1); fputc(1, f); fputc(0, f);
    w32(f, nmembers);
    w32(f, mem_off);
    w32(f, nsymbols);
    w32(f, sym_off);
    w32(f, str_off);
    w32(f, str_sz);

    /* no symbol table (nsymbols = 0) */

    /* member table: name offset=1 */
    msize = state->members[0].size;
    timestamp = state->members[0].timestamp;
    w32(f, 1);
    w32(f, data_off);
    w32(f, msize);
    w32(f, timestamp);
    w32(f, 0);
    w32(f, 0);

    /* string table */
    fputc(0, f);
    n = state->members[0].name;
    while (*n) {
        fputc(*n, f);
        n++;
    }
    fputc(0, f);

    /* payload */
    payload = state->members[0].data;
    for (i = 0; i < msize; i++) {
        fputc(payload[i], f);
    }

    fclose(f);
    return true;
}

int main(int argc, char **argv) {
    archive_state_t st;
    const char *obj = "divsi3.s32o";
    const char *arc = "work.s32a";
    FILE *in;
    long sz;
    uint8_t *data;

    if (argc >= 3) arc = argv[2];
    if (argc >= 4) obj = argv[3];

    in = fopen(obj, "rb");
    if (!in) {
        return 11;
    }
    sz = file_size_for(in);
    if (sz <= 0) {
        fclose(in);
        return 12;
    }
    if (fseek(in, 0, SEEK_SET) != 0) {
        fclose(in);
        return 13;
    }

    data = (uint8_t *)malloc((size_t)sz);
    if (!data) {
        fclose(in);
        return 14;
    }
    if (fread(data, 1, (size_t)sz, in) != (size_t)sz) {
        free(data);
        fclose(in);
        return 15;
    }
    fclose(in);

    st.members = g_members;
    st.nmembers = 0;
    st.members_capacity = MAX_MEM;

    st.members[0].name = "divsi3.s32o";
    st.members[0].data = data;
    st.members[0].size = (size_t)sz;
    st.members[0].timestamp = 0;
    st.nmembers = 1;
    return 19;

    if (!write_archive(&st, arc)) {
        free(data);
        return 17;
    }
    return 0;
}
