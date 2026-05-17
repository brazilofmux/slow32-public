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

#define MAX_MEM 64
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

static bool add_member_data(archive_state_t *state, const char *name, uint8_t *data, size_t size, uint32_t ts) {
    member_t *member;
    if (state->nmembers >= state->members_capacity) {
        printf("too many\n");
        free(data);
        return false;
    }
    member = &state->members[state->nmembers];
    member->name = dup_cstr(name);
    if (!member->name) {
        printf("dup fail\n");
        free(data);
        return false;
    }
    member->data = data;
    member->size = size;
    member->timestamp = ts;
    state->nmembers++;
    return true;
}

int main(int argc, char **argv) {
    archive_state_t st;
    FILE *f;
    long sz;
    uint8_t *data;

    if (argc < 4) return 1;
    st.members = g_members;
    st.nmembers = 0;
    st.members_capacity = MAX_MEM;

    f = fopen(argv[3], "rb");
    if (!f) { printf("open fail\n"); return 1; }
    sz = file_size_for(f);
    if (sz <= 0) { fclose(f); return 1; }
    fseek(f, 0, SEEK_SET);
    data = (uint8_t *)malloc((size_t)sz);
    if (!data) { fclose(f); return 1; }
    if (fread(data, 1, (size_t)sz, f) != (size_t)sz) { fclose(f); free(data); return 1; }
    fclose(f);

    if (!add_member_data(&st, "divsi3.s32o", data, (size_t)sz, 0)) {
        return 1;
    }

    printf("ok\n");
    return 0;
}
