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

static bool write_archive(archive_state_t *state, const char *out_name) {
    (void)state;
    (void)out_name;
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
    st.nmembers = 1;
    st.members_capacity = MAX_MEM;
    st.members[0].name = "divsi3.s32o";
    st.members[0].data = data;
    st.members[0].size = (size_t)sz;
    st.members[0].timestamp = 0;

    return 18;
}
