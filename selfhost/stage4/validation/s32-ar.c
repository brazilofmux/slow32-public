#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

static long file_size_for(FILE *f) {
    long pos = ftell(f);
    long size;
    if (pos < 0) return -1;
    if (fseek(f, 0, SEEK_END) != 0) return -1;
    size = ftell(f);
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

static int create_archive(const char *archive, const char *obj) {
    FILE *in;
    FILE *out;
    long sz;
    uint8_t *data;
    uint32_t str_off = 32u + 8u + 24u;
    uint32_t str_sz = 13u;
    uint32_t data_off = str_off + str_sz;
    int i;

    in = fopen(obj, "rb");
    if (!in) return 1;
    sz = file_size_for(in);
    if (sz <= 0) {
        fclose(in);
        return 1;
    }
    if (fseek(in, 0, SEEK_SET) != 0) {
        fclose(in);
        return 1;
    }

    data = (uint8_t *)malloc((size_t)sz);
    if (!data) {
        fclose(in);
        return 1;
    }
    if (fread(data, 1, (size_t)sz, in) != (size_t)sz) {
        fclose(in);
        free(data);
        return 1;
    }
    fclose(in);

    out = fopen(archive, "wb");
    if (!out) {
        free(data);
        return 1;
    }

    fputc('A', out); fputc('2', out); fputc('3', out); fputc('S', out);
    w16(out, 1); fputc(1, out); fputc(0, out);
    w32(out, 1);        /* nmembers */
    w32(out, 40);       /* mem_offset */
    w32(out, 1);        /* nsymbols */
    w32(out, 32);       /* sym_offset */
    w32(out, str_off);  /* str_offset */
    w32(out, str_sz);   /* str_size */

    w32(out, 1);        /* sym name_offset */
    w32(out, 0);        /* sym member_index */

    w32(out, 1);                /* member name_offset */
    w32(out, data_off);         /* member offset */
    w32(out, (uint32_t)sz);     /* member size */
    w32(out, 0);                /* timestamp */
    w32(out, 0);                /* uid */
    w32(out, 0);                /* gid */

    fputc(0, out);
    fputc('d', out); fputc('i', out); fputc('v', out); fputc('s', out);
    fputc('i', out); fputc('3', out); fputc('.', out); fputc('s', out);
    fputc('3', out); fputc('2', out); fputc('o', out); fputc(0, out);

    for (i = 0; i < (int)sz; i++) {
        fputc(data[i], out);
    }

    fclose(out);
    free(data);
    return 0;
}

static void usage(const char *prog) {
    fprintf(stderr, "Usage: %s c archive file\n", prog);
}

int main(int argc, char **argv) {
    if (argc < 4) {
        usage(argv[0]);
        return 1;
    }
    if (argv[1][0] != 'c') {
        usage(argv[0]);
        return 1;
    }
    return create_archive(argv[2], argv[3]);
}
