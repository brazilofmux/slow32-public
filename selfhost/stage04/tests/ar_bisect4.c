#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

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

int main(int argc, char **argv) {
    const char *obj = "divsi3.s32o";
    const char *arc = "work.s32a";
    FILE *in;
    FILE *out;
    long sz;
    uint8_t *data;
    uint32_t str_off = 32 + 8 + 24;
    uint32_t str_sz = 13;
    uint32_t data_off = str_off + str_sz;
    int i;

    if (argc >= 3) arc = argv[2];
    if (argc >= 4) obj = argv[3];

    in = fopen(obj, "rb");
    if (!in) { printf("open in fail\n"); return 1; }
    sz = file_size_for(in);
    if (sz <= 0) { fclose(in); return 1; }
    fseek(in, 0, SEEK_SET);
    data = (uint8_t *)malloc((size_t)sz);
    if (!data) { fclose(in); return 1; }
    if (fread(data, 1, (size_t)sz, in) != (size_t)sz) { fclose(in); free(data); return 1; }
    fclose(in);

    out = fopen(arc, "wb");
    if (!out) { free(data); return 1; }

    fputc('A', out); fputc('2', out); fputc('3', out); fputc('S', out);
    w16(out, 1); fputc(1, out); fputc(0, out);
    w32(out, 1);              /* nmembers */
    w32(out, 40);             /* mem_offset */
    w32(out, 1);              /* nsymbols */
    w32(out, 32);             /* sym_offset */
    w32(out, str_off);        /* str_offset */
    w32(out, str_sz);         /* str_size */

    w32(out, 1);              /* sym name_offset */
    w32(out, 0);              /* sym member_index */

    w32(out, 1);              /* member name_offset */
    w32(out, data_off);       /* member offset */
    w32(out, (uint32_t)sz);   /* member size */
    w32(out, 0);              /* timestamp */
    w32(out, 0);              /* uid */
    w32(out, 0);              /* gid */

    fputc(0, out);
    fputc('d', out); fputc('i', out); fputc('v', out); fputc('s', out); fputc('i', out);
    fputc('3', out); fputc('.', out); fputc('s', out); fputc('3', out); fputc('2', out);
    fputc('o', out); fputc(0, out);

    for (i = 0; i < (int)sz; i++) {
        fputc(data[i], out);
    }

    fclose(out);
    free(data);
    printf("wrote\n");
    return 0;
}
