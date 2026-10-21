/* Host DBF load: C stays a label; deleted records pack (GitHub issue 117). */
#include "sheet.h"

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

static void wr16(uint8_t *p, uint16_t v) {
    p[0] = (uint8_t)(v & 0xff);
    p[1] = (uint8_t)((v >> 8) & 0xff);
}

static void wr32(uint8_t *p, uint32_t v) {
    p[0] = (uint8_t)(v & 0xff);
    p[1] = (uint8_t)((v >> 8) & 0xff);
    p[2] = (uint8_t)((v >> 16) & 0xff);
    p[3] = (uint8_t)((v >> 24) & 0xff);
}

int main(void) {
    const char *path = "host_dbf.tmp";
    FILE *f;
    uint8_t hdr[32];
    uint8_t fdesc[32];
    uint8_t rec[1 + 5 + 5];
    uint8_t term;
    sheet_t sh;
    cell_t *c;
    int fail = 0;

    memset(hdr, 0, sizeof(hdr));
    hdr[0] = 0x03;
    wr32(hdr + 4, 3);          /* 3 records */
    wr16(hdr + 8, 32 + 64 + 1); /* 2 fields */
    wr16(hdr + 10, 1 + 5 + 5);

    f = fopen(path, "wb");
    if (!f) {
        perror(path);
        return 1;
    }
    fwrite(hdr, 1, 32, f);

    memset(fdesc, 0, sizeof(fdesc));
    fdesc[0] = 'A';
    fdesc[11] = 'C';
    fdesc[16] = 5;
    fwrite(fdesc, 1, 32, f);

    memset(fdesc, 0, sizeof(fdesc));
    fdesc[0] = 'B';
    fdesc[11] = 'N';
    fdesc[16] = 5;
    fdesc[17] = 0;
    fwrite(fdesc, 1, 32, f);

    term = 0x0D;
    fwrite(&term, 1, 1, f);

    /* rec 0 live: C "00123", N 42 */
    rec[0] = ' ';
    memcpy(rec + 1, "00123", 5);
    memcpy(rec + 6, "   42", 5);
    fwrite(rec, 1, 11, f);

    /* rec 1 deleted */
    rec[0] = '*';
    memcpy(rec + 1, "gone ", 5);
    memcpy(rec + 6, "    0", 5);
    fwrite(rec, 1, 11, f);

    /* rec 2 live: C "zz", N 7 */
    rec[0] = ' ';
    memcpy(rec + 1, "zz   ", 5);
    memcpy(rec + 6, "    7", 5);
    fwrite(rec, 1, 11, f);

    term = 0x1A;
    fwrite(&term, 1, 1, f);
    fclose(f);

    sheet_init(&sh);
    if (sheet_load_dbf(&sh, path) != 0) {
        fprintf(stderr, "FAIL load\n");
        remove(path);
        return 1;
    }
    c = sheet_cell(&sh, 0, 0);
    if (!c || c->kind != CELL_LABEL || !c->raw || strcmp(c->raw, "00123") != 0) {
        fprintf(stderr, "FAIL A1 C-field kind=%d raw=%s\n",
                c ? c->kind : -1, c && c->raw ? c->raw : "");
        fail = 1;
    }
    c = sheet_cell(&sh, 1, 0);
    if (!c || c->kind != CELL_NUM || c->value != 42.0) {
        fprintf(stderr, "FAIL B1 N-field v=%g\n", c ? c->value : 0);
        fail = 1;
    }
    c = sheet_cell(&sh, 0, 1);
    if (!c || c->kind != CELL_LABEL || !c->raw || strcmp(c->raw, "zz") != 0) {
        fprintf(stderr, "FAIL A2 packed over deleted kind=%d raw=%s\n",
                c ? c->kind : -1, c && c->raw ? c->raw : "");
        fail = 1;
    }
    c = sheet_cell(&sh, 1, 1);
    if (!c || c->kind != CELL_NUM || c->value != 7.0) {
        fprintf(stderr, "FAIL B2 v=%g\n", c ? c->value : 0);
        fail = 1;
    }
    c = sheet_cell(&sh, 0, 2);
    if (c && c->kind != CELL_EMPTY) {
        fprintf(stderr, "FAIL A3 should be empty after packing deleted\n");
        fail = 1;
    }
    sheet_clear(&sh);
    remove(path);
    if (fail) {
        return 1;
    }
    printf("ok\n");
    return 0;
}
