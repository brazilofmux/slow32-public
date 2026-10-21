/* Host WK1 load: IEEE bits and oversized records (GitHub issue 115). */
#include "sheet.h"

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

static int wr16(FILE *f, uint16_t v) {
    if (fputc(v & 0xff, f) == EOF || fputc((v >> 8) & 0xff, f) == EOF) {
        return -1;
    }
    return 0;
}

static int wr_rec(FILE *f, uint16_t op, const uint8_t *data, uint16_t len) {
    if (wr16(f, op) < 0 || wr16(f, len) < 0) {
        return -1;
    }
    if (len > 0 && fwrite(data, 1, len, f) != len) {
        return -1;
    }
    return 0;
}

int main(void) {
    const char *path = "host_wk1.tmp";
    FILE *f;
    sheet_t sh;
    uint8_t rec[2048];
    double big = 3000000000.0;
    double pi = 3.141592653589793;
    int fail = 0;
    cell_t *c;

    memset(rec, 0, sizeof(rec));
    f = fopen(path, "wb");
    if (!f) {
        perror(path);
        return 1;
    }
    rec[0] = 0x06;
    rec[1] = 0x04;
    wr_rec(f, 0x0000, rec, 2); /* BOF WK1 */

    rec[0] = 0xFF;
    rec[1] = 0;
    rec[2] = 0;
    rec[3] = 0;
    rec[4] = 0;
    memcpy(rec + 5, &big, 8);
    wr_rec(f, 0x000E, rec, 13); /* A1 NUMBER 3e9 */

    rec[1] = 1;
    memcpy(rec + 5, &pi, 8);
    wr_rec(f, 0x000E, rec, 13); /* B1 NUMBER pi */

    /* Oversized FORMULA at C1: 600-byte body, cached 42.0 at the front. */
    {
        double forty_two = 42.0;
        memset(rec, 0, sizeof(rec));
        rec[0] = 0xFF;
        rec[1] = 2;
        rec[2] = 0;
        rec[3] = 0;
        rec[4] = 0;
        memcpy(rec + 5, &forty_two, 8);
        wr_rec(f, 0x0010, rec, 600);
    }

    wr_rec(f, 0x0001, NULL, 0);
    fclose(f);

    sheet_init(&sh);
    if (sheet_load_wk1(&sh, path) != 0) {
        fprintf(stderr, "FAIL load\n");
        remove(path);
        return 1;
    }
    c = sheet_cell(&sh, 0, 0);
    if (!c || c->kind != CELL_NUM || c->value != big) {
        fprintf(stderr, "FAIL A1 got kind=%d v=%g\n", c ? c->kind : -1,
                c ? c->value : 0);
        fail = 1;
    }
    c = sheet_cell(&sh, 1, 0);
    if (!c || c->kind != CELL_NUM || c->value != pi) {
        fprintf(stderr, "FAIL B1 got v=%.17g want %.17g\n",
                c ? c->value : 0, pi);
        fail = 1;
    }
    c = sheet_cell(&sh, 2, 0);
    if (!c || c->kind != CELL_NUM || c->value != 42.0) {
        fprintf(stderr, "FAIL C1 oversized formula got v=%g\n",
                c ? c->value : 0);
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
