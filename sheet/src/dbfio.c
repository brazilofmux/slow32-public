/*
 * dBase III .DBF interchange for the sheet.
 *
 * Columns A.. become fields A.. . Used rows become records.
 * A column is numeric (N) if it has no labels; otherwise character (C).
 * Formulas export their cached value. dBase III header 0x03, no memo.
 */

#include "sheet.h"

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define DBF_MAX_FIELDS 26
#define DBF_N_LEN      16
#define DBF_C_LEN      24

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

static uint16_t rd16(const uint8_t *p) {
    return (uint16_t)(p[0] | (p[1] << 8));
}

static uint32_t rd32(const uint8_t *p) {
    return (uint32_t)p[0] | ((uint32_t)p[1] << 8) |
           ((uint32_t)p[2] << 16) | ((uint32_t)p[3] << 24);
}

static void used_range(const sheet_t *sh, int *cmax, int *rmax) {
    int r, c;
    *cmax = -1;
    *rmax = -1;
    for (r = 0; r < SHEET_ROWS; r++) {
        for (c = 0; c < SHEET_COLS; c++) {
            if (sh->cells[r][c].kind != CELL_EMPTY) {
                if (c > *cmax) *cmax = c;
                if (r > *rmax) *rmax = r;
            }
        }
    }
}

static int col_is_numeric(const sheet_t *sh, int c, int rmax) {
    int r;
    for (r = 0; r <= rmax; r++) {
        if (sh->cells[r][c].kind == CELL_LABEL) {
            return 0;
        }
    }
    return 1;
}

static void pad_right(char *dst, int len, const char *s) {
    int n = (int)strlen(s);
    int pad;
    if (n > len) {
        n = len;
    }
    pad = len - n;
    memset(dst, ' ', (size_t)pad);
    memcpy(dst + pad, s, (size_t)n);
}

static void pad_left(char *dst, int len, const char *s) {
    int n = (int)strlen(s);
    if (n > len) {
        n = len;
    }
    memcpy(dst, s, (size_t)n);
    memset(dst + n, ' ', (size_t)(len - n));
}

int sheet_save_dbf(const sheet_t *sh, const char *path) {
    FILE *f;
    int cmax, rmax;
    int nf, r, c;
    int is_num[DBF_MAX_FIELDS];
    int flen[DBF_MAX_FIELDS];
    uint16_t rec_size;
    uint16_t hdr_size;
    uint8_t hdr[32];
    uint8_t fdesc[32];
    uint8_t rec[1 + DBF_MAX_FIELDS * DBF_C_LEN];
    uint8_t term;
    uint32_t nrec;

    used_range(sh, &cmax, &rmax);
    if (cmax < 0 || rmax < 0) {
        return -1;
    }
    nf = cmax + 1;
    if (nf > DBF_MAX_FIELDS) {
        nf = DBF_MAX_FIELDS;
    }

    rec_size = 1;
    for (c = 0; c < nf; c++) {
        is_num[c] = col_is_numeric(sh, c, rmax);
        flen[c] = is_num[c] ? DBF_N_LEN : DBF_C_LEN;
        rec_size = (uint16_t)(rec_size + flen[c]);
    }
    hdr_size = (uint16_t)(32 + nf * 32 + 1);
    nrec = (uint32_t)(rmax + 1);

    f = fopen(path, "w");
    if (!f) {
        return -1;
    }

    memset(hdr, 0, sizeof(hdr));
    hdr[0] = 0x03;
    hdr[1] = 126;
    hdr[2] = 8;
    hdr[3] = 15;
    wr32(hdr + 4, nrec);
    wr16(hdr + 8, hdr_size);
    wr16(hdr + 10, rec_size);
    if (fwrite(hdr, 1, 32, f) != 32) {
        fclose(f);
        return -1;
    }

    for (c = 0; c < nf; c++) {
        memset(fdesc, 0, sizeof(fdesc));
        fdesc[0] = (uint8_t)('A' + c);
        fdesc[11] = (uint8_t)(is_num[c] ? 'N' : 'C');
        fdesc[16] = (uint8_t)flen[c];
        fdesc[17] = (uint8_t)(is_num[c] ? 4 : 0);
        if (fwrite(fdesc, 1, 32, f) != 32) {
            fclose(f);
            return -1;
        }
    }
    term = 0x0D;
    if (fwrite(&term, 1, 1, f) != 1) {
        fclose(f);
        return -1;
    }

    for (r = 0; r <= rmax; r++) {
        int off = 1;
        memset(rec, ' ', rec_size);
        rec[0] = ' ';
        for (c = 0; c < nf; c++) {
            const cell_t *cell = &sh->cells[r][c];
            char tmp[32];
            tmp[0] = '\0';
            if (cell->kind == CELL_LABEL && cell->raw) {
                strncpy(tmp, cell->raw, sizeof(tmp) - 1);
                tmp[sizeof(tmp) - 1] = '\0';
                pad_left((char *)rec + off, flen[c], tmp);
            } else if (cell->kind != CELL_EMPTY && cell->err == SERR_OK) {
                if (cell->value == (double)(long)cell->value) {
                    snprintf(tmp, sizeof(tmp), "%ld", (long)cell->value);
                } else {
                    snprintf(tmp, sizeof(tmp), "%.8g", cell->value);
                }
                pad_right((char *)rec + off, flen[c], tmp);
            }
            off += flen[c];
        }
        if (fwrite(rec, 1, rec_size, f) != rec_size) {
            fclose(f);
            return -1;
        }
    }
    term = 0x1A;
    fwrite(&term, 1, 1, f);
    fclose(f);
    return 0;
}

static void trim_spaces(char *s) {
    char *p = s;
    char *end;
    while (*p == ' ') {
        p++;
    }
    if (p != s) {
        memmove(s, p, strlen(p) + 1);
    }
    end = s + strlen(s);
    while (end > s && end[-1] == ' ') {
        end--;
    }
    *end = '\0';
}

int sheet_load_dbf(sheet_t *sh, const char *path) {
    FILE *f;
    uint8_t hdr[32];
    uint8_t fdesc[32];
    uint16_t hdr_size;
    uint16_t rec_size;
    uint32_t nrec;
    int nf;
    int i;
    int flen[DBF_MAX_FIELDS];
    char ftype[DBF_MAX_FIELDS];
    uint32_t recno;

    f = fopen(path, "r");
    if (!f) {
        return -1;
    }
    if (fread(hdr, 1, 32, f) != 32) {
        fclose(f);
        return -1;
    }
    if (hdr[0] != 0x03 && hdr[0] != 0x83) {
        fclose(f);
        return -1;
    }
    nrec = rd32(hdr + 4);
    hdr_size = rd16(hdr + 8);
    rec_size = rd16(hdr + 10);
    if (rec_size < 1 || rec_size > 1 + DBF_MAX_FIELDS * 64) {
        fclose(f);
        return -1;
    }
    nf = ((int)hdr_size - 33) / 32;
    if (nf < 1 || nf > DBF_MAX_FIELDS) {
        fclose(f);
        return -1;
    }

    for (i = 0; i < nf; i++) {
        if (fread(fdesc, 1, 32, f) != 32) {
            fclose(f);
            return -1;
        }
        ftype[i] = (char)fdesc[11];
        flen[i] = fdesc[16];
        if (flen[i] < 1) {
            fclose(f);
            return -1;
        }
    }

    /* The per-record field walk does memcpy(tmp, rec + off, ...) advancing off
     * by flen[i]. If the fields sum past rec_size, that reads past the record
     * (and past the stack rec[] buffer) — reject the file instead. */
    {
        int total = 1; /* deletion flag byte */
        for (i = 0; i < nf; i++) {
            total += flen[i];
        }
        if (total > (int)rec_size) {
            fclose(f);
            return -1;
        }
    }

    if (fseek(f, (long)hdr_size, SEEK_SET) != 0) {
        fclose(f);
        return -1;
    }

    sheet_clear(sh);
    for (recno = 0; recno < nrec && recno < (uint32_t)SHEET_ROWS; recno++) {
        uint8_t rec[1 + 26 * 64];
        int off = 1;
        if (rec_size > sizeof(rec)) {
            break;
        }
        if (fread(rec, 1, rec_size, f) != rec_size) {
            break;
        }
        if (rec[0] == '*') {
            continue; /* deleted — leave the sheet row empty */
        }
        for (i = 0; i < nf && i < SHEET_COLS; i++) {
            char tmp[65];
            int n = flen[i];
            if (n > 64) {
                n = 64;
            }
            memcpy(tmp, rec + off, (size_t)n);
            tmp[n] = '\0';
            trim_spaces(tmp);
            if (tmp[0]) {
                sheet_set(sh, i, (int)recno, tmp);
            }
            off += flen[i];
            (void)ftype;
        }
    }

    fclose(f);
    sheet_recalc(sh);
    return 0;
}
