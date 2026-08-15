/*
 * Lotus 1-2-3 WK1 / WKS subset.
 *
 * Records are little-endian: opcode:u16, length:u16, data[length].
 * We write WK1 (BOF version 0x0406) and read WKS (0x0404) or WK1.
 *
 * Written: BOF, INTEGER / NUMBER / LABEL, EOF.
 * Read:    those plus FORMULA (cached IEEE value only; RPN is skipped).
 * Formulas become numbers on export. Native .sht keeps the expressions.
 */

#include "sheet.h"

#include <stdint.h>
#include <stdio.h>
#include <string.h>

#define WK1_BOF     0x0000
#define WK1_EOF     0x0001
#define WK1_INTEGER 0x000D
#define WK1_NUMBER  0x000E
#define WK1_LABEL   0x000F
#define WK1_FORMULA 0x0010
#define WK1_VER_WKS 0x0404
#define WK1_VER_WK1 0x0406
#define WK1_MAX_REC 512

static int wr16(FILE *f, uint16_t v) {
    if (fputc(v & 0xff, f) == EOF) {
        return -1;
    }
    if (fputc((v >> 8) & 0xff, f) == EOF) {
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

static int rd16(FILE *f, uint16_t *out) {
    int lo = fgetc(f);
    int hi = fgetc(f);
    if (lo == EOF || hi == EOF) {
        return -1;
    }
    *out = (uint16_t)(lo | (hi << 8));
    return 0;
}

static int is_int16(double v) {
    long n;
    if (v != (double)(long)v) {
        return 0;
    }
    n = (long)v;
    return n >= -32768L && n <= 32767L;
}

int sheet_save_wk1(const sheet_t *sh, const char *path) {
    FILE *f;
    int r, c;
    uint8_t bof[2];

    f = fopen(path, "w");
    if (!f) {
        return -1;
    }

    /* Caller should have recalculated; formula cells export their value. */

    bof[0] = (uint8_t)(WK1_VER_WK1 & 0xff);
    bof[1] = (uint8_t)(WK1_VER_WK1 >> 8);
    if (wr_rec(f, WK1_BOF, bof, 2) < 0) {
        fclose(f);
        return -1;
    }

    for (r = 0; r < SHEET_ROWS; r++) {
        for (c = 0; c < SHEET_COLS; c++) {
            const cell_t *cell = &sh->cells[r][c];
            uint8_t rec[WK1_MAX_REC];
            if (cell->kind == CELL_EMPTY) {
                continue;
            }
            rec[0] = 0xFF; /* default format */
            rec[1] = (uint8_t)(c & 0xff);
            rec[2] = (uint8_t)((c >> 8) & 0xff);
            rec[3] = (uint8_t)(r & 0xff);
            rec[4] = (uint8_t)((r >> 8) & 0xff);

            if (cell->kind == CELL_LABEL) {
                const char *s = cell->raw ? cell->raw : "";
                size_t n = strlen(s);
                if (n > 240) {
                    n = 240;
                }
                rec[5] = '\'';
                memcpy(rec + 6, s, n);
                rec[6 + n] = 0;
                if (wr_rec(f, WK1_LABEL, rec, (uint16_t)(7 + n)) < 0) {
                    fclose(f);
                    return -1;
                }
            } else if (is_int16(cell->value) && cell->err == SERR_OK) {
                int16_t iv = (int16_t)cell->value;
                rec[5] = (uint8_t)(iv & 0xff);
                rec[6] = (uint8_t)((iv >> 8) & 0xff);
                if (wr_rec(f, WK1_INTEGER, rec, 7) < 0) {
                    fclose(f);
                    return -1;
                }
            } else if (cell->err == SERR_OK) {
                memcpy(rec + 5, &cell->value, 8);
                if (wr_rec(f, WK1_NUMBER, rec, 13) < 0) {
                    fclose(f);
                    return -1;
                }
            }
        }
    }

    if (wr_rec(f, WK1_EOF, NULL, 0) < 0) {
        fclose(f);
        return -1;
    }
    fclose(f);
    return 0;
}

static int put_number(sheet_t *sh, int col, int row, double v) {
    char buf[32];
    if (col < 0 || col >= SHEET_COLS || row < 0 || row >= SHEET_ROWS) {
        return 0;
    }
    if (v == (double)(long)v) {
        snprintf(buf, sizeof(buf), "%ld", (long)v);
    } else {
        snprintf(buf, sizeof(buf), "%.10g", v);
    }
    return sheet_set(sh, col, row, buf);
}

static int put_label(sheet_t *sh, int col, int row, const char *s) {
    if (col < 0 || col >= SHEET_COLS || row < 0 || row >= SHEET_ROWS) {
        return 0;
    }
    if (s[0] == '\'' || s[0] == '"' || s[0] == '^' || s[0] == '\\') {
        s++;
    }
    return sheet_set(sh, col, row, s);
}

int sheet_load_wk1(sheet_t *sh, const char *path) {
    FILE *f;
    uint16_t op;
    uint16_t len;
    uint8_t rec[WK1_MAX_REC];
    int saw_bof = 0;

    f = fopen(path, "r");
    if (!f) {
        return -1;
    }
    sheet_clear(sh);

    while (rd16(f, &op) == 0 && rd16(f, &len) == 0) {
        int col;
        int row;
        if (len > WK1_MAX_REC) {
            fclose(f);
            return -1;
        }
        if (len > 0 && fread(rec, 1, len, f) != len) {
            fclose(f);
            return -1;
        }
        if (op == WK1_BOF) {
            saw_bof = 1;
            continue;
        }
        if (op == WK1_EOF) {
            break;
        }
        if (len < 5) {
            continue;
        }
        col = rec[1] | (rec[2] << 8);
        row = rec[3] | (rec[4] << 8);

        if (op == WK1_INTEGER && len >= 7) {
            int16_t v = (int16_t)(rec[5] | (rec[6] << 8));
            put_number(sh, col, row, (double)v);
        } else if (op == WK1_NUMBER && len >= 13) {
            double v;
            memcpy(&v, rec + 5, 8);
            put_number(sh, col, row, v);
        } else if (op == WK1_FORMULA && len >= 13) {
            double v;
            memcpy(&v, rec + 5, 8);
            put_number(sh, col, row, v);
        } else if (op == WK1_LABEL && len >= 7) {
            rec[len - 1] = 0;
            put_label(sh, col, row, (char *)(rec + 5));
        }
    }

    fclose(f);
    if (!saw_bof) {
        return -1;
    }
    sheet_recalc(sh);
    return 0;
}
