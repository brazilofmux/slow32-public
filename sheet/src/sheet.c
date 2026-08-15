#include "sheet.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const char *sheet_err_str(int err) {
    switch (err) {
    case SERR_OK:    return "OK";
    case SERR_DIV0:  return "#DIV/0!";
    case SERR_CYCLE: return "#CYCLE!";
    case SERR_REF:   return "#REF!";
    case SERR_NAME:  return "#NAME?";
    case SERR_PARSE: return "#ERR";
    default:         return "#ERR";
    }
}

void sheet_init(sheet_t *sh) {
    memset(sh, 0, sizeof(*sh));
}

void sheet_clear(sheet_t *sh) {
    int r, c;
    for (r = 0; r < SHEET_ROWS; r++) {
        for (c = 0; c < SHEET_COLS; c++) {
            free(sh->cells[r][c].raw);
        }
    }
    memset(sh, 0, sizeof(*sh));
}

cell_t *sheet_cell(sheet_t *sh, int col, int row) {
    if (col < 0 || col >= SHEET_COLS || row < 0 || row >= SHEET_ROWS) {
        return NULL;
    }
    return &sh->cells[row][col];
}

static int looks_formula(const char *s) {
    if (*s == '=' || *s == '+' || *s == '@' || *s == '(') {
        return 1;
    }
    if (*s == '-') {
        const char *end;
        int col, row;
        if (parse_cell_name(s + 1, &end, &col, &row) && col >= 0) {
            return 1;
        }
    }
    {
        const char *end;
        int col, row;
        if (parse_cell_name(s, &end, &col, &row) && col >= 0) {
            while (*end == ' ' || *end == '\t') {
                end++;
            }
            if (*end == '\0' || *end == '+' || *end == '-' ||
                *end == '*' || *end == '/') {
                return 1;
            }
        }
    }
    return 0;
}

static int whole_number(const char *s, double *out) {
    char *end = NULL;
    double v;
    while (*s == ' ' || *s == '\t') {
        s++;
    }
    if (*s == '\0') {
        return 0;
    }
    v = strtod(s, &end);
    if (end == s) {
        return 0;
    }
    while (*end == ' ' || *end == '\t') {
        end++;
    }
    if (*end != '\0') {
        return 0;
    }
    *out = v;
    return 1;
}

int sheet_set(sheet_t *sh, int col, int row, const char *text) {
    cell_t *cell = sheet_cell(sh, col, row);
    char *copy;
    double num;

    if (!cell) {
        return SERR_REF;
    }
    free(cell->raw);
    memset(cell, 0, sizeof(*cell));

    if (!text) {
        return SERR_OK;
    }
    while (*text == ' ' || *text == '\t') {
        text++;
    }
    if (*text == '\0') {
        return SERR_OK;
    }

    copy = (char *)malloc(strlen(text) + 1);
    if (!copy) {
        return SERR_PARSE;
    }
    memcpy(copy, text, strlen(text) + 1);
    cell->raw = copy;

    if (looks_formula(copy)) {
        const char *expr = copy;
        if (*expr == '=') {
            expr++;
        }
        cell->kind = CELL_FORMULA;
        cell->err = formula_eval(sh, expr, &cell->value, &cell->err);
        return cell->err;
    }
    if (whole_number(copy, &num)) {
        cell->kind = CELL_NUM;
        cell->value = num;
        return SERR_OK;
    }
    cell->kind = CELL_LABEL;
    cell->value = 0.0;
    return SERR_OK;
}

double sheet_eval(sheet_t *sh, int col, int row, int *err) {
    cell_t *cell = sheet_cell(sh, col, row);
    int ferr = SERR_OK;
    double v = 0.0;

    if (!cell) {
        if (err) {
            *err = SERR_REF;
        }
        return 0.0;
    }
    if (cell->kind == CELL_EMPTY || cell->kind == CELL_LABEL) {
        if (err) {
            *err = SERR_OK;
        }
        return 0.0;
    }
    if (cell->kind == CELL_NUM) {
        if (err) {
            *err = SERR_OK;
        }
        return cell->value;
    }

    /* formula */
    if (cell->mark == MARK_GRAY) {
        cell->err = SERR_CYCLE;
        if (err) {
            *err = SERR_CYCLE;
        }
        return 0.0;
    }
    if (cell->mark == MARK_BLACK) {
        if (err) {
            *err = cell->err;
        }
        return cell->value;
    }

    cell->mark = MARK_GRAY;
    {
        const char *expr = cell->raw ? cell->raw : "";
        if (*expr == '=') {
            expr++;
        }
        ferr = formula_eval(sh, expr, &v, &ferr);
    }
    cell->value = v;
    cell->err = ferr;
    cell->mark = MARK_BLACK;
    if (err) {
        *err = ferr;
    }
    return v;
}

void sheet_recalc(sheet_t *sh) {
    int r, c;
    for (r = 0; r < SHEET_ROWS; r++) {
        for (c = 0; c < SHEET_COLS; c++) {
            sh->cells[r][c].mark = MARK_WHITE;
        }
    }
    for (r = 0; r < SHEET_ROWS; r++) {
        for (c = 0; c < SHEET_COLS; c++) {
            if (sh->cells[r][c].kind == CELL_FORMULA) {
                int err = SERR_OK;
                sheet_eval(sh, c, r, &err);
            }
        }
    }
}

void sheet_format_value(const cell_t *cell, char *buf, int cap) {
    if (cell->kind == CELL_EMPTY) {
        buf[0] = '\0';
        return;
    }
    if (cell->kind == CELL_FORMULA && cell->err != SERR_OK) {
        snprintf(buf, (size_t)cap, "%s", sheet_err_str(cell->err));
        return;
    }
    if (cell->kind == CELL_LABEL) {
        snprintf(buf, (size_t)cap, "%s", cell->raw ? cell->raw : "");
        return;
    }
    if (cell->value == (double)(long)cell->value) {
        snprintf(buf, (size_t)cap, "%ld", (long)cell->value);
    } else {
        snprintf(buf, (size_t)cap, "%g", cell->value);
    }
}

void sheet_list(const sheet_t *sh, FILE *out) {
    int r, c;
    int rmax = -1, cmax = -1;
    int used = 0;

    for (r = 0; r < SHEET_ROWS; r++) {
        for (c = 0; c < SHEET_COLS; c++) {
            if (sh->cells[r][c].kind != CELL_EMPTY) {
                if (r > rmax) rmax = r;
                if (c > cmax) cmax = c;
                used++;
            }
        }
    }
    if (used == 0) {
        fprintf(out, "(empty)\n");
        return;
    }

    fprintf(out, "     ");
    for (c = 0; c <= cmax; c++) {
        fprintf(out, " %10c", 'A' + c);
    }
    fprintf(out, "\n");
    for (r = 0; r <= rmax; r++) {
        fprintf(out, "%4d ", r + 1);
        for (c = 0; c <= cmax; c++) {
            char buf[16];
            sheet_format_value(&sh->cells[r][c], buf, (int)sizeof(buf));
            fprintf(out, " %10s", buf);
        }
        fprintf(out, "\n");
    }
}

static int ends_with_ci(const char *s, const char *suf) {
    size_t n;
    size_t m;
    const char *a;
    const char *b;
    if (!s || !suf) {
        return 0;
    }
    n = strlen(s);
    m = strlen(suf);
    if (n < m) {
        return 0;
    }
    a = s + n - m;
    b = suf;
    while (*b) {
        if (toupper((unsigned char)*a) != toupper((unsigned char)*b)) {
            return 0;
        }
        a++;
        b++;
    }
    return 1;
}

static int path_is_wk1(const char *path) {
    return ends_with_ci(path, ".wk1") || ends_with_ci(path, ".wks");
}

int sheet_save(const sheet_t *sh, const char *path) {
    if (path_is_wk1(path)) {
        return sheet_save_wk1(sh, path);
    }
    return sheet_save_sht(sh, path);
}

int sheet_load(sheet_t *sh, const char *path) {
    if (path_is_wk1(path)) {
        return sheet_load_wk1(sh, path);
    }
    return sheet_load_sht(sh, path);
}

int sheet_save_sht(const sheet_t *sh, const char *path) {
    FILE *f;
    int r, c;

    f = fopen(path, "w");
    if (!f) {
        return -1;
    }
    fprintf(f, "# s32-sheet v1\n");
    for (r = 0; r < SHEET_ROWS; r++) {
        for (c = 0; c < SHEET_COLS; c++) {
            const cell_t *cell = &sh->cells[r][c];
            char name[8];
            if (cell->kind == CELL_EMPTY || !cell->raw) {
                continue;
            }
            format_cell_name(c, r, name, (int)sizeof(name));
            if (cell->kind == CELL_FORMULA) {
                if (cell->raw[0] == '=') {
                    fprintf(f, "%s=%s\n", name, cell->raw);
                } else {
                    fprintf(f, "%s==%s\n", name, cell->raw);
                }
            } else {
                fprintf(f, "%s=%s\n", name, cell->raw);
            }
        }
    }
    fclose(f);
    return 0;
}

int sheet_load_sht(sheet_t *sh, const char *path) {
    FILE *f;
    char line[256];

    f = fopen(path, "r");
    if (!f) {
        return -1;
    }
    sheet_clear(sh);
    while (fgets(line, (int)sizeof(line), f)) {
        char *nl = strchr(line, '\n');
        char *eq;
        const char *end;
        int col, row;
        if (nl) {
            *nl = '\0';
        }
        if (line[0] == '#' || line[0] == '\0') {
            continue;
        }
        eq = strchr(line, '=');
        if (!eq) {
            continue;
        }
        *eq = '\0';
        if (!parse_cell_name(line, &end, &col, &row) || col < 0) {
            continue;
        }
        sheet_set(sh, col, row, eq + 1);
    }
    fclose(f);
    sheet_recalc(sh);
    return 0;
}
