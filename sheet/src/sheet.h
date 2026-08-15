#ifndef SHEET_H
#define SHEET_H

#include <stdio.h>

#define SHEET_COLS 26
#define SHEET_ROWS 64

enum {
    CELL_EMPTY = 0,
    CELL_NUM,
    CELL_LABEL,
    CELL_FORMULA
};

enum {
    SERR_OK = 0,
    SERR_DIV0,
    SERR_CYCLE,
    SERR_REF,
    SERR_NAME,
    SERR_PARSE
};

enum {
    MARK_WHITE = 0,
    MARK_GRAY,
    MARK_BLACK
};

typedef struct {
    int kind;
    char *raw;
    double value;
    int err;
    int mark;
} cell_t;

typedef struct {
    cell_t cells[SHEET_ROWS][SHEET_COLS];
} sheet_t;

int parse_cell_name(const char *s, const char **end, int *col, int *row);
void format_cell_name(int col, int row, char *buf, int cap);
const char *sheet_err_str(int err);

void sheet_init(sheet_t *sh);
void sheet_clear(sheet_t *sh);
cell_t *sheet_cell(sheet_t *sh, int col, int row);
int sheet_set(sheet_t *sh, int col, int row, const char *text);
double sheet_eval(sheet_t *sh, int col, int row, int *err);
void sheet_recalc(sheet_t *sh);
void sheet_format_value(const cell_t *cell, char *buf, int cap);
void sheet_list(const sheet_t *sh, FILE *out);
int sheet_save(const sheet_t *sh, const char *path);
int sheet_load(sheet_t *sh, const char *path);
int sheet_save_sht(const sheet_t *sh, const char *path);
int sheet_load_sht(sheet_t *sh, const char *path);
int sheet_save_wk1(const sheet_t *sh, const char *path);
int sheet_load_wk1(sheet_t *sh, const char *path);
int sheet_save_dbf(const sheet_t *sh, const char *path);
int sheet_load_dbf(sheet_t *sh, const char *path);

/* Full-screen grid. Returns after the user quits. filename_cap includes NUL. */
void sheet_ui(sheet_t *sh, char *filename, int filename_cap);

int formula_eval(sheet_t *sh, const char *expr, double *out, int *err);

#endif
