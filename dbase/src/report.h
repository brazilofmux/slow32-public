#ifndef REPORT_H
#define REPORT_H

#include <stdio.h>
#include "dbf.h"
#include "expr.h"

#define FRM_FILE_SIZE       1990
#define FRM_MAX_COLUMNS     24
#define FRM_MAX_EXPRS       55
#define FRM_EXPR_BUF_SIZE   1440

typedef struct {
    int width;              /* Column display width */
    int decimals;           /* Decimal places (numeric columns) */
    int totals;             /* 1 = include in totals */
    char content[256];      /* Content expression text */
    char header[256];       /* Header text (';' = line break) */
} frm_column_t;

typedef struct {
    /* Report parameters */
    char title[256];        /* Page title expression */
    char group_expr[256];   /* GROUP ON expression ("" = none) */
    char subgroup_expr[256];/* SUB-GROUP ON expression ("" = none) */
    char group_header[256]; /* Group header text */
    char subgroup_header[256];
    int page_width;         /* Default 80 */
    int lines_per_page;     /* Default 58 */
    int left_margin;        /* Default 8 */
    int right_margin;       /* Default 0 */
    int num_columns;        /* 0-24 */
    int double_space;       /* 1 = double-spaced */
    int summary_only;       /* 1 = no detail lines */
    int eject_after;        /* 1 = page eject after */
    int plain;              /* 1 = no page headers (runtime only, not stored) */
    int frm_plain;          /* the option byte's plain-page bit (Clipper) */
    int no_eject_before;    /* the option byte's "no eject before" bit */
    int group_eject;        /* page eject after each group (the PE byte) */

    /* Columns */
    frm_column_t columns[FRM_MAX_COLUMNS];
} frm_def_t;

/* Initialize with defaults */
void frm_init(frm_def_t *def);

/* Read/write binary .FRM file (return 0=ok, -1=error) */
int frm_read(const char *filename, frm_def_t *def);
int frm_write(const char *filename, const frm_def_t *def);

/* The layout REPORT FORM prints: dBASE III's (0, the interpreter's) or
 * CA-Clipper 5.2's (1, set by a compiled Clipper program's runtime) */
void report_set_clipper_layout(int on);
int report_clipper_layout(void);

/* Clipper's REPORT FORM: CA's FRMRUN layout (Summer '87 compatible, as
 * CA built it); to_printer turns page breaks into form feeds */
void report_generate_clipper(frm_def_t *def, dbf_t *db, expr_ctx_t *ectx,
                             const char *for_cond, const char *heading,
                             int plain, int summary, int noeject, int to_printer,
                             FILE *outfile);

/* Generate report output */
void report_generate(frm_def_t *def, dbf_t *db, expr_ctx_t *ectx,
                     const char *for_cond, const char *heading,
                     int plain, int summary, int noeject,
                     FILE *outfile);

#endif
