#ifndef SCREEN_H
#define SCREEN_H

#include "expr.h"
#include "memvar.h"

#define MAX_GETS 64

typedef struct {
    int row, col;
    char varname[MEMVAR_NAMELEN];
    char picture[64];
    int width;
    val_type_t type;
    char initial[256];
    int has_range;
    value_t range_lo, range_hi;
    char valid_expr[256];
    char when_expr[256];
} get_entry_t;

typedef struct {
    get_entry_t gets[MAX_GETS];
    int ngets;
    int term_available;
    int fg_color, bg_color;
    int last_row, last_col;  /* cursor position tracking */
} screen_state_t;

/* Initialize screen subsystem */
void screen_init(void);

/* @SAY command */
void screen_say(int row, int col, const char *expr_str, const char *picture);

/* @GET command (range_lo/range_hi = NULL if no RANGE) */
void screen_get(int row, int col, const char *varname, const char *picture,
                const value_t *range_lo, const value_t *range_hi,
                const char *valid_expr, const char *when_expr);

/* @CLEAR TO command */
void screen_clear_region(int r1, int c1, int r2, int c2);

/* @TO command (box drawing) */
void screen_box(int r1, int c1, int r2, int c2, int dbl);

/* READ command */
void screen_read(void);

/* CLEAR command */
void screen_clear(void);

/* CLEAR GETS command */
void screen_clear_gets(void);

/* SET COLOR TO command */
void screen_set_color(const char *spec);

/* Parse and execute @ command */
void screen_at_cmd(const char *line);

/* Cursor position tracking (for ROW()/COL() functions) */
int screen_get_row(void);
int screen_get_col(void);

/* Printer position tracking (for PROW()/PCOL() functions) */
int screen_get_prow(void);
int screen_get_pcol(void);

/* EJECT command (form feed / reset printer position) */
void screen_eject(void);

/* Printer output helpers that track PROW()/PCOL() */
void screen_print_newline(void);
void screen_print_text(const char *s);

#endif
