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
} get_entry_t;

typedef struct {
    get_entry_t gets[MAX_GETS];
    int ngets;
    int term_available;
    int fg_color, bg_color;
} screen_state_t;

/* Initialize screen subsystem */
void screen_init(void);

/* @SAY command */
void screen_say(int row, int col, const char *expr_str, const char *picture);

/* @GET command */
void screen_get(int row, int col, const char *varname, const char *picture);

/* @CLEAR TO command */
void screen_clear_region(int r1, int c1, int r2, int c2);

/* @TO command (box drawing) */
void screen_box(int r1, int c1, int r2, int c2, int dbl);

/* READ command */
void screen_read(void);

/* CLEAR command */
void screen_clear(void);

/* SET COLOR TO command */
void screen_set_color(const char *spec);

/* Parse and execute @ command */
void screen_at_cmd(const char *line);

#endif
