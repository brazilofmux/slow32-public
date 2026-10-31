#ifndef SCREEN_H
#define SCREEN_H

#include "expr.h"
#include "memvar.h"

struct ast_node;

#define MAX_GETS 64
#define KEY_NONE (-9999)   /* read_dbase_key(): no key (F2 is -1) */
#define MAX_PROMPTS 20

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
    struct ast_node *valid_ast;
    struct ast_node *when_ast;
    int is_field;    /* 1 if variable is a database field */
    int field_index; /* field index in db (valid when is_field=1) */
    value_t getobj;  /* the Get object beside it (getsys.c), or NIL */
} get_entry_t;

typedef struct {
    int row, col;
    char text[80];
    char message[160];
} prompt_entry_t;

typedef struct {
    get_entry_t gets[MAX_GETS];
    int ngets;
    prompt_entry_t prompts[MAX_PROMPTS];
    int nprompts;
    int term_available;
    int fg_color, bg_color;
    int last_row, last_col;  /* cursor position tracking */
} screen_state_t;

/* Initialize screen subsystem */
void screen_init(void);

/* Shutdown screen subsystem (release terminal service) */
void screen_shutdown(void);

/* @SAY command */
void screen_say(int row, int col, const char *expr_str, const char *picture);

/* @GET command (range_lo/range_hi = NULL if no RANGE) */
void screen_get_object(int row, int col, const char *name, const char *picture,
                       const value_t *initial, const value_t *obj);   /* a Get object's GET (READMODAL) */
void screen_get(int row, int col, const char *varname, const char *picture,
                const value_t *range_lo, const value_t *range_hi,
                const char *valid_expr, const char *when_expr);
void screen_get_expr(int row, int col, const char *target, const char *picture,
                     const value_t *range_lo, const value_t *range_hi,
                     const char *valid_expr, const char *when_expr);

/* @CLEAR TO command */
void screen_clear_region(int r1, int c1, int r2, int c2);

/* @TO command (box drawing) */
void screen_box(int r1, int c1, int r2, int c2, int dbl);
/* @ ... BOX / DISPBOX with a frame string: top-left, top, top-right, right,
 * bottom-right, bottom, bottom-left, left, and a ninth that fills */
void screen_box_frame(int r1, int c1, int r2, int c2, const char *frame);

/* READ command */
void screen_read(void);

/* CLEAR command */
void screen_clear(void);

/* CLEAR GETS command */
void screen_clear_gets(void);

/* @PROMPT command */
void screen_prompt(int row, int col, const char *text, const char *message);

/* MENU TO command — returns 1-based selection (0 if Esc) */
int screen_menu_to(void);

/* CLEAR prompts */
void screen_clear_prompts(void);

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

/* The printer: SET PRINTER TO's file, or the console standing in for it.
 * Everything written to it moves PROW()/PCOL(); print_stream with write
 * 0 only moves them (the text is on the console already). */
void screen_print_stream(const char *s, int write);
void screen_print_text(const char *s);
void screen_print_at(int row, int col, const char *s);   /* @ ... SAY, DEVPOS / DEVOUT */

/* Keyboard functions */
int  screen_inkey(double timeout);  /* INKEY(): <0=wait forever, 0=poll, >0=timeout secs */
int  screen_lastkey(void);          /* LASTKEY(): last key from INKEY/READ/WAIT */
int  screen_readkey(void);          /* READKEY(): how user exited last READ */
void screen_set_lastkey(int key);   /* Set lastkey (for WAIT, ACCEPT, etc.) */

/* Check for Esc keypress (non-blocking). Returns 1 if Esc pressed, 0 otherwise. */
int screen_check_escape(void);

/* Low-level key reader: reads one keypress from terminal, decoding escape sequences.
   Returns dBase key code. Only available when HAS_TERM is 1. */
int read_dbase_key(void);

/* SAVE SCREEN / RESTORE SCREEN */
void screen_save(void);
void screen_restore(void);

/* Check if terminal service is available */
int screen_term_available(void);

/* The terminal through screen.c: each write also lands in the shadow
 * screen, which SAVESCREEN / RESTSCREEN read (menu.c and browse.c paint
 * through these, never term_* directly) */
void screen_tgoto(int row, int col);
void screen_tputs(const char *s);
void screen_tputc(int ch);
void screen_tattr(int attr);
void screen_tcolor(int fg, int bg);
void screen_tclear(int mode);
value_t screen_save_region(int t, int l, int b, int r);                 /* a handle string */
void screen_restore_region(int t, int l, int b, int r, const value_t *saved);

/* The display mode: MAXROW(), MAXCOL(); SETMODE() (0 when refused) */
int screen_maxrow(void);
int screen_maxcol(void);
int screen_setmode(int rows, int cols);
void screen_message(int row, const char *msg);   /* a SET MESSAGE row, blanked, msg centred */

/* Key handler support (SET KEY / ON KEY) */
void screen_set_key_handler(int keycode, const char *procname);
void screen_clear_key_handlers(void);
int  screen_check_key_handler(int keycode);  /* returns 1 if handler fired */
/* SETKEY(): a block as the handler (NULL removes it); the one before */
value_t screen_set_key_block(int keycode, const value_t *block);

/* For the Get system (getsys.c): text at a position in either mode, the
 * cursor moved, text at the cursor, the picture mask, the colour spec as
 * SETCOLOR() gave it, and the READ in progress */
void screen_out_at(int row, int col, const char *text);
void screen_setpos(int row, int col);
void screen_out(const char *text);
void screen_apply_picture(char *out, const char *value, const char *picture, int outsize);
const char *screen_color_spec(void);
void screen_color_pair(int n);          /* apply pair n of SET COLOR: 1 standard, 2 enhanced, 5 unselected */
void screen_keyboard(const char *s);    /* KEYBOARD: clear the typeahead, stuff the string */
void screen_clear_typeahead(void);
int screen_key_pending(void);
void scoreboard_message(const char *msg);   /* GETSYS's row-0 message, until a key waits */           /* a key waiting in any buffer or at the terminal */
int screen_getkey(void);                /* an editor's next key: the buffers, then the terminal */
const char *screen_read_var(void);   /* the GET being read; "" outside a READ */
int screen_read_updated(void);       /* UPDATED(): a GET changed in the last READ */
value_t screen_active_get(void);     /* GETACTIVE(): its Get object, or NIL */
/* READMODAL's own loop (getsys.c) keeps the READ state these report:
 * the Get being read (NIL outside), its variable's name, UPDATED() */
void screen_set_modal_get(const value_t *get, const char *varname);
void screen_set_read_updated(int updated);
/* XBASE_STREAM_SCREEN=none: with no terminal, positioned output (@ SAY,
 * a GET's field, SETPOS) is dropped, as Harbour's stream renderer drops
 * it; ? output still prints.  For comparing against Harbour. */
int screen_stream_quiet(void);
int screen_nextkey(void);            /* NEXTKEY(): the pending key, left pending */
void screen_set_printer_pos(int row, int col);   /* SETPRC() */

#endif
