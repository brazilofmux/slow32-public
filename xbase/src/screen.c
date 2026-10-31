#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <stdlib.h>
#include <ctype.h>
#include "screen.h"
#include "keys.h"
#include "command.h"
#include "program.h"
#include "util.h"
#include "memvar.h"
#include "expr.h"
#include "ast.h"
#include "lex.h"
#include "report.h"
#include "date.h"
#include "func.h"
#include "getsys.h"

#ifdef __slow32__
#include <term.h>
#define HAS_TERM 1
#else
#define HAS_TERM 0
#endif

static screen_state_t scr;
static int print_row, print_col;   /* printer position tracking */
static int last_key;               /* last key from INKEY/READ/WAIT */
static int read_exit_key;          /* how user exited last READ */
static char read_var[MEMVAR_NAMELEN];   /* the GET being read */
static int read_updated;           /* a GET changed in the last READ */
static int active_get = -1;        /* its index while a READ runs */
static char color_spec[64] = "W/N,N/W,N,N,N/W";

/* Key handler state (SET KEY / ON KEY) */
#define MAX_KEY_HANDLERS 32
static struct {
    int keycode;
    char procedure[64];
    value_t block;      /* SETKEY()'s handler, when the entry is one */
} key_handlers[MAX_KEY_HANDLERS];
static int key_handler_count;
static int in_key_handler;  /* re-entrancy guard */

/* The display mode: 25 by 80 until SETMODE() asks for another */
static int scr_rows = 25, scr_cols = 80;


/* dBase III key codes for special keys */

/* Extended key codes (above ASCII range) */
/* Function keys as dBASE III and Clipper number them (inkey.ch): F1 is
 * 28, F2 to F10 are -1 to -9 */

void screen_init(void) {
    memset(&scr, 0, sizeof(scr));
    scr.fg_color = 7; /* white */
    scr.bg_color = 0; /* black */
    scr.last_col = 1;
#if HAS_TERM
    /* Try to negotiate terminal service. If denied (e.g. --deny term),
     * fall back to console mode (newlines + spaces). */
    scr.term_available = (term_init() == 0) ? 1 : 0;
#else
    scr.term_available = 0;
#endif
}

void screen_shutdown(void) {
#if HAS_TERM
    if (scr.term_available) {
        term_cleanup();
        scr.term_available = 0;
    }
#endif
}

/* SAVE SCREEN / RESTORE SCREEN without a variable: one buffer, the
 * whole screen's cells (its handle kept as text, which no collection
 * sweeps) */
static char whole_screen[32];

void screen_save(void) {
    value_t h = screen_save_region(0, 0, scr_rows - 1, scr_cols - 1);
    str_copy(whole_screen, h.type == VAL_CHAR ? val_text(&h) : "", sizeof(whole_screen));
}

void screen_restore(void) {
    value_t h;
    if (!whole_screen[0]) return;
    h = val_str(whole_screen);
    screen_restore_region(0, 0, scr_rows - 1, scr_cols - 1, &h);
}

int screen_term_available(void) { return scr.term_available; }

#if !HAS_TERM
/* No terminal service (a host build): the shadow is still kept; output
 * is the stream */
static void term_gotoxy(int row, int col) { (void)row; (void)col; }
static void term_puts(const char *s) { fputs(s, stdout); }
static void term_putc(int ch) { putchar(ch); }
static void term_set_attr(int attr) { (void)attr; }
static void term_set_color(int fg, int bg) { (void)fg; (void)bg; }
static void term_clear(int mode) { (void)mode; }
static void term_begin_update(void) { }
static void term_end_update(void) { }
#endif

/* ---- The shadow screen ----
 * The terminal's cells live in the host (the term service keeps them);
 * a Clipper program's SAVESCREEN() / RESTSCREEN() need them here.  Every
 * write xbase makes to the terminal goes through these wrappers and
 * lands in the shadow too: a cell is its character and an attribute
 * byte (bit 7 set, bold, background, foreground).  A saved region is
 * kept here and handed to the program as a short handle string (a
 * value_t string holds 255 characters; a screen is 4,000 cells). */
#define SHADOW_MAX_ROWS 128
#define SHADOW_MAX_COLS 256
typedef struct { unsigned char ch, attr; } shadow_cell_t;
static shadow_cell_t shadow[SHADOW_MAX_ROWS][SHADOW_MAX_COLS];
static int shadow_row, shadow_col;
static int shadow_fg = 7, shadow_bg = 0, shadow_bold, shadow_inverse;
static int shadow_ready;

int screen_maxrow(void) { return scr_rows - 1; }
int screen_maxcol(void) { return scr_cols - 1; }

/* SETMODE(): the display hardware Clipper drove is the terminal here, and
 * the shadow's size is the hardware's limit */
int screen_setmode(int rows, int cols) {
    if (rows < 1 || rows > SHADOW_MAX_ROWS || cols < 1 || cols > SHADOW_MAX_COLS) return 0;
    scr_rows = rows;
    scr_cols = cols;
    if (shadow_row >= scr_rows) shadow_row = scr_rows - 1;
    if (shadow_col > scr_cols) shadow_col = scr_cols;
    return 1;
}

static unsigned char shadow_attr(void) {
    int fg = shadow_inverse ? shadow_bg : shadow_fg;
    int bg = shadow_inverse ? shadow_fg : shadow_bg;
    return (unsigned char)(0x80 | (shadow_bold ? 0x40 : 0) | ((bg & 7) << 3) | (fg & 7));
}

static void shadow_init(void) {
    int r, c;
    if (shadow_ready) return;
    shadow_ready = 1;
    for (r = 0; r < SHADOW_MAX_ROWS; r++)
        for (c = 0; c < SHADOW_MAX_COLS; c++) { shadow[r][c].ch = ' '; shadow[r][c].attr = 0x87; }
}

static void shadow_put(int ch) {
    shadow_init();
    if (shadow_row >= 0 && shadow_row < scr_rows && shadow_col >= 0 && shadow_col < scr_cols) {
        shadow[shadow_row][shadow_col].ch = (unsigned char)ch;
        shadow[shadow_row][shadow_col].attr = shadow_attr();
    }
    if (shadow_row == scr_rows - 1 && shadow_col == scr_cols - 1) {
        shadow_col = scr_cols;   /* the screen's last cell: nothing follows it (see shadow_full) */
        return;
    }
    if (++shadow_col >= scr_cols) {
        shadow_col = 0;
        if (shadow_row < scr_rows - 1) shadow_row++;
    }
}

void screen_tgoto(int row, int col) {
    shadow_init();
    shadow_row = row;
    shadow_col = col;
    term_gotoxy(row + 1, col + 1);   /* the terminal's rows and columns count from 1; ours from 0 */
}

/* Past the screen's last cell: Clipper writes the screen as video memory
 * and what runs off the bottom-right corner is lost; a terminal would wrap
 * and scroll the whole screen up (PE's status line ends one past column 79) */
static int shadow_full(void) {
    return shadow_row == scr_rows - 1 && shadow_col >= scr_cols;
}

void screen_tputs(const char *s) {
    const char *p;
    for (p = s; *p; p++) {
        if (*p == '\n') { shadow_col = 0; if (shadow_row < scr_rows - 1) shadow_row++; }
        else if (*p == '\r') shadow_col = 0;
        else if (shadow_full()) {
            /* the rest of the string falls off the screen */
            size_t keep = (size_t)(p - s);
            char *head = keep ? malloc(keep + 1) : NULL;
            if (head) { memcpy(head, s, keep); head[keep] = '\0'; term_puts(head); free(head); }
            return;
        }
        else shadow_put((unsigned char)*p);
    }
    term_puts(s);
}

void screen_tputc(int ch) {
    if (ch == '\n') { shadow_col = 0; if (shadow_row < scr_rows - 1) shadow_row++; }
    else if (ch == '\r') shadow_col = 0;
    else if (shadow_full()) return;
    else shadow_put(ch);
    term_putc(ch);
}

void screen_tattr(int attr) {
    if (attr == 0) { shadow_bold = 0; shadow_inverse = 0; shadow_fg = 7; shadow_bg = 0; }
    else if (attr == 1) shadow_bold = 1;
    else if (attr == 7) shadow_inverse = 1;
    term_set_attr(attr);
}

void screen_tcolor(int fg, int bg) {
    shadow_fg = fg;
    shadow_bg = bg;
    term_set_color(fg, bg);
}

void screen_tclear(int mode) {
    unsigned char a;
    int r, c;
    shadow_init();
    a = shadow_attr();
    if (mode == 0) {
        for (r = 0; r < scr_rows; r++)
            for (c = 0; c < scr_cols; c++) { shadow[r][c].ch = ' '; shadow[r][c].attr = a; }
        shadow_row = shadow_col = 0;
    } else if (shadow_row >= 0 && shadow_row < scr_rows) {
        for (c = shadow_col > 0 ? shadow_col : 0; c < scr_cols; c++) { shadow[shadow_row][c].ch = ' '; shadow[shadow_row][c].attr = a; }
        if (mode == 2)
            for (r = shadow_row + 1; r < scr_rows; r++)
                for (c = 0; c < scr_cols; c++) { shadow[r][c].ch = ' '; shadow[r][c].attr = a; }
    }
    term_clear(mode);
}

/* Saved regions: a ring of 64, each handed out as "\1SCR<slot>.<gen>" */
#define SAVED_MAX 64
typedef struct { unsigned gen; int rows, cols; shadow_cell_t *cells; } saved_region_t;
static saved_region_t saved_regions[SAVED_MAX];
static unsigned saved_gen;
static int saved_next;

static void region_clamp(int *t, int *l, int *b, int *r) {
    if (*t < 0) *t = 0;
    if (*l < 0) *l = 0;
    if (*b >= scr_rows) *b = scr_rows - 1;
    if (*r >= scr_cols) *r = scr_cols - 1;
}

value_t screen_save_region(int t, int l, int b, int r) {
    saved_region_t *s;
    int rows, cols, i, j;
    char handle[32];
    shadow_init();
    region_clamp(&t, &l, &b, &r);
    rows = b - t + 1;
    cols = r - l + 1;
    if (rows <= 0 || cols <= 0) return val_str("");
    s = &saved_regions[saved_next];
    free(s->cells);
    s->cells = malloc((size_t)rows * (size_t)cols * sizeof(shadow_cell_t));
    if (!s->cells) return val_str("");
    s->rows = rows;
    s->cols = cols;
    s->gen = ++saved_gen;
    for (i = 0; i < rows; i++)
        for (j = 0; j < cols; j++) s->cells[i * cols + j] = shadow[t + i][l + j];
    snprintf(handle, sizeof(handle), "\001SCR%d.%u", saved_next, s->gen);
    saved_next = (saved_next + 1) % SAVED_MAX;
    return val_str(handle);
}

void screen_restore_region(int t, int l, int b, int r, const value_t *saved) {
    int slot, rows, cols, i, j;
    unsigned gen;
    saved_region_t *s;
    int keep_row = shadow_row, keep_col = shadow_col;
    int keep_fg = shadow_fg, keep_bg = shadow_bg, keep_bold = shadow_bold, keep_inverse = shadow_inverse;
    unsigned char cur = 0;
    int paint = scr.term_available;   /* without a terminal the shadow alone (a stream shows none of it) */
    if (!saved || saved->type != VAL_CHAR || sscanf(saved->str, "\001SCR%d.%u", &slot, &gen) != 2) return;
    if (slot < 0 || slot >= SAVED_MAX) return;
    s = &saved_regions[slot];
    if (!s->cells || s->gen != gen) return;
    shadow_init();
    region_clamp(&t, &l, &b, &r);
    rows = b - t + 1;
    cols = r - l + 1;
    if (rows > s->rows) rows = s->rows;
    if (cols > s->cols) cols = s->cols;
    for (i = 0; i < rows; i++) {
        shadow_row = t + i;
        shadow_col = l;
        if (paint) term_gotoxy(t + i + 1, l + 1);
        for (j = 0; j < cols; j++) {
            shadow_cell_t cell = s->cells[i * s->cols + j];
            if (!paint) {
                shadow[t + i][l + j] = cell;
                continue;
            }
            if (cell.attr != cur) {
                cur = cell.attr;
                term_set_attr(0);
                if (cur & 0x40) term_set_attr(1);
                term_set_color(cur & 7, (cur >> 3) & 7);
            }
            shadow[t + i][l + j] = cell;
            term_putc(cell.ch);
        }
    }
    /* the program's own colour and cursor back */
    shadow_fg = keep_fg; shadow_bg = keep_bg; shadow_bold = keep_bold; shadow_inverse = keep_inverse;
    shadow_row = keep_row;
    shadow_col = keep_col;
    if (!paint) return;
    term_set_attr(0);
    if (shadow_bold) term_set_attr(1);
    if (shadow_inverse) term_set_attr(7);
    term_set_color(shadow_fg, shadow_bg);
    term_gotoxy(keep_row + 1, keep_col + 1);
}

/* ---- Apply PICTURE mask to a string ---- */
void screen_apply_picture(char *out, const char *value, const char *picture, int outsize) {
    int i;
    int vi = 0;
    int vlen = strlen(value);
    int plen = strlen(picture);

    if (plen == 0 || picture[0] == '\0') {
        str_copy(out, value, outsize);
        return;
    }

    for (i = 0; i < plen && i < outsize - 1; i++) {
        char pc = picture[i];
        char vc = (vi < vlen) ? value[vi] : ' ';

        switch (pc) {
        case '!': /* uppercase */
            if (vc >= 'a' && vc <= 'z') vc -= 32;
            out[i] = vc;
            vi++;
            break;
        case '9': /* digits only */
            out[i] = (vc >= '0' && vc <= '9') ? vc : ' ';
            vi++;
            break;
        case 'A': /* alpha only */
            out[i] = ((vc >= 'A' && vc <= 'Z') || (vc >= 'a' && vc <= 'z')) ? vc : ' ';
            vi++;
            break;
        case 'X': /* any char */
            out[i] = vc;
            vi++;
            break;
        case '#': /* digits/spaces/sign */
            out[i] = (vc >= '0' && vc <= '9' || vc == ' ' || vc == '+' || vc == '-') ? vc : ' ';
            vi++;
            break;
        default:
            /* literal character in picture */
            out[i] = pc;
            break;
        }
    }
    out[i] = '\0';
}

/* ---- Position cursor (fallback: newlines and spaces) ---- */
static void goto_pos(int row, int col) {
#if HAS_TERM
    if (scr.term_available) {
        screen_tgoto(row, col);
        return;
    }
#endif
    if (screen_stream_quiet()) return;
    /* Fallback: track last SAY/GET row to merge same-row output.
     * We can't track absolute cursor position because other printf
     * calls (dot prompt, ? output, etc.) move the cursor without
     * updating our tracking. So: same row = stay on line, different
     * row = emit newlines proportional to the row gap. */
    if (row == scr.last_row && scr.last_col > 0) {
        /* Same row as last SAY/GET — pad with spaces from current pos */
        if (col < scr.last_col) {
            /* Need to go back — carriage return and re-pad */
            putchar('\r');
            scr.last_col = 1;
        }
    } else {
        /* Different row — emit newlines for row gap */
        int gap = row - scr.last_row;
        int r;
        if (gap < 1) gap = 1;
        for (r = 0; r < gap; r++) putchar('\n');
        scr.last_col = 1;
    }
    /* Pad to target column */
    {
        int i;
        for (i = scr.last_col; i < col; i++) putchar(' ');
    }
}

static void screen_update_pos(int row, int col, int len) {
    /* Track cursor position after outputting len chars starting at (row, col).
     * col is 1-based. After output, cursor is at col+len (1-based). */
    int end_col;
    if (col < 1) col = 1;
    if (len < 0) len = 0;
    end_col = col + len;
    /* Handle wrap past scr_cols */
    if (end_col > scr_cols + 1) {
        row += (end_col - 1) / scr_cols;
        end_col = ((end_col - 1) % scr_cols) + 1;
    }
    scr.last_row = row;
    scr.last_col = end_col;  /* 1-based: cursor is AT this column */
}

/* ---- Text at a position, for the Get system and TBrowse ---- */
void screen_out_at(int row, int col, const char *text) {
    goto_pos(row, col);
#if HAS_TERM
    if (scr.term_available)
        screen_tputs(text);
    else
#endif
    if (!screen_stream_quiet())
        printf("%s", text);
    screen_update_pos(row, col, (int)strlen(text));
}

void screen_setpos(int row, int col) {
    goto_pos(row, col);
    screen_update_pos(row, col, 0);
}

void screen_out(const char *text) {
    screen_out_at(scr.last_row, scr.last_col, text);
}

const char *screen_color_spec(void) {
    return color_spec;
}

const char *screen_read_var(void) {
    return read_var;
}

int screen_read_updated(void) {
    return read_updated;
}

static value_t modal_get;          /* READMODAL's loop: the Get it is reading (NIL: none) */

value_t screen_active_get(void) {
    if (modal_get.type == VAL_OBJECT) return modal_get;
    if (active_get >= 0 && active_get < scr.ngets) return scr.gets[active_get].getobj;
    return val_nil();
}

void screen_set_modal_get(const value_t *get, const char *varname) {
    modal_get = get ? *get : val_nil();
    str_copy(read_var, varname ? varname : "", sizeof(read_var));
}

void screen_set_read_updated(int updated) {
    read_updated = updated;
}

int screen_stream_quiet(void) {
    static int quiet = -1;
    if (quiet < 0) {
        const char *e = getenv("XBASE_STREAM_SCREEN");
        quiet = e && strcmp(e, "none") == 0;
    }
#if HAS_TERM
    if (scr.term_available) return 0;
#endif
    return quiet;
}

/* ---- @SAY ---- */
void screen_say(int row, int col, const char *expr_str, const char *picture) {
    expr_ctx_t *ctx = cmd_get_expr_ctx();
    value_t val;
    char buf[256];
    char formatted[256];

    if (ast_eval_dynamic(expr_str, ctx, &val) != 0) {
        if (ctx->error) prog_error(expr_error_code(ctx->error), ctx->error);
        return;
    }

    if (val.type == VAL_NUM && !(picture && picture[0])) val_to_display(&val, buf, sizeof(buf));
    else val_to_string(&val, buf, sizeof(buf));

    if (picture && picture[0]) {
        screen_apply_picture(formatted, buf, picture, sizeof(formatted));
    } else {
        str_copy(formatted, buf, sizeof(formatted));
    }

    if (cmd_get_device() == 1) {
        screen_print_at(row, col, formatted);
    } else {
        goto_pos(row, col);
#if HAS_TERM
        if (scr.term_available)
            screen_tputs(formatted);
        else
#endif
        if (!screen_stream_quiet())
            printf("%s", formatted);
    }
    screen_update_pos(row, col, (int)strlen(formatted));
}

/* ---- @GET ---- */
static void get_show_initial(const get_entry_t *g);

void screen_get(int row, int col, const char *varname, const char *picture,
                const value_t *range_lo, const value_t *range_hi,
                const char *valid_expr, const char *when_expr) {
    memvar_store_t *store = cmd_get_memvar_store();
    value_t val;

    if (scr.ngets >= MAX_GETS) {
        prog_error(ERR_SYNTAX, "Too many GETs");
        return;
    }

    scr.gets[scr.ngets].row = row;
    scr.gets[scr.ngets].col = col;
    str_copy(scr.gets[scr.ngets].varname, varname, MEMVAR_NAMELEN);
    if (picture)
        str_copy(scr.gets[scr.ngets].picture, picture, sizeof(scr.gets[scr.ngets].picture));
    else
        scr.gets[scr.ngets].picture[0] = '\0';

    if (range_lo && range_hi) {
        scr.gets[scr.ngets].has_range = 1;
        scr.gets[scr.ngets].range_lo = *range_lo;
        scr.gets[scr.ngets].range_hi = *range_hi;
    } else {
        scr.gets[scr.ngets].has_range = 0;
    }

    if (valid_expr)
        str_copy(scr.gets[scr.ngets].valid_expr, valid_expr, sizeof(scr.gets[scr.ngets].valid_expr));
    else
        scr.gets[scr.ngets].valid_expr[0] = '\0';

    if (when_expr)
        str_copy(scr.gets[scr.ngets].when_expr, when_expr, sizeof(scr.gets[scr.ngets].when_expr));
    else
        scr.gets[scr.ngets].when_expr[0] = '\0';

    /* Compile VALID/WHEN ASTs */
    scr.gets[scr.ngets].valid_ast = NULL;
    scr.gets[scr.ngets].when_ast = NULL;
    if (scr.gets[scr.ngets].valid_expr[0] && !strchr(scr.gets[scr.ngets].valid_expr, '&')) {
        const char *ast_err;
        scr.gets[scr.ngets].valid_ast = ast_compile(scr.gets[scr.ngets].valid_expr, store, &ast_err);
    }
    if (scr.gets[scr.ngets].when_expr[0] && !strchr(scr.gets[scr.ngets].when_expr, '&')) {
        const char *ast_err;
        scr.gets[scr.ngets].when_ast = ast_compile(scr.gets[scr.ngets].when_expr, store, &ast_err);
    }

    scr.gets[scr.ngets].is_field = 0;
    scr.gets[scr.ngets].field_index = -1;
    /* A variable's Get reads and assigns through a block, as Clipper's
     * preprocessor writes it -- a LOCAL stays reachable from a program's
     * reader (CA's GETSYS.PRG); a field's Get has no block, the editor
     * writes the record */
    if (memvar_find(store, varname, &val) == 0)
        scr.gets[scr.ngets].getobj = getsys_note_get_expr(row, col, varname, picture, valid_expr, when_expr);
    else
        scr.gets[scr.ngets].getobj = getsys_note_get(row, col, varname, picture, valid_expr, when_expr);

    /* Get current value for display */
    if (memvar_find(store, varname, &val) == 0) {
        val_to_string(&val, scr.gets[scr.ngets].initial, sizeof(scr.gets[scr.ngets].initial));
        scr.gets[scr.ngets].type = val.type;
        scr.gets[scr.ngets].width = strlen(scr.gets[scr.ngets].initial);
        /* a number's field is ten wide at least (STR()'s width); a string's
         * is its own length, as Clipper's Get makes it ("abc" is three) */
        if (val.type == VAL_NUM && scr.gets[scr.ngets].width < 10) scr.gets[scr.ngets].width = 10;
        if (scr.gets[scr.ngets].width < 1) scr.gets[scr.ngets].width = 1;
    } else {
        /* Try database field */
        expr_ctx_t *ctx = cmd_get_expr_ctx();
        dbf_t *db = ctx->db;
        int fi = -1;
        if (db && dbf_is_open(db))
            fi = dbf_find_field(db, varname);
        if (fi >= 0) {
            char raw[256];
            dbf_get_field_raw(db, fi, raw, sizeof(raw));
            trim_right(raw);
            scr.gets[scr.ngets].is_field = 1;
            scr.gets[scr.ngets].field_index = fi;
            if (db->fields[fi].type == 'N') {
                double nv = atof(raw);
                scr.gets[scr.ngets].type = VAL_NUM;
                val_to_string(&(value_t){.type=VAL_NUM,.num=nv},
                              scr.gets[scr.ngets].initial,
                              sizeof(scr.gets[scr.ngets].initial));
            } else {
                scr.gets[scr.ngets].type = VAL_CHAR;
                str_copy(scr.gets[scr.ngets].initial, raw,
                         sizeof(scr.gets[scr.ngets].initial));
            }
            scr.gets[scr.ngets].width = db->fields[fi].length;
        } else {
            scr.gets[scr.ngets].initial[0] = '\0';
            scr.gets[scr.ngets].type = VAL_CHAR;
            scr.gets[scr.ngets].width = 10;
        }
    }

    /* Display the field, as wide as the picture lets it be (@S<n> is a
     * window; a 127-wide value painted whole ran over DBU's dialog) */
    get_show_initial(&scr.gets[scr.ngets]);
    scr.ngets++;
}

#if HAS_TERM
int read_dbase_key(void);
#endif

typedef struct get_picture get_picture_t;
static void parse_get_picture(const char *p, get_picture_t *pic);
static int get_picture_window(const char *picture, int value_width);

/* The GET's value shown where it sits, cut to the picture's window */
static void get_show_initial(const get_entry_t *g) {
    char shown[256];
    int w = get_picture_window(g->picture, (int)strlen(g->initial));
    str_copy(shown, g->initial, sizeof(shown));
    if (g->type == VAL_NUM) {
        /* a number sits right-justified in its field, as the editor shows it
         * (RL's "@ 06,16 GET lbl_height PICTURE "999"" showed "5  ") */
        int n = (int)strlen(shown);
        if (n < w && w < (int)sizeof(shown)) {
            memmove(shown + (w - n), shown, (size_t)n + 1);
            memset(shown, ' ', (size_t)(w - n));
        }
    }
    if (w < (int)strlen(shown)) shown[w] = '\0';
    goto_pos(g->row, g->col);
#if HAS_TERM
    if (scr.term_available)
        screen_tputs(shown);
    else
#endif
    if (!screen_stream_quiet())
        printf("%s", shown);
    screen_update_pos(g->row, g->col, (int)strlen(shown));
}

/* A GET from a Get object (READMODAL over its array): the value it shows
 * comes with it, and the editor stores back through the object (its block
 * when it has one) rather than by name */
void screen_get_object(int row, int col, const char *name, const char *picture,
                       const value_t *initial, const value_t *obj) {
    get_entry_t *g;
    if (scr.ngets >= MAX_GETS) {
        prog_error(ERR_SYNTAX, "Too many GETs");
        return;
    }
    g = &scr.gets[scr.ngets];
    memset(g, 0, sizeof(*g));
    g->row = row;
    g->col = col;
    str_copy(g->varname, name ? name : "", MEMVAR_NAMELEN);
    str_copy(g->picture, picture ? picture : "", sizeof(g->picture));
    g->field_index = -1;
    g->getobj = *obj;
    val_to_string(initial, g->initial, sizeof(g->initial));
    g->type = initial->type;
    g->width = (int)strlen(g->initial);
    if (g->width < 1) g->width = 1;
    get_show_initial(g);
    scr.ngets++;
}

/* @ GET <expression target>: the value shown comes from evaluating it, and
 * the editor stores back through the Get object's block (getsys makes one
 * around the expression) */
void screen_get_expr(int row, int col, const char *target, const char *picture,
                     const value_t *range_lo, const value_t *range_hi,
                     const char *valid_expr, const char *when_expr) {
    get_entry_t *g;
    value_t v;
    expr_ctx_t *ctx = cmd_get_expr_ctx();
    if (scr.ngets >= MAX_GETS) {
        prog_error(ERR_SYNTAX, "Too many GETs");
        return;
    }
    g = &scr.gets[scr.ngets];
    memset(g, 0, sizeof(*g));
    g->row = row;
    g->col = col;
    str_copy(g->varname, target, MEMVAR_NAMELEN);
    str_copy(g->picture, picture ? picture : "", sizeof(g->picture));
    g->field_index = -1;
    if (range_lo && range_hi) { g->has_range = 1; g->range_lo = *range_lo; g->range_hi = *range_hi; }
    if (valid_expr) str_copy(g->valid_expr, valid_expr, sizeof(g->valid_expr));
    if (when_expr) str_copy(g->when_expr, when_expr, sizeof(g->when_expr));
    if (g->valid_expr[0] && !strchr(g->valid_expr, '&')) { const char *e; g->valid_ast = ast_compile(g->valid_expr, cmd_get_memvar_store(), &e); }
    if (g->when_expr[0] && !strchr(g->when_expr, '&')) { const char *e; g->when_ast = ast_compile(g->when_expr, cmd_get_memvar_store(), &e); }
    g->getobj = getsys_note_get_expr(row, col, target, picture, valid_expr, when_expr);
    if (ast_eval_dynamic(target, ctx, &v) != 0) { ctx->error = NULL; v = val_str(""); }
    val_to_string(&v, g->initial, sizeof(g->initial));
    g->type = v.type;
    g->width = (int)strlen(g->initial);
    if (g->width < 1) g->width = 1;
    get_show_initial(g);
    scr.ngets++;
}

/* The edited value back where it came from: the Get object\x27s block when
 * the entry carries one, else the variable by name */
static void get_store_value(get_entry_t *g, const value_t *v) {
    if (g->getobj.type == VAL_OBJECT && getsys_has_block(&g->getobj)) {
        getsys_varput(&g->getobj, v);
        return;
    }
    if (g->is_field) {
        /* a field GET stores into the record only: it also made a memvar
         * of the field's name once, and the next GET on the field found
         * that memvar first (DBU's second field showed the first's name
         * and kept none of its own) */
        expr_ctx_t *fctx = cmd_get_expr_ctx();
        dbf_t *fdb = fctx->db;
        if (fdb && dbf_is_open(fdb)) {
            char fraw[256];
            val_to_string(v, fraw, sizeof(fraw));
            dbf_set_field_raw(fdb, g->field_index, fraw);
            dbf_flush_record(fdb);
        }
        return;
    }
    if (g->varname[0]) memvar_set(cmd_get_memvar_store(), g->varname, v);
}


/* A field GET's value as it was shown, back into the record (VALID refused
 * the entry) */
static void get_store_initial(get_entry_t *g) {
    value_t v = g->type == VAL_NUM ? val_num(atof(g->initial)) : val_str(g->initial);
    get_store_value(g, &v);
}

/* ---- The GET editor's picture ---- */
struct get_picture {
    int clear, upper, alpha, scroll, literal_r;
    char tmpl[64];
};

/* The columns a GET occupies on screen: the template's length, or the
 * @S window, else the value's own width */
static int get_picture_window(const char *picture, int value_width) {
    get_picture_t pic;
    int w = value_width;
    parse_get_picture(picture, &pic);
    if (pic.tmpl[0]) w = (int)strlen(pic.tmpl);
    if (pic.scroll > 0 && pic.scroll < w) w = pic.scroll;
    return w;
}

static int get_insert_mode;   /* toggled by Ins; SET INSERT is not kept here */

static void parse_get_picture(const char *p, get_picture_t *pic) {
    memset(pic, 0, sizeof(*pic));
    if (!p) return;
    if (*p == '@') {
        p++;
        while (*p && *p != ' ') {
            switch (toupper((unsigned char)*p)) {
            case 'K': pic->clear = 1; break;
            case '!': pic->upper = 1; break;
            case 'A': pic->alpha = 1; break;
            case 'R': pic->literal_r = 1; break;
            case 'S':
                pic->scroll = atoi(p + 1);
                while (p[1] >= '0' && p[1] <= '9') p++;
                break;
            default: break;   /* B C D E N X Z J: formats this editor does not shape */
            }
            p++;
        }
        p = skip_ws(p);
    }
    str_copy(pic->tmpl, p, sizeof(pic->tmpl));
}

/* A template position that is a literal (not one of the characters a key
 * fills) */
static int get_picture_literal(const get_picture_t *pic, int pos) {
    char t = pic->tmpl[pos];
    return t && !strchr("9#ANXL!Y", t);
}

/* The character a key puts at pos under the picture, 0 to refuse it */
static int get_picture_accept(const get_picture_t *pic, int pos, int key) {
    char t = pic->tmpl[0] && pos < (int)strlen(pic->tmpl) ? pic->tmpl[pos] : 'X';
    int is_alpha = isalpha(key), is_digit = isdigit(key);
    if (pic->upper || t == '!') key = toupper(key);
    if (pic->alpha && !is_alpha && key != ' ') return 0;
    switch (t) {
    case '9': return is_digit ? key : 0;
    case '#': return (is_digit || key == ' ' || key == '.' || key == '+' || key == '-') ? key : 0;
    case 'A': return (is_alpha || key == ' ') ? key : 0;
    case 'N': return (is_alpha || is_digit || key == ' ') ? key : 0;
    case 'L': case 'Y': return strchr("TFYNtfyn", key) ? toupper(key) : 0;
    case 'X': case '!': return key;
    default:  return key;   /* a literal: typed over, stepped past after */
    }
}

static void get_paint(const get_entry_t *g, const char *buf, int fwidth, int dwidth, int win, int pos) {
    int fi;
    term_begin_update();
    screen_tgoto(g->row, g->col);
    for (fi = 0; fi < dwidth; fi++) {
        int k = win + fi;
        screen_tputc(k < fwidth ? buf[k] : ' ');
    }
    screen_tgoto(g->row, g->col + (pos - win));
    term_end_update();
}

/* An accepted value shown again in its field, through its picture -- a
 * Get displays its variable after assigning it ("050" typed is " 50") */
static void get_show_value(const get_entry_t *g, const value_t *v) {
    value_t a[2], r;
    char shown[256];
    int w;
    a[0] = *v;
    a[1] = val_str(g->picture);
    if (func_call(cmd_get_expr_ctx(), "TRANSFORM", a, 2, &r) != 0 || r.type != VAL_CHAR) {
        cmd_get_expr_ctx()->error = NULL;
        return;
    }
    str_copy(shown, val_text(&r), sizeof(shown));
    w = get_picture_window(g->picture, (int)strlen(shown));
    if (w < (int)strlen(shown)) shown[w] = '\0';
    screen_tgoto(g->row, g->col);
    screen_tputs(shown);
}

/* ---- GETSYS's scoreboard (Clipper's READ) ----
 * With SET SCOREBOARD on, row 0 from column 60 says "Ins" while inserting,
 * "Invalid Date" for a date that is not one, "Range: lo - hi" for a value
 * outside a RANGE; a message stays until a key is waiting -- the key is
 * not taken, the editor gets it -- and is then wiped. */
static int getsys_style(void) { return report_clipper_layout(); }

static int scoreboard_on(void) {
    const set_options_t *o = cmd_get_set_opts();
    return o && o->scoreboard;
}

static void scoreboard_say(int col, const char *text) {
    int r = shadow_row, c = shadow_col;
    screen_tgoto(0, col);
    screen_tputs(text);
    screen_tgoto(r, c);
}

static void scoreboard_insert(int inserting) {
    if (getsys_style() && scoreboard_on()) scoreboard_say(60, inserting ? "Ins" : "   ");
}

void scoreboard_message(const char *msg) {
    int len = (int)strlen(msg), col = scr_cols - 1 - len < 60 ? scr_cols - 1 - len : 60, k;
    char blank[96];
    if (!scoreboard_on()) return;
    if (col < 0) col = 0;
    scoreboard_say(col, msg);
#if HAS_TERM
    while (!screen_key_pending()) term_wait_key(250);
#endif
    for (k = 0; k < len && k < (int)sizeof(blank) - 1; k++) blank[k] = ' ';
    blank[k] = '\0';
    scoreboard_say(col, blank);
}

/* The text edited back into a value of the GET's own type: a date through
 * SET DATE (*bad set when the text is not a date), a logical from T/Y */
static value_t get_text_value(const get_entry_t *g, const char *buf, int *bad) {
    *bad = 0;
    if (g->type == VAL_NUM) return val_num(atof(buf));
    if (g->type == VAL_DATE) {
        int32_t d = date_from_display(buf, cmd_get_date_format(), cmd_get_epoch());
        const char *p;
        int digits = 0;
        for (p = buf; *p; p++) if (*p >= '0' && *p <= '9') digits = 1;
        if (d == 0 && digits) *bad = 1;
        return val_date(d);
    }
    if (g->type == VAL_LOGIC) {
        const char *p = buf;
        while (*p == ' ') p++;
        return val_logic(*p == 'T' || *p == 't' || *p == 'Y' || *p == 'y');
    }
    return val_str(buf);
}

/* A line typed for a character GET when there is no terminal: what the
 * picture's case rules make of it, as the full-screen editor makes of
 * each key -- every letter upper case under @!, and those at a template's
 * ! positions.  (Keys a template would refuse have no line-mode meaning
 * and are kept.) */
static void get_line_case(const get_entry_t *g, char *line) {
    get_picture_t pic;
    int k;
    if (g->type != VAL_CHAR) return;
    parse_get_picture(g->picture, &pic);
    for (k = 0; line[k]; k++)
        if (pic.upper || (k < (int)strlen(pic.tmpl) && pic.tmpl[k] == '!'))
            line[k] = (char)toupper((unsigned char)line[k]);
}

/* ---- READ ---- */
void screen_read(void) {
    memvar_store_t *store = cmd_get_memvar_store();
    int i;
    read_updated = 0;

#if HAS_TERM
    if (scr.term_available) {
        /* Full-screen READ with terminal support */
        int escaped = 0;
        term_set_raw(1);

        for (i = 0; i < scr.ngets && !escaped; i++) {
            char buf[256];
            int pos, len, key, nav = 0;   /* nav: -1 the GET before, 1 the READ is done */
            char kept_edit[256];
            int have_kept, refused = 0;
            str_copy(read_var, scr.gets[i].varname, sizeof(read_var));
            active_get = i;

            /* WHEN — skip field if false */
            if (scr.gets[i].when_ast) {
                value_t res;
                if (ast_eval(scr.gets[i].when_ast, cmd_get_expr_ctx(), &res) != 0 ||
                    (res.type == VAL_LOGIC && !res.logic))
                    continue;
            } else if (scr.gets[i].when_expr[0]) {
                value_t res;
                if (ast_eval_dynamic(scr.gets[i].when_expr, cmd_get_expr_ctx(), &res) != 0 ||
                    (res.type == VAL_LOGIC && !res.logic))
                    continue;
            }

            kept_edit[0] = '\0';
            have_kept = 0;
            for (;;) {
            /* The picture: functions after @ (K clear on the first key,
             * S<n> a scrolling window, ! upper case, A letters only, R
             * literals shown but not kept), then the template.  Its
             * length is the field's width (it was the whole string's, so
             * "@KS30" made a field four wide). */
            get_picture_t pic;
            int fwidth, dwidth, win = 0, first_key = 1;
            parse_get_picture(scr.gets[i].picture, &pic);
            if (scr.gets[i].type == VAL_DATE && !pic.tmpl[0]) {
                /* a date edits through the date format: digits where the
                 * date has them, its separators fixed ("99/99/99") */
                const char *t = scr.gets[i].initial;
                int k;
                for (k = 0; t[k] && k < (int)sizeof(pic.tmpl) - 1; k++)
                    pic.tmpl[k] = (t[k] == '/' || t[k] == '.' || t[k] == '-') ? t[k] : '9';
                pic.tmpl[k] = '\0';
            }
            fwidth = scr.gets[i].width;
            if (pic.tmpl[0]) fwidth = (int)strlen(pic.tmpl);
            if (fwidth > 255) fwidth = 255;
            if (fwidth < 1) fwidth = 1;
            dwidth = (pic.scroll > 0 && pic.scroll < fwidth) ? pic.scroll : fwidth;
            nav = 0;

            str_copy(buf, scr.gets[i].initial, sizeof(buf));
            len = strlen(buf);
            if (scr.gets[i].type == VAL_NUM) {
                /* a number sits right-justified in its field, and the
                 * first digit typed replaces it (Clipper's numeric GET;
                 * "20" over 10 became 200 here) */
                if (len < fwidth) {
                    memmove(buf + (fwidth - len), buf, (size_t)len + 1);
                    memset(buf, ' ', (size_t)(fwidth - len));
                    len = fwidth;
                }
                pic.clear = 1;
            }
            /* Pad to field width */
            while (len < fwidth && len < 255) { buf[len++] = ' '; }
            buf[len] = '\0';
            if (pic.tmpl[0]) {
                /* the template's literals stand in the field ("  /  /  ") */
                int k;
                for (k = 0; k < fwidth && pic.tmpl[k]; k++)
                    if (get_picture_literal(&pic, k)) buf[k] = pic.tmpl[k];
            }
            if (have_kept) {
                /* edited again after a refused value: the text as it was
                 * typed, the cursor home (GETSYS's oGet:home()) */
                str_copy(buf, kept_edit, sizeof(buf));
                len = (int)strlen(buf);
                while (len < fwidth && len < 255) buf[len++] = ' ';
                buf[len] = '\0';
                first_key = 0;
                have_kept = 0;
            }
            pos = 0;  /* cursor at start of field (dBase convention) */
            while (pos < fwidth && pic.tmpl[0] && get_picture_literal(&pic, pos)) pos++;

            get_paint(&scr.gets[i], buf, fwidth, dwidth, win, pos);

            for (;;) {
                key = screen_getkey();
                if (key != KEY_NONE && key != 0 && screen_check_key_handler(key)) {
                    /* a SET KEY handler ran; CLEAR GETS in it ends the READ,
                     * as Clipper's does (RL's F10: CLEAR GETS, exit_status := .T.) */
                    if (scr.ngets == 0) { last_key = key; escaped = 1; nav = 1; break; }
                    continue;
                }
                if (key == KEY_NONE && refused) key = K_ESC;   /* input gone after a refusal: no endless re-edit */
                if (key == K_ENTER || key == '\n' || key == KEY_NONE) { last_key = 13; break; }
                if (key == K_ESC) { /* Escape - cancel READ */
                    last_key = 27;
                    escaped = 1;
                    str_copy(buf, scr.gets[i].initial, sizeof(buf));
                    break;
                }
                if (key == K_UP || key == K_SH_TAB) { last_key = key; nav = -1; break; }
                if (key == K_DOWN || key == K_TAB) { last_key = key; nav = 0; break; }
                if (key == K_PGUP || key == K_PGDN || key == K_CTRL_W) { last_key = key; nav = 1; break; }
                if (key == K_LEFT) { if (pos > 0) pos--; }
                else if (key == K_RIGHT) { if (pos < fwidth - 1) pos++; }
                else if (key == K_HOME) pos = 0;
                else if (key == K_END) {
                    int e = fwidth;
                    while (e > 0 && buf[e - 1] == ' ') e--;
                    pos = e < fwidth ? e : fwidth - 1;
                } else if (key == K_INS) { get_insert_mode = !get_insert_mode; scoreboard_insert(get_insert_mode); }
                else if (key == K_DEL) {
                    memmove(buf + pos, buf + pos + 1, (size_t)(fwidth - pos - 1));
                    buf[fwidth - 1] = ' ';
                } else if (key == K_BS || key == 127) {
                    if (pos > 0) {
                        pos--;
                        memmove(buf + pos, buf + pos + 1, (size_t)(fwidth - pos - 1));
                        buf[fwidth - 1] = ' ';
                    }
                } else if (key == K_CTRL_Y) {
                    memset(buf + pos, ' ', (size_t)(fwidth - pos));
                } else if (key == K_CTRL_U) {
                    str_copy(buf, scr.gets[i].initial, sizeof(buf));
                    len = strlen(buf);
                    while (len < fwidth) buf[len++] = ' ';
                    buf[len] = '\0';
                } else if (key >= 32 && key <= 255) {
                    int ch = get_picture_accept(&pic, pos, key);
                    if (ch == 0) continue;   /* the template refuses it */
                    if (first_key && pic.clear) { memset(buf, ' ', (size_t)fwidth); pos = 0; }
                    first_key = 0;
                    if (get_insert_mode) {
                        memmove(buf + pos + 1, buf + pos, (size_t)(fwidth - pos - 1));
                    }
                    buf[pos] = (char)ch;
                    pos++;
                    /* a literal in the template is stepped over */
                    while (pos < fwidth && pic.tmpl[0] && get_picture_literal(&pic, pos)) pos++;
                    if (pos >= fwidth) {
                        if (!cmd_get_confirm()) {
                            last_key = 13;
                            get_paint(&scr.gets[i], buf, fwidth, dwidth, win, fwidth - 1);   /* the last character shows */
                            break;  /* auto-advance: field full, CONFIRM OFF */
                        }
                        pos = fwidth - 1;
                    }
                }
                first_key = 0;
                if (pos < win) win = pos;
                if (pos >= win + dwidth) win = pos - dwidth + 1;
                get_paint(&scr.gets[i], buf, fwidth, dwidth, win, pos);
            }

            if (escaped) break;  /* Escape exits READ without storing */

            /* Store value back */
            {
                value_t v;
                trim_right(buf);
                {
                    int bad;
                    v = get_text_value(&scr.gets[i], buf, &bad);
                    if (bad) {
                        if (getsys_style()) { scoreboard_message("Invalid Date"); scoreboard_insert(get_insert_mode); }
                        { str_copy(kept_edit, buf, sizeof(kept_edit)); have_kept = 1; refused = 1; continue; }   /* GETSYS: oGet:badDate(), the GET is edited again */
                    }
                }
                /* RANGE validation (match line-mode behavior) */
                if (getsys_style() && scr.gets[i].has_range) {
                    /* GETSYS's RangeCheck: only an edited value, the message
                     * on the scoreboard, the key left for the editor */
                    char now[256];
                    value_t lt, gt;
                    val_to_string(&v, now, sizeof(now));
                    if (strcmp(now, scr.gets[i].initial) != 0 &&
                        (ast_eval_binop(cmd_get_expr_ctx(), OP_LT, &v, &scr.gets[i].range_lo, &lt) != 0 || (lt.type == VAL_LOGIC && lt.logic) ||
                         ast_eval_binop(cmd_get_expr_ctx(), OP_GT, &v, &scr.gets[i].range_hi, &gt) != 0 || (gt.type == VAL_LOGIC && gt.logic))) {
                        char rmsg[160], a[64], b[64];
                        value_t ta[2], tr;
                        cmd_get_expr_ctx()->error = NULL;
                        ta[0] = scr.gets[i].range_lo; ta[1] = val_str("");
                        if (func_call(cmd_get_expr_ctx(), "TRANSFORM", ta, 2, &tr) == 0 && tr.type == VAL_CHAR) str_copy(a, val_text(&tr), sizeof(a)); else a[0] = '\0';
                        ta[0] = scr.gets[i].range_hi;
                        if (func_call(cmd_get_expr_ctx(), "TRANSFORM", ta, 2, &tr) == 0 && tr.type == VAL_CHAR) str_copy(b, val_text(&tr), sizeof(b)); else b[0] = '\0';
                        cmd_get_expr_ctx()->error = NULL;
                        snprintf(rmsg, sizeof(rmsg), "Range: %s - %s", skip_ws(a), skip_ws(b));
                        rmsg[79] = '\0';
                        scoreboard_message(rmsg);
                        { str_copy(kept_edit, buf, sizeof(kept_edit)); have_kept = 1; refused = 1; continue; }
                    }
                } else if (scr.gets[i].has_range && v.type == VAL_NUM) {
                    double n = v.num;
                    double lo = scr.gets[i].range_lo.num;
                    double hi = scr.gets[i].range_hi.num;
                    if (n < lo || n > hi) {
                        char rmsg[80];
                        snprintf(rmsg, sizeof(rmsg), "Range: %g to %g", lo, hi);
                        screen_tgoto(scr_rows - 1, 1);
                        screen_tputs(rmsg);
                        term_getkey();  /* wait for acknowledgment */
                        screen_tgoto(scr_rows - 1, 1);
                        {
                            int ri;
                            for (ri = 0; ri < 40; ri++) screen_tputc(' ');
                        }
                        { str_copy(kept_edit, buf, sizeof(kept_edit)); have_kept = 1; refused = 1; continue; }  /* reject, keep current value */
                    }
                }
                /* VALID validation — set memvar temporarily, restore on failure */
                if (scr.gets[i].valid_ast || scr.gets[i].valid_expr[0]) {
                    value_t old_val, res;
                    int had_old = (memvar_find(store, scr.gets[i].varname, &old_val) == 0);
                    int ok;
                    get_store_value(&scr.gets[i], &v);
                    if (scr.gets[i].valid_ast)
                        ok = (ast_eval(scr.gets[i].valid_ast, cmd_get_expr_ctx(), &res) == 0 &&
                              res.type == VAL_LOGIC && res.logic);
                    else
                        ok = (ast_eval_dynamic(scr.gets[i].valid_expr, cmd_get_expr_ctx(), &res) == 0 &&
                              res.type == VAL_LOGIC && res.logic);
                    if (!ok) {
                        if (had_old) memvar_set(store, scr.gets[i].varname, &old_val);
                        else if (scr.gets[i].is_field) get_store_initial(&scr.gets[i]);
                        if (getsys_style()) { str_copy(kept_edit, buf, sizeof(kept_edit)); have_kept = 1; refused = 1; continue; }   /* GETSYS says nothing: the GET is edited again */
                        screen_tgoto(scr_rows - 1, 1);
                        screen_tputs("Invalid entry.");
                        term_getkey();  /* wait for acknowledgment */
                        screen_tgoto(scr_rows - 1, 1);
                        {
                            int ri;
                            for (ri = 0; ri < 40; ri++) screen_tputc(' ');
                        }
                        { str_copy(kept_edit, buf, sizeof(kept_edit)); have_kept = 1; refused = 1; continue; }  /* re-edit field */
                    }
                } else {
                    get_store_value(&scr.gets[i], &v);
                }
                {   /* UPDATED() */
                    char nv[256];
                    val_to_string(&v, nv, sizeof(nv));
                    if (strcmp(nv, scr.gets[i].initial) != 0) read_updated = 1;
                }
                if (getsys_style() && v.type == VAL_NUM) get_show_value(&scr.gets[i], &v);
                break;  /* accepted */
            }
            } /* for (;;) */
            if (nav < 0 && i > 0) i -= 2;
            else if (nav > 0) break;
        }

        term_set_raw(0);
        /* Set READKEY() value based on how READ was exited */
        read_exit_key = (last_key == 27) ? 36 : 12;
    } else
#endif
    {
        /* Fallback: line-mode input for each GET */
        for (i = 0; i < scr.ngets; i++) {
            char line[256];
            value_t v;

            /* Check WHEN condition — skip field if false or eval error */
            if (scr.gets[i].when_ast) {
                value_t res;
                if (ast_eval(scr.gets[i].when_ast, cmd_get_expr_ctx(), &res) != 0 ||
                    (res.type == VAL_LOGIC && !res.logic))
                    continue;
            } else if (scr.gets[i].when_expr[0]) {
                value_t res;
                if (ast_eval_dynamic(scr.gets[i].when_expr, cmd_get_expr_ctx(), &res) != 0 ||
                    (res.type == VAL_LOGIC && !res.logic))
                    continue;
            }

            for (;;) {
                /* Position cursor at GET location */
                goto_pos(scr.gets[i].row, scr.gets[i].col);
                scr.last_row = scr.gets[i].row;
                scr.last_col = scr.gets[i].col;

                if (read_line(line, sizeof(line)) < 0) break;
                
                {
                    int bad;
                    if (line[0] == '\0') {
                        v = get_text_value(&scr.gets[i], scr.gets[i].initial, &bad);   /* keep current value */
                    } else {
                        trim_right(line);
                        get_line_case(&scr.gets[i], line);
                        v = get_text_value(&scr.gets[i], line, &bad);
                        if (bad) { printf("\nInvalid date\n"); continue; }
                    }
                }

                /* RANGE validation */
                if (scr.gets[i].has_range && v.type == VAL_NUM) {
                    double n = v.num;
                    double lo = scr.gets[i].range_lo.num;
                    double hi = scr.gets[i].range_hi.num;
                    if (n < lo || n > hi) {
                        printf("\nRange: %g to %g\n", lo, hi);
                        continue;
                    }
                }

                /* VALID validation — set memvar temporarily, restore on failure */
                if (scr.gets[i].valid_ast || scr.gets[i].valid_expr[0]) {
                    value_t old_val, res;
                    int had_old = (memvar_find(store, scr.gets[i].varname, &old_val) == 0);
                    int ok;
                    get_store_value(&scr.gets[i], &v);
                    if (scr.gets[i].valid_ast)
                        ok = (ast_eval(scr.gets[i].valid_ast, cmd_get_expr_ctx(), &res) == 0 &&
                              res.type == VAL_LOGIC && res.logic);
                    else
                        ok = (ast_eval_dynamic(scr.gets[i].valid_expr, cmd_get_expr_ctx(), &res) == 0 &&
                              res.type == VAL_LOGIC && res.logic);
                    if (!ok) {
                        if (had_old) memvar_set(store, scr.gets[i].varname, &old_val);
                        else if (scr.gets[i].is_field) get_store_initial(&scr.gets[i]);
                        printf("\nInvalid entry.\n");
                        continue;
                    }
                } else {
                    get_store_value(&scr.gets[i], &v);
                }
                {   /* UPDATED() */
                    char nv[256];
                    val_to_string(&v, nv, sizeof(nv));
                    if (strcmp(nv, scr.gets[i].initial) != 0) read_updated = 1;
                }
                printf("\n");
                break;
            }
        }
        last_key = 13;
        read_exit_key = 12;
    }

    read_var[0] = '\0';
    active_get = -1;

    /* Free ASTs */
    {
        int j;
        for (j = 0; j < scr.ngets; j++) {
            ast_free(scr.gets[j].valid_ast);
            ast_free(scr.gets[j].when_ast);
            scr.gets[j].valid_ast = NULL;
            scr.gets[j].when_ast = NULL;
        }
    }
    scr.ngets = 0;
}

/* ---- @CLEAR TO ---- */
void screen_clear_region(int r1, int c1, int r2, int c2) {
#if HAS_TERM
    if (scr.term_available) {
        int r, w;
        w = c2 - c1 + 1;
        if (w <= 0) return;
        for (r = r1; r <= r2; r++) {
            int i;
            screen_tgoto(r, c1);
            for (i = 0; i < w; i++) screen_tputc(' ');
        }
        screen_tgoto(r1, c1);
        return;
    }
#endif
    /* Fallback: print blank lines */
    {
        int r;
        (void)c1; (void)c2;
        for (r = r1; r <= r2; r++) printf("\n");
    }
}

/* A message row (SET MESSAGE): blanked across the screen, the message
 * centred on it */
void screen_message(int row, const char *msg) {
    char blank[SHADOW_MAX_COLS + 1];
    int mlen = (int)strlen(msg), mcol;
    memset(blank, ' ', (size_t)scr_cols);
    blank[scr_cols] = '\0';
    screen_tgoto(row, 0);
    screen_tputs(blank);
    if (!mlen) return;
    mcol = (scr_cols - mlen) / 2;
    if (mcol < 0) mcol = 0;
    screen_tgoto(row, mcol);
    screen_tputs(msg);
}

/* ---- @TO (box drawing) ---- */
void screen_box(int r1, int c1, int r2, int c2, int dbl) {
    /* Box characters: single vs double */
    char h, v, tl, tr, bl, br;

    /* the line-drawing set the terminal shows DBU's frames with (CP437) */
    if (dbl) {
        h = (char)0xCD; v = (char)0xBA; tl = (char)0xC9; tr = (char)0xBB; bl = (char)0xC8; br = (char)0xBC;
    } else {
        h = (char)0xC4; v = (char)0xB3; tl = (char)0xDA; tr = (char)0xBF; bl = (char)0xC0; br = (char)0xD9;
    }

#if HAS_TERM
    if (scr.term_available) {
        int r, c;
        if (r1 == r2 || c1 == c2) {
            /* a line, not a box: @ 1,0 TO 1,MAXCOL() rules a row across */
            if (r1 == r2) { screen_tgoto(r1, c1); for (c = c1; c <= c2; c++) screen_tputc(h); }
            else for (r = r1; r <= r2; r++) { screen_tgoto(r, c1); screen_tputc(v); }
            return;
        }
        /* Top */
        screen_tgoto(r1, c1);
        screen_tputc(tl);
        for (c = c1 + 1; c < c2; c++) screen_tputc(h);
        screen_tputc(tr);
        /* Sides */
        for (r = r1 + 1; r < r2; r++) {
            screen_tgoto(r, c1); screen_tputc(v);
            screen_tgoto(r, c2); screen_tputc(v);
        }
        /* Bottom */
        screen_tgoto(r2, c1);
        screen_tputc(bl);
        for (c = c1 + 1; c < c2; c++) screen_tputc(h);
        screen_tputc(br);
        return;
    }
#endif
    /* Fallback: skip box drawing silently */
    (void)r1; (void)c1; (void)r2; (void)c2;
    (void)h; (void)v; (void)tl; (void)tr; (void)bl; (void)br;
}

void screen_box_frame(int r1, int c1, int r2, int c2, const char *frame) {
    size_t n = strlen(frame);
    char f[9];
    int i;
    if (n == 0) return;
    for (i = 0; i < 9; i++) f[i] = (size_t)i < n ? frame[i] : (i == 8 ? '\0' : frame[n - 1]);
    if (n == 1) { for (i = 0; i < 8; i++) f[i] = frame[0]; f[8] = '\0'; }
#if HAS_TERM
    if (scr.term_available) {
        int r, c;
        screen_tgoto(r1, c1);
        screen_tputc(f[0]);
        for (c = c1 + 1; c < c2; c++) screen_tputc(f[1]);
        screen_tputc(f[2]);
        for (r = r1 + 1; r < r2; r++) {
            screen_tgoto(r, c1);
            screen_tputc(f[7]);
            if (f[8]) for (c = c1 + 1; c < c2; c++) screen_tputc(f[8]);
            screen_tgoto(r, c2);
            screen_tputc(f[3]);
        }
        screen_tgoto(r2, c1);
        screen_tputc(f[6]);
        for (c = c1 + 1; c < c2; c++) screen_tputc(f[5]);
        screen_tputc(f[4]);
        return;
    }
#endif
    (void)r1; (void)c1; (void)r2; (void)c2;   /* no terminal: nothing to draw on */
}

/* ---- CLEAR ---- */
void screen_clear(void) {
#if HAS_TERM
    if (scr.term_available) {
        screen_tclear(0);
        screen_tgoto(0, 0);
        return;
    }
#endif
    /* Fallback: a screen of newlines */
    {
        int i;
        for (i = 0; i < scr_rows - 1; i++) printf("\n");
    }
    scr.last_row = 0;
    scr.last_col = 1;
    scr.nprompts = 0;
}

/* ---- CLEAR GETS ---- */
void screen_clear_gets(void) {
    int i;
    for (i = 0; i < scr.ngets; i++) {
        ast_free(scr.gets[i].valid_ast);
        ast_free(scr.gets[i].when_ast);
        scr.gets[i].valid_ast = NULL;
        scr.gets[i].when_ast = NULL;
    }
    scr.ngets = 0;
}

/* ---- @PROMPT ---- */
void screen_prompt(int row, int col, const char *text, const char *message) {
    if (scr.nprompts >= MAX_PROMPTS) {
        printf("Too many PROMPTs.\n");
        return;
    }

    scr.prompts[scr.nprompts].row = row;
    scr.prompts[scr.nprompts].col = col;
    str_copy(scr.prompts[scr.nprompts].text, text, sizeof(scr.prompts[0].text));
    if (message)
        str_copy(scr.prompts[scr.nprompts].message, message, sizeof(scr.prompts[0].message));
    else
        scr.prompts[scr.nprompts].message[0] = '\0';

    /* Display the prompt text at the specified position */
    goto_pos(row, col);
#if HAS_TERM
    if (scr.term_available)
        screen_tputs(text);
    else
#endif
    if (!screen_stream_quiet())
        printf("%s", text);
    screen_update_pos(row, col, (int)strlen(text));

    scr.nprompts++;
}

void screen_clear_prompts(void) {
    scr.nprompts = 0;
}

/* ---- MENU TO ---- */
int screen_menu_to(void) {
    int cur = 0;  /* 0-based index of current selection */
    int n = scr.nprompts;
    int msg_row = cmd_get_message_row();
    int wrap = cmd_get_wrap();
    int key;
    int i;

    if (n == 0) return 0;

#if HAS_TERM
    if (scr.term_available) {
        term_set_raw(1);

        for (;;) {
            /* Redraw all prompts: current in reverse, others normal */
            term_begin_update();
            for (i = 0; i < n; i++) {
                screen_tgoto(scr.prompts[i].row, scr.prompts[i].col);
                if (i == cur)
                    screen_tattr(7);  /* reverse */
                else
                    screen_tattr(0);  /* normal */
                screen_tputs(scr.prompts[i].text);
            }

            /* Show MESSAGE for current selection if SET MESSAGE is active */
            if (msg_row >= 0) {
                screen_tattr(0);
                screen_message(msg_row, scr.prompts[cur].message);
            }
            term_end_update();

            key = screen_getkey();
            if (key != KEY_NONE && key != 0 && screen_check_key_handler(key)) continue;

            if (key == '\r' || key == '\n') {
                last_key = 13;
                break;
            }
            if (key == 27) {  /* Escape */
                last_key = 27;
                cur = -1;
                break;
            }
            if (key == K_UP) {
                if (cur > 0) {
                    cur--;
                } else if (wrap) {
                    cur = n - 1;
                }
            } else if (key == K_DOWN) {
                if (cur < n - 1) {
                    cur++;
                } else if (wrap) {
                    cur = 0;
                }
            } else if (key == K_HOME) {
                cur = 0;
            } else if (key == K_END) {
                cur = n - 1;
            } else if (key == K_LEFT) {
                if (cur > 0) {
                    cur--;
                } else if (wrap) {
                    cur = n - 1;
                }
            } else if (key == K_RIGHT) {
                if (cur < n - 1) {
                    cur++;
                } else if (wrap) {
                    cur = 0;
                }
            }
        }

        /* Restore normal display for all prompts */
        screen_tattr(0);
        for (i = 0; i < n; i++) {
            screen_tgoto(scr.prompts[i].row, scr.prompts[i].col);
            screen_tputs(scr.prompts[i].text);
        }

        /* Clear message row */
        if (msg_row >= 0) {
            screen_message(msg_row, "");
        }

        term_set_raw(0);
    } else
#endif
    {
        /* No terminal: the choices listed, and a line read for the
         * choice -- its number, or a letter that starts a prompt (the
         * full-screen menu's first-letter key); an empty line (Enter) or
         * the end of input keeps the current one, 0 is Esc. */
        char line[64];
        const char *p;
        printf("Menu: ");
        for (i = 0; i < n; i++) {
            printf("%d. %s  ", i + 1, scr.prompts[i].text);
        }
        printf("\n");
        cur = 0;
        if (read_line(line, sizeof(line)) >= 0) {
            p = skip_ws(line);
            if (*p >= '0' && *p <= '9') {
                int k = atoi(p);
                if (k == 0) cur = -1;
                else if (k <= n) cur = k - 1;
            } else if (*p) {
                for (i = 0; i < n; i++) {
                    const char *t = skip_ws(scr.prompts[i].text);
                    if (toupper((unsigned char)*t) == toupper((unsigned char)*p)) { cur = i; break; }
                }
            }
        }
    }

    scr.nprompts = 0;  /* consume prompts after MENU TO */
    return (cur >= 0) ? cur + 1 : 0;  /* 1-based, 0 = Esc */
}

/* ---- SET COLOR TO ---- */

/* One colour of a pair: a letter run (N B G BG R RB GR W, U underline,
 * I inverse, X blank), then + or * for intensity */
static const char *parse_one_color(const char *p, int *color, int *bold, int *underline, int *inverse) {
    char run[4];
    int n = 0;
    while (*p && ((*p >= 'A' && *p <= 'Z') || (*p >= 'a' && *p <= 'z'))) {
        if (n < 3) run[n++] = (char)((*p >= 'a') ? *p - 32 : *p);
        p++;
    }
    run[n] = '\0';
    if (!strcmp(run, "N") || !strcmp(run, "X")) *color = 0;
    else if (!strcmp(run, "B")) *color = 4;
    else if (!strcmp(run, "G")) *color = 2;
    else if (!strcmp(run, "BG")) *color = 6;
    else if (!strcmp(run, "R")) *color = 1;
    else if (!strcmp(run, "RB")) *color = 5;
    else if (!strcmp(run, "GR")) *color = 3;
    else if (!strcmp(run, "W")) *color = 7;
    else if (!strcmp(run, "U")) { *color = 7; *underline = 1; }
    else if (!strcmp(run, "I")) { *color = 7; *inverse = 1; }
    while (*p == '+' || *p == '*') { *bold = 1; p++; }
    return p;
}

/* Apply one "fg/bg" pair to the terminal */
static void apply_color_pair(const char *p) {
    int fg = 7, bg = 0;
    int bold = 0, underline = 0, inverse = 0;
    int dummy = 0;
    p = skip_ws(p);
    p = parse_one_color(p, &fg, &bold, &underline, &inverse);
    p = skip_ws(p);
    if (*p == '/') {
        p = skip_ws(p + 1);
        p = parse_one_color(p, &bg, &dummy, &dummy, &dummy);
    }
    scr.fg_color = fg;
    scr.bg_color = bg;
#if HAS_TERM
    if (scr.term_available) {
        screen_tattr(0);  /* reset all attributes */
        if (bold) screen_tattr(1);
        if (underline) screen_tattr(4);
        if (inverse) screen_tattr(7);
        screen_tcolor(fg, bg);
    }
#endif
}

/* The n-th pair (1 standard, 2 enhanced, 3 border, 4 background, 5
 * unselected) of the current setting; Clipper's defaults for a pair the
 * setting leaves empty */
void screen_color_pair(int n) {
    static const char *dflt[] = { "W/N", "N/W", "N", "N", "N/W" };
    const char *p = color_spec;
    int i;
    for (i = 1; i < n && *p; i++) {
        const char *c = strchr(p, ',');
        if (!c) { p = ""; break; }
        p = c + 1;
    }
    p = skip_ws(p);
    if (*p == '\0' || *p == ',') p = (n >= 1 && n <= 5) ? dflt[n - 1] : "W/N";
    apply_color_pair(p);
}

void screen_set_color(const char *spec) {
    str_copy(color_spec, skip_ws(spec), sizeof(color_spec));   /* SETCOLOR() gives it back */
    apply_color_pair(color_spec);
}

/* The GET clause of @ ... GET and of @ ... SAY ... GET: the target (a
 * variable, an array element, an alias->field), PICTURE, RANGE, VALID,
 * WHEN; p is the text after the word GET */
static void at_get_clause(int row, int col, const char *p) {
    char varname[MEMVAR_NAMELEN];
    char picture[64] = "";
    char valid_expr[256] = "";
    char when_expr[256] = "";
    char target[512] = "";   /* an array element or alias->field: read and stored through a block */
    char expanded[1024];
    char range_text[300] = "";   /* "lo, hi" as written */
    int i = 0;
    int has_range = 0;
    value_t range_lo, range_hi;

    p = skip_ws(p);
    if (*p == '&') {
        /* @ ROW(), COL() GET &cur_field (RL's label editor): the macro is
         * the target's text */
        lex_macro_expand(p, cmd_get_memvar_store(), expanded, sizeof(expanded));
        p = skip_ws(expanded);
    }

    /* Parse variable name; M-> and MEMVAR-> say what it is anyway
     * (DBU's @ r, c GET M->wk_str) */
    if (strncasecmp(p, "M->", 3) == 0) p += 3;
    else if (strncasecmp(p, "MEMVAR->", 8) == 0) p += 8;
    {
        const char *tstart = p;
        while (is_ident_char(*p) && i < MEMVAR_NAMELEN - 1)
            varname[i++] = *p++;
        varname[i] = '\0';
        if (*p == '[' || (p[0] == '-' && p[1] == '>')) {
            const char *ast_err, *q = tstart;
            ast_node_t *tmp = ast_compile_adv(&q, cmd_get_memvar_store(), &ast_err);
            int len = (int)(q - tstart);
            ast_free(tmp);
            if (len > (int)sizeof(target) - 1) len = (int)sizeof(target) - 1;
            memcpy(target, tstart, len);
            target[len] = '\0';
            trim_right(target);
            p = q;
        }
    }

    /* Parse optional clauses */
    for (;;) {
        p = skip_ws(p);
        if (str_imatch(p, "PICTURE")) {
            p = skip_ws(p + 7);
            if (*p == '"' || *p == '\'') {
                char q = *p++;
                i = 0;
                while (*p && *p != q && i < 63)
                    picture[i++] = *p++;
                picture[i] = '\0';
                if (*p) p++;
            } else {
                /* an expression: PICTURE M->pfunc */
                value_t pv;
                expr_ctx_t *ctx = cmd_get_expr_ctx();
                if (ast_eval_adv(ctx, &p, &pv) == 0 && pv.type == VAL_CHAR)
                    str_copy(picture, pv.str, sizeof(picture));
                else
                    ctx->error = NULL;
            }
        } else if (str_imatch(p, "RANGE")) {
            expr_ctx_t *ctx = cmd_get_expr_ctx();
            const char *r0;
            p = skip_ws(p + 5);
            r0 = p;
            if (ast_eval_adv(ctx, &p, &range_lo) == 0) {
                p = skip_ws(p);
                if (*p == ',') p++;
                p = skip_ws(p);
                if (ast_eval_adv(ctx, &p, &range_hi) == 0) {
                    int len = (int)(p - r0);
                    has_range = 1;
                    if (len > (int)sizeof(range_text) - 1) len = (int)sizeof(range_text) - 1;
                    memcpy(range_text, r0, (size_t)len);
                    range_text[len] = '\0';
                    trim_right(range_text);
                }
            }
        } else if (str_imatch(p, "VALID")) {
            const char *ast_err;
            ast_node_t *tmp;
            p = skip_ws(p + 5);
            {
            const char *start = p;
            tmp = ast_compile_adv(&p, cmd_get_memvar_store(), &ast_err);
            ast_free(tmp);  /* only needed to advance p */
            {
            int len = (int)(p - start);
            if (len > 255) len = 255;
            memcpy(valid_expr, start, len);
            valid_expr[len] = '\0';
            }
            }
        } else if (str_imatch(p, "WHEN")) {
            const char *ast_err;
            ast_node_t *tmp;
            p = skip_ws(p + 4);
            {
            const char *start = p;
            tmp = ast_compile_adv(&p, cmd_get_memvar_store(), &ast_err);
            ast_free(tmp);  /* only needed to advance p */
            {
            int len = (int)(p - start);
            if (len > 255) len = 255;
            memcpy(when_expr, start, len);
            when_expr[len] = '\0';
            }
            }
        } else {
            break;
        }
    }

    if (target[0])
        screen_get_expr(row, col, target, picture,
                        has_range ? &range_lo : NULL,
                        has_range ? &range_hi : NULL,
                        valid_expr, when_expr);
    else
        screen_get(row, col, varname, picture,
                   has_range ? &range_lo : NULL,
                   has_range ? &range_hi : NULL,
                   valid_expr, when_expr);
    if (has_range && !valid_expr[0] && scr.ngets > 0 && func_is_overridden("READMODAL")) {
        /* Clipper's std.ch: RANGE lo, hi is VALID {|_1| RangeCheck(_1,, lo, hi)}
         * -- a program's reader (CA's GETSYS) checks it through the Get */
        char text[400];
        snprintf(text, sizeof(text), "{|_1| RangeCheck(_1,, %s)}", range_text);
        getsys_set_postblock(&scr.gets[scr.ngets - 1].getobj, text);
    }
}

/* ---- Parse @ command ---- */
void screen_at_cmd(const char *line) {
    const char *p = skip_ws(line);
    int row, col;
    expr_ctx_t *ctx = cmd_get_expr_ctx();
    value_t v;

    /* Skip @ */
    if (*p != '@') return;
    p++;
    p = skip_ws(p);

    /* Parse row (expression) */
    {
        if (ast_eval_adv(ctx, &p, &v) != 0 || v.type != VAL_NUM) {
            printf("Syntax: @ row,col SAY|GET|CLEAR TO|TO ...\n");
            return;
        }
        row = (int)v.num;
        p = skip_ws(p);
        if (*p == ',') p++;
        p = skip_ws(p);
        if (ast_eval_adv(ctx, &p, &v) != 0 || v.type != VAL_NUM) {
            printf("Syntax: @ row,col SAY|GET|CLEAR TO|TO ...\n");
            return;
        }
        col = (int)v.num;
        p = skip_ws(p);
    }

    /* @ top, left, bottom, right BOX frame (Clipper) */
    if (*p == ',') {
        int r2, c2;
        value_t fv;
        p = skip_ws(p + 1);
        if (ast_eval_adv(ctx, &p, &v) != 0 || v.type != VAL_NUM) { printf("Syntax: @ t,l,b,r BOX frame\n"); return; }
        r2 = (int)v.num;
        p = skip_ws(p);
        if (*p == ',') p++;
        p = skip_ws(p);
        if (ast_eval_adv(ctx, &p, &v) != 0 || v.type != VAL_NUM) { printf("Syntax: @ t,l,b,r BOX frame\n"); return; }
        c2 = (int)v.num;
        p = skip_ws(p);
        if (!str_imatch(p, "BOX")) { printf("Syntax: @ t,l,b,r BOX frame\n"); return; }
        p = skip_ws(p + 3);
        if (ast_eval_adv(ctx, &p, &fv) != 0 || fv.type != VAL_CHAR) { printf("Syntax: @ t,l,b,r BOX frame\n"); return; }
        screen_box_frame(row, col, r2, c2, fv.str);
        return;
    }

    /* Sub-command */
    if (str_imatch(p, "SAY")) {
        char picture[64] = "";
        const char *pic_pos;
        char expr_str[256];
        char say_text[512];
        const char *get_clause = NULL;

        p = skip_ws(p + 3);
        {
            /* @ r,c SAY ... GET ...: the GET follows the text, one column on
             * (RL's @ 19,10 SAY "Width" GET m_width PICTURE "99"); it was
             * handed to the expression and lost, and its PICTURE taken as
             * the SAY's */
            const char *g = p;
            int depth = 0;
            while (*g) {
                if (*g == '"' || *g == '\'') { char q = *g++; while (*g && *g != q) g++; if (*g) g++; continue; }
                if (*g == '[' && (g == p || !is_ident_char(g[-1]))) { while (*g && *g != ']') g++; if (*g) g++; continue; }
                if (*g == '(' || *g == '[') depth++;
                else if (*g == ')' || *g == ']') depth--;
                else if (depth == 0 && (g == p || !is_ident_char(g[-1])) && strncasecmp(g, "GET", 3) == 0 && (g[3] == ' ' || g[3] == '\t')) { get_clause = g + 3; break; }
                g++;
            }
            if (get_clause) {
                int len = (int)(g - p);
                if (len > (int)sizeof(say_text) - 1) len = (int)sizeof(say_text) - 1;
                memcpy(say_text, p, (size_t)len);
                say_text[len] = '\0';
                trim_right(say_text);
                p = say_text;
            }
        }

        /* Find PICTURE clause */
        pic_pos = p;
        {
            const char *f = p;
            const char *found = NULL;
            while (*f) {
                if (str_imatch(f, "PICTURE")) {
                    found = f;
                    break;
                }
                /* Skip string literals */
                if (*f == '"' || *f == '\'') {
                    char q = *f++;
                    while (*f && *f != q) f++;
                    if (*f) f++;
                    continue;
                }
                f++;
            }
            if (found) {
                int len = (int)(found - p);
                if (len > (int)sizeof(expr_str) - 1) len = (int)sizeof(expr_str) - 1;
                memcpy(expr_str, p, len);
                expr_str[len] = '\0';
                trim_right(expr_str);
                found = skip_ws(found + 7);
                /* Picture is quoted string */
                if (*found == '"' || *found == '\'') {
                    char q = *found++;
                    int i = 0;
                    while (*found && *found != q && i < 63)
                        picture[i++] = *found++;
                    picture[i] = '\0';
                }
            } else {
                str_copy(expr_str, p, sizeof(expr_str));
                trim_right(expr_str);
            }
        }

        screen_say(row, col, expr_str, picture);
        if (get_clause) at_get_clause(shadow_row, shadow_col + 1, get_clause);

    } else if (str_imatch(p, "GET")) {
        at_get_clause(row, col, p + 3);

    } else if (str_imatch(p, "CLEAR")) {
        int r2 = scr_rows - 1, c2 = scr_cols - 1;
        p = skip_ws(p + 5);
        if (str_imatch(p, "TO")) {   /* the corner: expressions */
            p = skip_ws(p + 2);
            if (ast_eval_adv(ctx, &p, &v) == 0 && v.type == VAL_NUM) r2 = (int)v.num;
            p = skip_ws(p);
            if (*p == ',') p++;
            p = skip_ws(p);
            if (ast_eval_adv(ctx, &p, &v) == 0 && v.type == VAL_NUM) c2 = (int)v.num;
        }
        screen_clear_region(row, col, r2, c2);

    } else if (str_imatch(p, "TO")) {
        int r2, c2;
        int dbl = 0;
        p = skip_ws(p + 2);
        r2 = row; c2 = col;
        if (ast_eval_adv(ctx, &p, &v) == 0 && v.type == VAL_NUM) r2 = (int)v.num;
        p = skip_ws(p);
        if (*p == ',') p++;
        p = skip_ws(p);
        if (ast_eval_adv(ctx, &p, &v) == 0 && v.type == VAL_NUM) c2 = (int)v.num;
        p = skip_ws(p);
        if (str_imatch(p, "DOUBLE")) dbl = 1;
        screen_box(row, col, r2, c2, dbl);

    } else if (str_imatch(p, "PROMPT")) {
        /* @ row, col PROMPT <cExp> [MESSAGE <cExp>]: expressions, as the
         * guide has them (RL's menu is PROMPT aMainMenu[1]; a quoted
         * literal was all this read) */
        char text[80] = "";
        char message[160] = "";
        p = skip_ws(p + 6);
        if (ast_eval_adv(ctx, &p, &v) == 0 && v.type == VAL_CHAR) {
            str_copy(text, val_text(&v), sizeof(text));
        } else {
            if (ctx->error) ctx->error = NULL;
            printf("Syntax: @ row,col PROMPT <text> [MESSAGE <help>]\n");
            return;
        }
        p = skip_ws(p);
        if (str_imatch(p, "MESSAGE")) {
            p = skip_ws(p + 7);
            if (ast_eval_adv(ctx, &p, &v) == 0 && v.type == VAL_CHAR) str_copy(message, val_text(&v), sizeof(message));
            else if (ctx->error) ctx->error = NULL;
        }

        screen_prompt(row, col, text, message);

    } else if (*p == '\0') {   /* @ row, col alone: the line cleared from there, the cursor left there */
        screen_clear_region(row, col, row, scr_cols - 1);
        screen_setpos(row, col);
    } else {
        printf("Syntax: @ row,col SAY|GET|PROMPT|CLEAR TO|TO ...\n");
    }
}

/* ROW() / COL(): with a terminal, the shadow's cursor, which every write
 * and move keeps (last_row/last_col trail @ SAY only: DBU's GetNew(Row(),
 * Col(), ...) after a TBrowse hilite landed its GET at the screen edge) */
int screen_get_row(void) {
#if HAS_TERM
    if (scr.term_available) return shadow_row;
#endif
    return scr.last_row;
}
int screen_get_col(void) {
#if HAS_TERM
    if (scr.term_available) return shadow_col;
#endif
    return scr.last_col;
}
int screen_get_prow(void) { return print_row; }
int screen_get_pcol(void) { return print_col; }

/* EJECT: a form feed to a printer file; on the console, which has no
 * pages, a line that says where one would be */
void screen_eject(void) {
    FILE *fp = cmd_get_printer_fp();
    if (fp) fputs("\f\r", fp);
    else out_console("\n--- PAGE ---\n");
    print_row = 0;
    print_col = 0;
}

/* SET MARGIN's columns open every line the printer writes on, and count
 * in PCOL() */
static void printer_put(const char *s, size_t n, int write) {
    char buf[256];
    FILE *fp = cmd_get_printer_fp();
    if (!write || !n) return;
    if (fp) { fwrite(s, 1, n, fp); return; }
    while (n) {
        size_t k = n < sizeof(buf) - 1 ? n : sizeof(buf) - 1;
        memcpy(buf, s, k);
        buf[k] = '\0';
        out_console(buf);
        s += k;
        n -= k;
    }
}

void screen_print_stream(const char *s, int write) {
    static const char spaces[] = "                                ";
    const char *run;
    if (!s) return;
    for (run = s; *s; s++) {
        if (*s == '\n') { print_row++; print_col = 0; continue; }
        if (*s == '\r') { print_col = 0; continue; }
        if (print_col == 0 && cmd_get_margin() > 0) {
            int m = cmd_get_margin();
            printer_put(run, (size_t)(s - run), write);
            run = s;
            print_col = m;
            while (m > 0) {
                int k = m < (int)sizeof(spaces) - 1 ? m : (int)sizeof(spaces) - 1;
                printer_put(spaces, (size_t)k, write);
                m -= k;
            }
        }
        print_col++;
    }
    printer_put(run, (size_t)(s - run), write);
}

void screen_print_text(const char *s) {
    screen_print_stream(s, 1);
}

/* To a place on the page: a row above the head's is the next page (Clipper
 * ejects), then new lines to the row; a column behind the head is a
 * carriage return, then spaces to the column */
void screen_print_at(int row, int col, const char *s) {
    int margin = cmd_get_margin(), need;
    if (row < print_row) screen_eject();
    while (print_row < row) screen_print_text("\n");
    if (col + margin < print_col) screen_print_text("\r");
    /* at the line's start the first character written brings the margin */
    need = print_col == 0 ? col : col + margin - print_col;
    while (need-- > 0) screen_print_text(" ");
    screen_print_text(s);
}

/* ---- Keyboard functions ---- */

/* Translate a raw key (possibly an escape sequence) to dBase key code.
 * In raw terminal mode, arrow keys arrive as ESC [ A/B/C/D sequences.
 * term_getkey() reads one byte at a time, so we must read the sequence. */
#if HAS_TERM
int read_dbase_key(void) {
    int ch = term_getkey();
    if (ch < 0) return KEY_NONE;
    if (ch == 27) {
        int ch2;
        /* Possible escape sequence -- check if more bytes follow */
        if (!term_kbhit()) return 27;  /* bare Escape */
        ch2 = term_getkey();
        if (ch2 == '[') {
            /* CSI: parameters (digits and ;) then a final byte.  The
             * second parameter is the modifier: 5 is Ctrl (xterm). */
            int params[4] = { 0, 0, 0, 0 };
            int np = 0, have = 0, final = 0, ctrl;
            for (;;) {
                int c;
                if (!term_kbhit()) return 0;   /* a torn sequence: nothing */
                c = term_getkey();
                if (c >= '0' && c <= '9') {
                    if (np < 4) params[np] = params[np] * 10 + (c - '0');
                    have = 1;
                } else if (c == ';') {
                    if (np < 3) np++;
                    have = 1;
                } else {
                    final = c;
                    break;
                }
            }
            if (have) np++;
            ctrl = np >= 2 && params[1] == 5;
            switch (final) {
            case 'A': return K_UP;
            case 'B': return K_DOWN;
            case 'C': return ctrl ? K_CTRL_RIGHT : K_RIGHT;
            case 'D': return ctrl ? K_CTRL_LEFT : K_LEFT;
            case 'H': return ctrl ? K_CTRL_HOME : K_HOME;
            case 'F': return ctrl ? K_CTRL_END : K_END;
            case 'Z': return K_SH_TAB;
            case '~':
                switch (params[0]) {
                case 1:  return ctrl ? K_CTRL_HOME : K_HOME;
                case 2:  return K_INS;
                case 3:  return K_DEL;
                case 4:  return ctrl ? K_CTRL_END : K_END;
                case 5:  return ctrl ? K_CTRL_PGUP : K_PGUP;
                case 6:  return ctrl ? K_CTRL_PGDN : K_PGDN;
                case 7:  return K_HOME;
                case 8:  return K_END;
                case 11: return K_F1;
                case 12: return K_F2;
                case 13: return K_F3;
                case 14: return K_F4;
                case 15: return K_F5;
                case 17: return K_F6;
                case 18: return K_F7;
                case 19: return K_F8;
                case 20: return K_F9;
                case 21: return K_F10;
                case 23: return K_F11;
                case 24: return K_F12;
                default: return 0;
                }
            default:
                return 0;   /* an unknown sequence is no key */
            }
        } else if (ch2 == 'O') {
            /* SS3 sequences: F1-F4 on some terminals */
            int ch3;
            if (!term_kbhit()) return ch2;
            ch3 = term_getkey();
            switch (ch3) {
            case 'P': return K_F1;
            case 'Q': return K_F2;
            case 'R': return K_F3;
            case 'S': return K_F4;
            case 'H': return K_HOME;
            case 'F': return K_END;
            default: return ch3;
            }
        }
        if ((ch2 >= 'a' && ch2 <= 'z') || (ch2 >= 'A' && ch2 <= 'Z')) {
            /* Alt and a letter, as a terminal sends it (ESC, the letter):
             * Clipper's code, 256 and the PC scan code (Alt-W is 273) */
            static const short alt[26] = {
                286, 304, 302, 288, 274, 289, 290, 291, 279, 292, 293, 294, 306,
                305, 280, 281, 272, 275, 287, 276, 278, 303, 273, 301, 277, 300 };
            return alt[(ch2 | 0x20) - 'a'];
        }
        return ch2; /* ESC + something else */
    }
    /* Map backspace variants */
    if (ch == 127) return 8;  /* normalize DEL to BS */
    return ch;
}
#endif

static int pushed_key;   /* a key NEXTKEY() read and left for the next INKEY() */
static int typeahead_next(void);

/* The typeahead buffer KEYBOARD fills: read before the terminal by every
 * wait state, cleared by CLEAR TYPEAHEAD and by KEYBOARD itself.  Each
 * byte of the string is one key, 0..255 as Clipper (and Harbour) have it:
 * CHR(252) comes back as 252, not F5. */
#define TYPEAHEAD_MAX 256
static int typeahead[TYPEAHEAD_MAX];
static int typeahead_head, typeahead_len;

void screen_keyboard(const char *s) {
    typeahead_head = typeahead_len = 0;
    pushed_key = 0;
    while (*s && typeahead_len < TYPEAHEAD_MAX) typeahead[typeahead_len++] = (unsigned char)*s++;
}

void screen_clear_typeahead(void) {
    typeahead_head = typeahead_len = 0;
    pushed_key = 0;
}

/* A key waiting, anywhere: NEXTKEY's pushback, the typeahead buffer, the
 * terminal.  The user interface functions call their user function with
 * the idle mode when there is none. */
int screen_key_pending(void) {
    if (pushed_key > 0 || typeahead_head < typeahead_len) return 1;
#if HAS_TERM
    if (scr.term_available) {
        int hit;
        term_set_raw(1);
        hit = term_kbhit();
        term_set_raw(0);
        return hit;
    }
#endif
    return 0;
}

/* The next key: NEXTKEY's pushback, then the typeahead buffer; 0 when
 * neither has one */
static int typeahead_next(void) {
    int k;
    if (pushed_key > 0) { k = pushed_key; pushed_key = 0; return k; }
    if (typeahead_head < typeahead_len) return typeahead[typeahead_head++];
    return 0;
}

/* A key for an editor's loop (READ, MENU TO, BROWSE): the buffers first,
 * KEYBOARD's stuffing included, then the terminal */
int screen_getkey(void) {
    int k = typeahead_next();
    if (k) { last_key = k; return k; }
#if HAS_TERM
    return read_dbase_key();
#else
    return 0;
#endif
}

int screen_nextkey(void) {
    if (pushed_key <= 0) {
        if (typeahead_head < typeahead_len) return typeahead[typeahead_head];
        pushed_key = screen_inkey(0.0);
    }
    return pushed_key > 0 ? pushed_key : 0;
}

void screen_set_printer_pos(int row, int col) {
    print_row = row;
    print_col = col;
}

static int key_trace = -1;
int screen_inkey(double timeout) {
    if (key_trace < 0) { const char *e = getenv("XBASE_KEY_TRACE"); key_trace = e && *e; }
    if (key_trace) fprintf(stderr, "[inkey t=%g pushed=%d ta=%d/%d handlers=%d term=%d]\n", timeout, pushed_key, typeahead_head, typeahead_len, key_handler_count, scr.term_available);
    if (pushed_key > 0) {
        int k = pushed_key;
        pushed_key = 0;
        last_key = k;
        return k;
    }
    if (typeahead_head < typeahead_len) {
        int k = typeahead[typeahead_head++];
        last_key = k;
        if (screen_check_key_handler(k)) return 0;
        return k;
    }
#if HAS_TERM
    if (scr.term_available) {
        int key;
        term_set_raw(1);

        if (timeout == 0.0) {
            /* Poll — non-blocking */
            if (!term_kbhit()) {
                term_set_raw(0);
                return 0;
            }
            key = read_dbase_key();
            if (key_trace) fprintf(stderr, "[inkey poll read %d]\n", key);
        } else if (timeout < 0.0) {
            /* Wait forever (INKEY(0) in dBase = wait forever) */
            key = read_dbase_key();
            if (key_trace) fprintf(stderr, "[inkey wait read %d]\n", key);
        } else {
            /* Wait up to timeout seconds: a timer beside the keyboard in
             * the host, whichever comes first (term_wait_key).  This was
             * a loop of key-available requests with no clock behind it. */
            key = term_wait_key((int)(timeout * 1000.0)) ? read_dbase_key() : 0;
        }

        term_set_raw(0);
        if (key != 0 && key != KEY_NONE) {
            last_key = key;   /* function keys too: their codes are negative (F10 is -9) */
            if (screen_check_key_handler(key)) return 0;
        }
        return (key == KEY_NONE) ? 0 : key;
    }
#endif
    /* Fallback: use getchar (blocking only, no timeout) */
    if (timeout == 0.0) return 0;  /* can't poll without terminal */
    fflush(stdout);
    {
        int c = getchar();
        if (c < 0) return 0;
        if (c == '\n') c = 13;
        last_key = c;
        if (screen_check_key_handler(c)) return 0;
        return c;
    }
}

int screen_lastkey(void) {
    return last_key;
}

int screen_readkey(void) {
    return read_exit_key;
}

void screen_set_lastkey(int key) {
    last_key = key;
}

/* ---- Key handler support (SET KEY / ON KEY) ---- */

void screen_set_key_handler(int keycode, const char *procname) {
    int i;
    if (!procname || !procname[0]) {
        /* Remove handler for this keycode */
        for (i = 0; i < key_handler_count; i++) {
            if (key_handlers[i].keycode == keycode) {
                key_handlers[i] = key_handlers[--key_handler_count];
                return;
            }
        }
        return;
    }
    /* Replace existing or add new */
    for (i = 0; i < key_handler_count; i++) {
        if (key_handlers[i].keycode == keycode) {
            str_copy(key_handlers[i].procedure, procname, sizeof(key_handlers[i].procedure));
            return;
        }
    }
    if (key_handler_count < MAX_KEY_HANDLERS) {
        key_handlers[key_handler_count].keycode = keycode;
        str_copy(key_handlers[key_handler_count].procedure, procname,
                 sizeof(key_handlers[key_handler_count].procedure));
        key_handlers[key_handler_count].block = val_nil();
        key_handler_count++;
    }
}

value_t screen_set_key_block(int keycode, const value_t *block) {
    int i;
    value_t prev = val_nil();
    for (i = 0; i < key_handler_count; i++) {
        if (key_handlers[i].keycode == keycode) {
            prev = key_handlers[i].block;
            break;
        }
    }
    screen_set_key_handler(keycode, NULL);   /* out, then in again with the block */
    if (block && key_handler_count < MAX_KEY_HANDLERS) {
        key_handlers[key_handler_count].keycode = keycode;
        key_handlers[key_handler_count].procedure[0] = '\0';
        key_handlers[key_handler_count].block = *block;
        key_handler_count++;
    }
    return prev;
}

void screen_clear_key_handlers(void) {
    key_handler_count = 0;
}

int screen_check_key_handler(int keycode) {
    int i;
    if (in_key_handler) return 0;
    for (i = 0; i < key_handler_count; i++) {
        if (key_handlers[i].keycode == keycode) {
            in_key_handler = 1;
            if (key_handlers[i].block.type == VAL_BLOCK) {   /* SETKEY(): (procname, line, readvar) */
                expr_ctx_t *ctx = cmd_get_expr_ctx();
                value_t a[3], r;
                func_call(ctx, "PROCNAME", NULL, 0, &a[0]);
                func_call(ctx, "PROCLINE", NULL, 0, &a[1]);
                a[2] = val_str(read_var);
                if (ast_eval_block(ctx, &key_handlers[i].block, a, 3, &r) != 0 && ctx->error)
                    prog_error(expr_error_code(ctx->error), ctx->error);
            } else {
                prog_do(key_handlers[i].procedure);
            }
            in_key_handler = 0;
            return 1;
        }
    }
    return 0;
}

int screen_check_escape(void) {
#if HAS_TERM
    if (scr.term_available) {
        if (term_kbhit()) {
            int ch = term_getkey();
            if (ch == 27) return 1;
            /* Not Esc — discard (can't push back) */
        }
    }
#endif
    return 0;
}
