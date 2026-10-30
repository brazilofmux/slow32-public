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
#include "getsys.h"
#include "func.h"

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

/* Assume 80-column screen for cursor tracking */
#define SCREEN_COLS 80

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

void screen_save(void) {
#if HAS_TERM
    if (scr.term_available) {
        term_save_screen();
    }
#endif
}

void screen_restore(void) {
#if HAS_TERM
    if (scr.term_available) {
        term_restore_screen();
    }
#endif
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
 * write dbase makes to the terminal goes through these wrappers and
 * lands in the shadow too: a cell is its character and an attribute
 * byte (bit 7 set, bold, background, foreground).  A saved region is
 * kept here and handed to the program as a short handle string (a
 * value_t string holds 255 characters; a screen is 4,000 cells). */
#define SHADOW_ROWS 25
#define SHADOW_COLS 80
typedef struct { unsigned char ch, attr; } shadow_cell_t;
static shadow_cell_t shadow[SHADOW_ROWS][SHADOW_COLS];
static int shadow_row, shadow_col;
static int shadow_fg = 7, shadow_bg = 0, shadow_bold, shadow_inverse;
static int shadow_ready;

static unsigned char shadow_attr(void) {
    int fg = shadow_inverse ? shadow_bg : shadow_fg;
    int bg = shadow_inverse ? shadow_fg : shadow_bg;
    return (unsigned char)(0x80 | (shadow_bold ? 0x40 : 0) | ((bg & 7) << 3) | (fg & 7));
}

static void shadow_init(void) {
    int r, c;
    if (shadow_ready) return;
    shadow_ready = 1;
    for (r = 0; r < SHADOW_ROWS; r++)
        for (c = 0; c < SHADOW_COLS; c++) { shadow[r][c].ch = ' '; shadow[r][c].attr = 0x87; }
}

static void shadow_put(int ch) {
    shadow_init();
    if (shadow_row >= 0 && shadow_row < SHADOW_ROWS && shadow_col >= 0 && shadow_col < SHADOW_COLS) {
        shadow[shadow_row][shadow_col].ch = (unsigned char)ch;
        shadow[shadow_row][shadow_col].attr = shadow_attr();
    }
    if (++shadow_col >= SHADOW_COLS) {
        shadow_col = 0;
        if (shadow_row < SHADOW_ROWS - 1) shadow_row++;
    }
}

void screen_tgoto(int row, int col) {
    shadow_init();
    shadow_row = row;
    shadow_col = col;
    term_gotoxy(row, col);
}

void screen_tputs(const char *s) {
    const char *p;
    for (p = s; *p; p++) {
        if (*p == '\n') { shadow_col = 0; if (shadow_row < SHADOW_ROWS - 1) shadow_row++; }
        else if (*p == '\r') shadow_col = 0;
        else shadow_put((unsigned char)*p);
    }
    term_puts(s);
}

void screen_tputc(int ch) {
    if (ch == '\n') { shadow_col = 0; if (shadow_row < SHADOW_ROWS - 1) shadow_row++; }
    else if (ch == '\r') shadow_col = 0;
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
        for (r = 0; r < SHADOW_ROWS; r++)
            for (c = 0; c < SHADOW_COLS; c++) { shadow[r][c].ch = ' '; shadow[r][c].attr = a; }
        shadow_row = shadow_col = 0;
    } else if (shadow_row >= 0 && shadow_row < SHADOW_ROWS) {
        for (c = shadow_col > 0 ? shadow_col : 0; c < SHADOW_COLS; c++) { shadow[shadow_row][c].ch = ' '; shadow[shadow_row][c].attr = a; }
        if (mode == 2)
            for (r = shadow_row + 1; r < SHADOW_ROWS; r++)
                for (c = 0; c < SHADOW_COLS; c++) { shadow[r][c].ch = ' '; shadow[r][c].attr = a; }
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
    if (*b >= SHADOW_ROWS) *b = SHADOW_ROWS - 1;
    if (*r >= SHADOW_COLS) *r = SHADOW_COLS - 1;
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
        term_gotoxy(t + i, l);
        for (j = 0; j < cols; j++) {
            shadow_cell_t cell = s->cells[i * s->cols + j];
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
    term_set_attr(0);
    if (shadow_bold) term_set_attr(1);
    if (shadow_inverse) term_set_attr(7);
    term_set_color(shadow_fg, shadow_bg);
    shadow_row = keep_row;
    shadow_col = keep_col;
    term_gotoxy(keep_row, keep_col);
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
    /* Handle wrap past SCREEN_COLS */
    if (end_col > SCREEN_COLS + 1) {
        row += (end_col - 1) / SCREEN_COLS;
        end_col = ((end_col - 1) % SCREEN_COLS) + 1;
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

value_t screen_active_get(void) {
    if (active_get >= 0 && active_get < scr.ngets) return scr.gets[active_get].getobj;
    return val_nil();
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

    val_to_string(&val, buf, sizeof(buf));

    if (picture && picture[0]) {
        screen_apply_picture(formatted, buf, picture, sizeof(formatted));
    } else {
        str_copy(formatted, buf, sizeof(formatted));
    }

    if (cmd_get_device() == 1) {
        /* PRINT mode: advance with newlines/spaces to requested position */
        if (row < print_row) {
            screen_eject();
        }
        while (print_row < row) {
            printf("\n");
            print_row++;
            print_col = 0;
        }
        while (print_col < col) {
            putchar(' ');
            print_col++;
        }
        screen_print_text(formatted);
    } else {
        goto_pos(row, col);
#if HAS_TERM
        if (scr.term_available)
            screen_tputs(formatted);
        else
#endif
            printf("%s", formatted);
    }
    screen_update_pos(row, col, (int)strlen(formatted));
}

/* ---- @GET ---- */
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
    scr.gets[scr.ngets].getobj = getsys_note_get(row, col, varname, picture, valid_expr, when_expr);

    /* Get current value for display */
    if (memvar_find(store, varname, &val) == 0) {
        val_to_string(&val, scr.gets[scr.ngets].initial, sizeof(scr.gets[scr.ngets].initial));
        scr.gets[scr.ngets].type = val.type;
        scr.gets[scr.ngets].width = strlen(scr.gets[scr.ngets].initial);
        if (scr.gets[scr.ngets].width < 10) scr.gets[scr.ngets].width = 10;
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

    /* Display the field */
    goto_pos(row, col);
#if HAS_TERM
    if (scr.term_available)
        screen_tputs(scr.gets[scr.ngets].initial);
    else
#endif
        printf("%s", scr.gets[scr.ngets].initial);
    screen_update_pos(row, col, (int)strlen(scr.gets[scr.ngets].initial));

    scr.ngets++;
}

#if HAS_TERM
int read_dbase_key(void);
#endif

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
    goto_pos(row, col);
#if HAS_TERM
    if (scr.term_available)
        screen_tputs(g->initial);
    else
#endif
        printf("%s", g->initial);
    screen_update_pos(row, col, (int)strlen(g->initial));
    scr.ngets++;
}

/* The edited value back where it came from: the Get object\x27s block when
 * the entry carries one, else the variable by name */
static void get_store_value(get_entry_t *g, const value_t *v) {
    if (g->getobj.type == VAL_OBJECT && getsys_has_block(&g->getobj)) {
        getsys_varput(&g->getobj, v);
        return;
    }
    if (g->varname[0]) memvar_set(cmd_get_memvar_store(), g->varname, v);
}


/* ---- The GET editor's picture ---- */
typedef struct {
    int clear, upper, alpha, scroll, literal_r;
    char tmpl[64];
} get_picture_t;

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

            for (;;) {
            /* The picture: functions after @ (K clear on the first key,
             * S<n> a scrolling window, ! upper case, A letters only, R
             * literals shown but not kept), then the template.  Its
             * length is the field's width (it was the whole string's, so
             * "@KS30" made a field four wide). */
            get_picture_t pic;
            int fwidth, dwidth, win = 0, first_key = 1;
            parse_get_picture(scr.gets[i].picture, &pic);
            fwidth = scr.gets[i].width;
            if (pic.tmpl[0]) fwidth = (int)strlen(pic.tmpl);
            if (fwidth > 255) fwidth = 255;
            if (fwidth < 1) fwidth = 1;
            dwidth = (pic.scroll > 0 && pic.scroll < fwidth) ? pic.scroll : fwidth;
            nav = 0;

            str_copy(buf, scr.gets[i].initial, sizeof(buf));
            /* Pad to field width */
            len = strlen(buf);
            while (len < fwidth && len < 255) { buf[len++] = ' '; }
            buf[len] = '\0';
            pos = 0;  /* cursor at start of field (dBase convention) */

            get_paint(&scr.gets[i], buf, fwidth, dwidth, win, pos);

            for (;;) {
                key = screen_getkey();
                if (key != KEY_NONE && key != 0 && screen_check_key_handler(key)) continue;
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
                } else if (key == K_INS) get_insert_mode = !get_insert_mode;
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
                if (scr.gets[i].type == VAL_NUM) {
                    v = val_num(atof(buf));
                } else {
                    v = val_str(buf);
                }
                /* RANGE validation (match line-mode behavior) */
                if (scr.gets[i].has_range && v.type == VAL_NUM) {
                    double n = v.num;
                    double lo = scr.gets[i].range_lo.num;
                    double hi = scr.gets[i].range_hi.num;
                    if (n < lo || n > hi) {
                        char rmsg[80];
                        snprintf(rmsg, sizeof(rmsg), "Range: %g to %g", lo, hi);
                        screen_tgoto(24, 1);
                        screen_tputs(rmsg);
                        term_getkey();  /* wait for acknowledgment */
                        screen_tgoto(24, 1);
                        {
                            int ri;
                            for (ri = 0; ri < 40; ri++) screen_tputc(' ');
                        }
                        continue;  /* reject, keep current value */
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
                        screen_tgoto(24, 1);
                        screen_tputs("Invalid entry.");
                        term_getkey();  /* wait for acknowledgment */
                        screen_tgoto(24, 1);
                        {
                            int ri;
                            for (ri = 0; ri < 40; ri++) screen_tputc(' ');
                        }
                        continue;  /* re-edit field */
                    }
                } else {
                    get_store_value(&scr.gets[i], &v);
                }
                {   /* UPDATED() */
                    char nv[256];
                    val_to_string(&v, nv, sizeof(nv));
                    if (strcmp(nv, scr.gets[i].initial) != 0) read_updated = 1;
                }
                /* Write back to database field if applicable */
                if (scr.gets[i].is_field) {
                    expr_ctx_t *fctx = cmd_get_expr_ctx();
                    dbf_t *fdb = fctx->db;
                    if (fdb && dbf_is_open(fdb)) {
                        char fraw[256];
                        val_to_string(&v, fraw, sizeof(fraw));
                        dbf_set_field_raw(fdb, scr.gets[i].field_index, fraw);
                        dbf_flush_record(fdb);
                    }
                }
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
                
                if (line[0] == '\0') {
                    /* Keep current value */
                    if (scr.gets[i].type == VAL_NUM) v = val_num(atof(scr.gets[i].initial));
                    else v = val_str(scr.gets[i].initial);
                } else {
                    trim_right(line);
                    if (scr.gets[i].type == VAL_NUM) v = val_num(atof(line));
                    else v = val_str(line);
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
                /* Write back to database field if applicable */
                if (scr.gets[i].is_field) {
                    expr_ctx_t *fctx = cmd_get_expr_ctx();
                    dbf_t *fdb = fctx->db;
                    if (fdb && dbf_is_open(fdb)) {
                        char fraw[256];
                        val_to_string(&v, fraw, sizeof(fraw));
                        dbf_set_field_raw(fdb, scr.gets[i].field_index, fraw);
                        dbf_flush_record(fdb);
                    }
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

/* ---- @TO (box drawing) ---- */
void screen_box(int r1, int c1, int r2, int c2, int dbl) {
    /* Box characters: single vs double */
    char h, v, tl, tr, bl, br;

    if (dbl) {
        h = '='; v = '|'; tl = '+'; tr = '+'; bl = '+'; br = '+';
    } else {
        h = '-'; v = '|'; tl = '+'; tr = '+'; bl = '+'; br = '+';
    }

#if HAS_TERM
    if (scr.term_available) {
        int r, c;
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
    /* Fallback: print 24 newlines */
    {
        int i;
        for (i = 0; i < 24; i++) printf("\n");
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
                char blank[81];
                int len;
                screen_tgoto(msg_row, 0);
                screen_tattr(0);
                /* Clear the message row */
                len = 80;
                if (len > (int)sizeof(blank) - 1) len = (int)sizeof(blank) - 1;
                memset(blank, ' ', len);
                blank[len] = '\0';
                screen_tputs(blank);
                /* Display current message centered */
                if (scr.prompts[cur].message[0]) {
                    int mlen = (int)strlen(scr.prompts[cur].message);
                    int mcol = (80 - mlen) / 2;
                    if (mcol < 0) mcol = 0;
                    screen_tgoto(msg_row, mcol);
                    screen_tputs(scr.prompts[cur].message);
                }
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
            char blank[81];
            int len = 80;
            if (len > (int)sizeof(blank) - 1) len = (int)sizeof(blank) - 1;
            memset(blank, ' ', len);
            blank[len] = '\0';
            screen_tgoto(msg_row, 0);
            screen_tputs(blank);
        }

        term_set_raw(0);
    } else
#endif
    {
        /* Non-terminal fallback: just pick first option */
        printf("Menu: ");
        for (i = 0; i < n; i++) {
            printf("%d. %s  ", i + 1, scr.prompts[i].text);
        }
        printf("\n");
        cur = 0;
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

        p = skip_ws(p + 3);

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

    } else if (str_imatch(p, "GET")) {
        char varname[MEMVAR_NAMELEN];
        char picture[64] = "";
        char valid_expr[256] = "";
        char when_expr[256] = "";
        int i = 0;
        int has_range = 0;
        value_t range_lo, range_hi;

        p = skip_ws(p + 3);

        /* Parse variable name; M-> and MEMVAR-> say what it is anyway
         * (DBU's @ r, c GET M->wk_str) */
        if (strncasecmp(p, "M->", 3) == 0) p += 3;
        else if (strncasecmp(p, "MEMVAR->", 8) == 0) p += 8;
        while (is_ident_char(*p) && i < MEMVAR_NAMELEN - 1)
            varname[i++] = *p++;
        varname[i] = '\0';

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
                p = skip_ws(p + 5);
                if (ast_eval_adv(ctx, &p, &range_lo) == 0) {
                    p = skip_ws(p);
                    if (*p == ',') p++;
                    p = skip_ws(p);
                    if (ast_eval_adv(ctx, &p, &range_hi) == 0) {
                        has_range = 1;
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

        screen_get(row, col, varname, picture,
                   has_range ? &range_lo : NULL,
                   has_range ? &range_hi : NULL,
                   valid_expr, when_expr);

    } else if (str_imatch(p, "CLEAR")) {
        int r2 = 24, c2 = 79;
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
        char text[80] = "";
        char message[160] = "";
        int i = 0;
        p = skip_ws(p + 6);

        /* Parse prompt text (quoted string) */
        if (*p == '"' || *p == '\'') {
            char q = *p++;
            while (*p && *p != q && i < 79)
                text[i++] = *p++;
            text[i] = '\0';
            if (*p) p++;
        } else {
            printf("Syntax: @ row,col PROMPT \"text\" [MESSAGE \"help\"]\n");
            return;
        }

        /* Parse optional MESSAGE clause */
        p = skip_ws(p);
        if (str_imatch(p, "MESSAGE")) {
            p = skip_ws(p + 7);
            if (*p == '"' || *p == '\'') {
                char q = *p++;
                i = 0;
                while (*p && *p != q && i < 159)
                    message[i++] = *p++;
                message[i] = '\0';
                if (*p) p++;
            }
        }

        screen_prompt(row, col, text, message);

    } else if (*p == '\0') {   /* @ row, col alone: the line cleared from there, the cursor left there */
        screen_clear_region(row, col, row, SCREEN_COLS - 1);
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

void screen_eject(void) {
    printf("\n--- PAGE ---\n");
    print_row = 0;
    print_col = 0;
}

void screen_print_newline(void) {
    printf("\n");
    print_row++;
    print_col = 0;
}

void screen_print_text(const char *s) {
    if (!s) return;
    while (*s) {
        if (*s == '\n') {
            printf("\n");
            print_row++;
            print_col = 0;
            s++;
            continue;
        }
        putchar(*s++);
        print_col++;
    }
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

int screen_inkey(double timeout) {
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
        } else if (timeout < 0.0) {
            /* Wait forever (INKEY(0) in dBase = wait forever) */
            key = read_dbase_key();
        } else {
            /* Wait up to timeout seconds: a timer beside the keyboard in
             * the host, whichever comes first (term_wait_key).  This was
             * a loop of key-available requests with no clock behind it. */
            key = term_wait_key((int)(timeout * 1000.0)) ? read_dbase_key() : 0;
        }

        term_set_raw(0);
        if (key > 0) {
            last_key = key;
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
