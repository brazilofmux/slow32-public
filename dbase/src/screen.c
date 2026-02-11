#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "screen.h"
#include "command.h"
#include "program.h"
#include "util.h"
#include "memvar.h"
#include "expr.h"
#include "ast.h"

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

/* Key handler state (SET KEY / ON KEY) */
#define MAX_KEY_HANDLERS 32
static struct {
    int keycode;
    char procedure[64];
} key_handlers[MAX_KEY_HANDLERS];
static int key_handler_count;
static int in_key_handler;  /* re-entrancy guard */

/* Assume 80-column screen for cursor tracking */
#define SCREEN_COLS 80

/* dBase III key codes for special keys */
#define DBASE_KEY_UP     5
#define DBASE_KEY_DOWN  24
#define DBASE_KEY_LEFT  19
#define DBASE_KEY_RIGHT  4
#define DBASE_KEY_PGUP  18
#define DBASE_KEY_PGDN   3
#define DBASE_KEY_HOME   1
#define DBASE_KEY_END    6
#define DBASE_KEY_INS   22
#define DBASE_KEY_DEL    7

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

int screen_term_available(void) { return scr.term_available; }

/* ---- Apply PICTURE mask to a string ---- */
static void apply_picture(char *out, const char *value, const char *picture, int outsize) {
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
        fflush(stdout);  /* drain stdio before moving cursor via MMIO */
        term_gotoxy(row, col);
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

/* ---- @SAY ---- */
void screen_say(int row, int col, const char *expr_str, const char *picture) {
    expr_ctx_t *ctx = cmd_get_expr_ctx();
    value_t val;
    char buf[256];
    char formatted[256];

    if (expr_eval_str(ctx, expr_str, &val) != 0) {
        if (ctx->error) printf("Error: %s\n", ctx->error);
        return;
    }

    val_to_string(&val, buf, sizeof(buf));

    if (picture && picture[0]) {
        apply_picture(formatted, buf, picture, sizeof(formatted));
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
        printf("Too many GETs.\n");
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
    if (scr.gets[scr.ngets].valid_expr[0]) {
        const char *ast_err;
        scr.gets[scr.ngets].valid_ast = ast_compile(scr.gets[scr.ngets].valid_expr, store, &ast_err);
    }
    if (scr.gets[scr.ngets].when_expr[0]) {
        const char *ast_err;
        scr.gets[scr.ngets].when_ast = ast_compile(scr.gets[scr.ngets].when_expr, store, &ast_err);
    }

    scr.gets[scr.ngets].is_field = 0;
    scr.gets[scr.ngets].field_index = -1;

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
    printf("%s", scr.gets[scr.ngets].initial);
    screen_update_pos(row, col, (int)strlen(scr.gets[scr.ngets].initial));

    scr.ngets++;
}

#if HAS_TERM
int read_dbase_key(void);
#endif

/* ---- READ ---- */
void screen_read(void) {
    memvar_store_t *store = cmd_get_memvar_store();
    int i;

#if HAS_TERM
    if (scr.term_available) {
        /* Full-screen READ with terminal support */
        int escaped = 0;
        fflush(stdout);  /* flush all pending SAY/GET output before raw mode */
        term_set_raw(1);

        for (i = 0; i < scr.ngets && !escaped; i++) {
            char buf[256];
            int pos, len, key;

            /* WHEN — skip field if false */
            if (scr.gets[i].when_ast) {
                value_t res;
                if (ast_eval(scr.gets[i].when_ast, cmd_get_expr_ctx(), &res) != 0 ||
                    (res.type == VAL_LOGIC && !res.logic))
                    continue;
            } else if (scr.gets[i].when_expr[0]) {
                value_t res;
                if (expr_eval_str(cmd_get_expr_ctx(), scr.gets[i].when_expr, &res) != 0 ||
                    (res.type == VAL_LOGIC && !res.logic))
                    continue;
            }

            for (;;) {
            int fwidth = scr.gets[i].width;
            /* PICTURE length determines field width if available */
            if (scr.gets[i].picture[0]) {
                int plen = strlen(scr.gets[i].picture);
                if (plen > 0) fwidth = plen;
            }

            str_copy(buf, scr.gets[i].initial, sizeof(buf));
            /* Pad to field width */
            len = strlen(buf);
            while (len < fwidth && len < 255) { buf[len++] = ' '; }
            buf[len] = '\0';
            pos = 0;  /* cursor at start of field (dBase convention) */

            fflush(stdout);
            term_gotoxy(scr.gets[i].row, scr.gets[i].col);
            printf("%-*s", fwidth, buf);
            fflush(stdout);
            term_gotoxy(scr.gets[i].row, scr.gets[i].col);

            for (;;) {
                key = read_dbase_key();
                if (key > 0 && screen_check_key_handler(key)) continue;
                if (key == '\r' || key == '\n' || key == -1) {
                    last_key = 13;
                    break;
                }
                if (key == 27) { /* Escape - cancel READ */
                    last_key = 27;
                    escaped = 1;
                    str_copy(buf, scr.gets[i].initial, sizeof(buf));
                    break;
                }
                if (key == 8 || key == 127) { /* Backspace */
                    if (pos > 0) {
                        pos--;
                        buf[pos] = ' ';
                        term_gotoxy(scr.gets[i].row, scr.gets[i].col + pos);
                        putchar(' ');
                        fflush(stdout);
                        term_gotoxy(scr.gets[i].row, scr.gets[i].col + pos);
                    }
                    continue;
                }
                if (key >= 32 && key < 127 && pos < fwidth) {
                    /* Apply PICTURE mask if any */
                    if (scr.gets[i].picture[0] && pos < (int)strlen(scr.gets[i].picture)) {
                        char pc = scr.gets[i].picture[pos];
                        if (pc == '9' && (key < '0' || key > '9')) continue;
                        if (pc == 'A' && !((key >= 'A' && key <= 'Z') || (key >= 'a' && key <= 'z'))) continue;
                        if (pc == '!' && key >= 'a' && key <= 'z') key -= 32;
                    }
                    /* Overwrite mode */
                    buf[pos] = (char)key;
                    putchar((char)key);
                    fflush(stdout);
                    pos++;
                    if (pos >= fwidth) {
                        if (!cmd_get_confirm()) {
                            last_key = 13;
                            break;  /* auto-advance: field full, CONFIRM OFF */
                        }
                        /* Field full — position at end */
                        term_gotoxy(scr.gets[i].row, scr.gets[i].col + fwidth - 1);
                    }
                }
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
                        fflush(stdout);
                        term_gotoxy(24, 1);
                        printf("Range: %g to %g", lo, hi);
                        fflush(stdout);
                        term_getkey();  /* wait for acknowledgment */
                        term_gotoxy(24, 1);
                        printf("%-40s", "");
                        fflush(stdout);
                        continue;  /* reject, keep current value */
                    }
                }
                /* VALID validation — set memvar temporarily, restore on failure */
                if (scr.gets[i].valid_ast || scr.gets[i].valid_expr[0]) {
                    value_t old_val, res;
                    int had_old = (memvar_find(store, scr.gets[i].varname, &old_val) == 0);
                    int ok;
                    memvar_set(store, scr.gets[i].varname, &v);
                    if (scr.gets[i].valid_ast)
                        ok = (ast_eval(scr.gets[i].valid_ast, cmd_get_expr_ctx(), &res) == 0 &&
                              res.type == VAL_LOGIC && res.logic);
                    else
                        ok = (expr_eval_str(cmd_get_expr_ctx(), scr.gets[i].valid_expr, &res) == 0 &&
                              res.type == VAL_LOGIC && res.logic);
                    if (!ok) {
                        if (had_old) memvar_set(store, scr.gets[i].varname, &old_val);
                        fflush(stdout);
                        term_gotoxy(24, 1);
                        printf("Invalid entry.");
                        fflush(stdout);
                        term_getkey();  /* wait for acknowledgment */
                        term_gotoxy(24, 1);
                        printf("%-40s", "");
                        fflush(stdout);
                        continue;  /* re-edit field */
                    }
                } else {
                    memvar_set(store, scr.gets[i].varname, &v);
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
                if (expr_eval_str(cmd_get_expr_ctx(), scr.gets[i].when_expr, &res) != 0 ||
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
                    memvar_set(store, scr.gets[i].varname, &v);
                    if (scr.gets[i].valid_ast)
                        ok = (ast_eval(scr.gets[i].valid_ast, cmd_get_expr_ctx(), &res) == 0 &&
                              res.type == VAL_LOGIC && res.logic);
                    else
                        ok = (expr_eval_str(cmd_get_expr_ctx(), scr.gets[i].valid_expr, &res) == 0 &&
                              res.type == VAL_LOGIC && res.logic);
                    if (!ok) {
                        if (had_old) memvar_set(store, scr.gets[i].varname, &old_val);
                        printf("\nInvalid entry.\n");
                        continue;
                    }
                } else {
                    memvar_set(store, scr.gets[i].varname, &v);
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
            term_gotoxy(r, c1);
            for (i = 0; i < w; i++) putchar(' ');
        }
        term_gotoxy(r1, c1);
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
        term_gotoxy(r1, c1);
        putchar(tl);
        for (c = c1 + 1; c < c2; c++) putchar(h);
        putchar(tr);
        /* Sides */
        for (r = r1 + 1; r < r2; r++) {
            term_gotoxy(r, c1); putchar(v);
            term_gotoxy(r, c2); putchar(v);
        }
        /* Bottom */
        term_gotoxy(r2, c1);
        putchar(bl);
        for (c = c1 + 1; c < c2; c++) putchar(h);
        putchar(br);
        return;
    }
#endif
    /* Fallback: skip box drawing silently */
    (void)r1; (void)c1; (void)r2; (void)c2;
    (void)h; (void)v; (void)tl; (void)tr; (void)bl; (void)br;
}

/* ---- CLEAR ---- */
void screen_clear(void) {
#if HAS_TERM
    if (scr.term_available) {
        fflush(stdout);
        term_clear(0);
        term_gotoxy(1, 1);
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

/* ---- SET COLOR TO ---- */
void screen_set_color(const char *spec) {
    /* Parse color spec: "fg/bg" or "fg"
       Colors: W(hite), N(black), R(ed), G(reen), B(lue),
               GR(gray), BG(bright green), etc.
       Attributes: U=underline, I=inverse, +=bold, *=blink (treated as bold) */
    const char *p = skip_ws(spec);
    int fg = 7, bg = 0;
    int bold = 0, underline = 0, inverse = 0;

    /* Check for attribute-only foreground specifiers */
    if (*p == 'U' || *p == 'u') {
        underline = 1; fg = 7; p++;
    } else if (*p == 'I' || *p == 'i') {
        inverse = 1; fg = 7; p++;
    } else {
        /* Parse foreground color */
        switch (*p) {
        case 'N': case 'n': fg = 0; p++; break; /* black */
        case 'B': case 'b': fg = 4; p++; break; /* blue */
        case 'G': case 'g': fg = 2; p++; break; /* green */
        case 'R': case 'r': fg = 1; p++; break; /* red */
        case 'W': case 'w': fg = 7; p++; break; /* white */
        default: if (*p && *p != '/' && *p != '+' && *p != '*') p++; break;
        }
    }

    /* Check for '+' (bold/high intensity) or '*' (blink, treated as bold) */
    if (*p == '+') { bold = 1; p++; }
    else if (*p == '*') { bold = 1; p++; }

    /* Parse background after '/' */
    if (*p == '/') {
        p++;
        switch (*p) {
        case 'N': case 'n': bg = 0; p++; break;
        case 'B': case 'b': bg = 4; p++; break;
        case 'G': case 'g': bg = 2; p++; break;
        case 'R': case 'r': bg = 1; p++; break;
        case 'W': case 'w': bg = 7; p++; break;
        default: if (*p) p++; break;
        }
    }

    scr.fg_color = fg;
    scr.bg_color = bg;

#if HAS_TERM
    if (scr.term_available) {
        term_set_attr(0);  /* reset all attributes */
        if (bold) term_set_attr(1);
        if (underline) term_set_attr(4);
        if (inverse) term_set_attr(7);
        term_set_color(fg, bg);
    }
#endif
    (void)p;
}

/* ---- Parse @ command ---- */
void screen_at_cmd(const char *line) {
    const char *p = skip_ws(line);
    int row, col;

    /* Skip @ */
    if (*p != '@') return;
    p++;
    p = skip_ws(p);

    /* Parse row (expression) */
    {
        expr_ctx_t *ctx = cmd_get_expr_ctx();
        value_t v;
        if (expr_eval(ctx, &p, &v) != 0 || v.type != VAL_NUM) {
            printf("Syntax: @ row,col SAY|GET|CLEAR TO|TO ...\n");
            return;
        }
        row = (int)v.num;
        p = skip_ws(p);
        if (*p == ',') p++;
        p = skip_ws(p);
        if (expr_eval(ctx, &p, &v) != 0 || v.type != VAL_NUM) {
            printf("Syntax: @ row,col SAY|GET|CLEAR TO|TO ...\n");
            return;
        }
        col = (int)v.num;
        p = skip_ws(p);
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

        /* Parse variable name */
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
                }
            } else if (str_imatch(p, "RANGE")) {
                expr_ctx_t *ctx = cmd_get_expr_ctx();
                p = skip_ws(p + 5);
                if (expr_eval(ctx, &p, &range_lo) == 0) {
                    p = skip_ws(p);
                    if (*p == ',') p++;
                    p = skip_ws(p);
                    if (expr_eval(ctx, &p, &range_hi) == 0) {
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
        if (str_imatch(p, "TO")) {
            p = skip_ws(p + 2);
            r2 = atoi(p);
            while (*p >= '0' && *p <= '9') p++;
            p = skip_ws(p);
            if (*p == ',') p++;
            p = skip_ws(p);
            c2 = atoi(p);
        }
        screen_clear_region(row, col, r2, c2);

    } else if (str_imatch(p, "TO")) {
        int r2, c2;
        int dbl = 0;
        p = skip_ws(p + 2);
        r2 = atoi(p);
        while (*p >= '0' && *p <= '9') p++;
        p = skip_ws(p);
        if (*p == ',') p++;
        p = skip_ws(p);
        c2 = atoi(p);
        while (*p >= '0' && *p <= '9') p++;
        p = skip_ws(p);
        if (str_imatch(p, "DOUBLE")) dbl = 1;
        screen_box(row, col, r2, c2, dbl);

    } else {
        printf("Syntax: @ row,col SAY|GET|CLEAR TO|TO ...\n");
    }
}

int screen_get_row(void) { return scr.last_row; }
int screen_get_col(void) { return scr.last_col; }
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
    if (ch < 0) return -1;
    if (ch == 27) {
        /* Possible escape sequence — check if more bytes follow */
        if (!term_kbhit()) return 27;  /* bare Escape */
        int ch2 = term_getkey();
        if (ch2 == '[') {
            int ch3 = term_getkey();
            switch (ch3) {
            case 'A': return DBASE_KEY_UP;
            case 'B': return DBASE_KEY_DOWN;
            case 'C': return DBASE_KEY_RIGHT;
            case 'D': return DBASE_KEY_LEFT;
            case 'H': return DBASE_KEY_HOME;
            case 'F': return DBASE_KEY_END;
            case '5':  /* PgUp: ESC [ 5 ~ */
                if (term_kbhit()) term_getkey(); /* consume ~ */
                return DBASE_KEY_PGUP;
            case '6':  /* PgDn: ESC [ 6 ~ */
                if (term_kbhit()) term_getkey();
                return DBASE_KEY_PGDN;
            case '2':  /* Ins: ESC [ 2 ~ */
                if (term_kbhit()) term_getkey();
                return DBASE_KEY_INS;
            case '3':  /* Del: ESC [ 3 ~ */
                if (term_kbhit()) term_getkey();
                return DBASE_KEY_DEL;
            default: return ch3; /* unknown sequence — return last byte */
            }
        }
        return ch2; /* ESC + something else */
    }
    /* Map backspace variants */
    if (ch == 127) return 8;  /* normalize DEL to BS */
    return ch;
}
#endif

int screen_inkey(double timeout) {
#if HAS_TERM
    if (scr.term_available) {
        int key;
        fflush(stdout);
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
            /* Wait up to timeout seconds.
             * We poll in a loop since we don't have select/alarm. */
            int ticks = (int)(timeout * 100); /* ~10ms granularity */
            int i;
            key = 0;
            for (i = 0; i < ticks; i++) {
                if (term_kbhit()) {
                    key = read_dbase_key();
                    break;
                }
                /* Small busy-wait — no usleep available on SLOW-32 */
            }
        }

        term_set_raw(0);
        if (key > 0) {
            last_key = key;
            if (screen_check_key_handler(key)) return 0;
        }
        return (key < 0) ? 0 : key;
    }
#endif
    /* Fallback: use getchar (blocking only, no timeout) */
    if (timeout == 0.0) return 0;  /* can't poll without terminal */
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
        key_handler_count++;
    }
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
            prog_do(key_handlers[i].procedure);
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
