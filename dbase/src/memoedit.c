/* MEMOEDIT() -- the guide's text editor in a window (CA-Clipper 5.2).
 *
 * The text is held as lines, each ending hard (CR LF in the text) or soft
 * (CHR(141) LF, put there by word wrap and taken out again when a
 * paragraph is reformed).  The window shows the lines from a top line and
 * a left column, and follows the cursor both ways.  The user function is
 * called with ME_INIT until it answers 0, once each time no key is
 * waiting (ME_IDLE), and for each configurable or unknown key (ME_UNKEY,
 * or ME_UNKEYX once the text has changed); its answer is a key action
 * (1-31), ME_IGNORE, ME_DATA, a toggle, or ME_DEFAULT for the key's own
 * action.  Ctrl-W returns the text, Esc the string as it was given.
 *
 * Where CA-Clipper's key codes coincide, the code decides, as it did
 * there: Ctrl-End is 23, Ctrl-W's code, and saves; Ctrl-Right is 2,
 * Ctrl-B's, and moves a word right (a user function asks for a reform by
 * answering 2, as the guide has it); Home is 1 and End 6 (Ctrl-A, Ctrl-F). */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "memoedit.h"
#include "screen.h"
#include "keys.h"
#include "func.h"
#include "command.h"
#include "set.h"
#include "gc.h"
#include "getsys.h"

#define ME_IDLE          0
#define ME_UNKEY         1
#define ME_UNKEYX        2
#define ME_INIT          3
#define ME_DEFAULT       0
#define ME_IGNORE       32
#define ME_DATA         33
#define ME_TOGGLEWRAP   34
#define ME_TOGGLESCROLL 35
#define ME_WORDRIGHT   100
#define ME_BOTTOMRIGHT 101

#define SOFT_CR ((char)0x8D)

typedef struct {
    char *s;
    int len, cap;
    int soft;           /* ends with a soft return (word wrap) */
} me_line_t;

typedef struct {
    expr_ctx_t *ctx;
    me_line_t *lines;
    int n, cap;
    int top, left, bottom, right, rows, cols;
    int edit, wrap, scroll;
    int width, tabsize;
    int row, col;       /* the cursor in the text: line 0.., column 0.. */
    int topline, leftcol;
    int changed;
    const char *udf;
} me_t;

/* ---- the lines ---- */

static void line_reserve(me_line_t *l, int need) {
    if (need + 1 <= l->cap) return;
    l->cap = (need + 1) * 2;
    l->s = realloc(l->s, (size_t)l->cap);
}

static void line_set(me_line_t *l, const char *s, int len) {
    line_reserve(l, len);
    memcpy(l->s, s, (size_t)len);
    l->len = len;
    l->s[len] = '\0';
}

static void lines_insert(me_t *m, int at, const char *s, int len, int soft) {
    me_line_t *l;
    if (m->n == m->cap) {
        m->cap = m->cap ? m->cap * 2 : 64;
        m->lines = realloc(m->lines, (size_t)m->cap * sizeof(me_line_t));
    }
    memmove(m->lines + at + 1, m->lines + at, (size_t)(m->n - at) * sizeof(me_line_t));
    m->n++;
    l = &m->lines[at];
    memset(l, 0, sizeof(*l));
    line_set(l, s, len);
    l->soft = soft;
}

static void lines_delete(me_t *m, int at) {
    free(m->lines[at].s);
    memmove(m->lines + at, m->lines + at + 1, (size_t)(m->n - at - 1) * sizeof(me_line_t));
    m->n--;
    if (m->n == 0) lines_insert(m, 0, "", 0, 0);
}

static void load_text(me_t *m, const char *t, int len) {
    int start = 0, i;
    for (i = 0; i < len; i++) {
        if (t[i] == '\n') {
            int end = i, soft = 0;
            if (end > start && t[end - 1] == '\r') end--;
            else if (end > start && t[end - 1] == SOFT_CR) { end--; soft = 1; }
            lines_insert(m, m->n, t + start, end - start, soft);
            start = i + 1;
        }
    }
    lines_insert(m, m->n, t + start, len - start, 0);
}

static value_t text_value(me_t *m) {
    int total = 0, i, k = 0;
    char *buf;
    value_t v;
    for (i = 0; i < m->n; i++) total += m->lines[i].len + 2;
    buf = malloc((size_t)total + 1);
    if (!buf) return val_str("");
    for (i = 0; i < m->n; i++) {
        memcpy(buf + k, m->lines[i].s, (size_t)m->lines[i].len);
        k += m->lines[i].len;
        if (i < m->n - 1) {
            buf[k++] = m->lines[i].soft ? SOFT_CR : '\r';
            buf[k++] = '\n';
        }
    }
    v = val_strn(buf, k);
    free(buf);
    return v;
}

/* ---- word wrap ---- */

/* Break line i at the width: at the last blank before it (the blanks stay
 * on the first line -- CA-Clipper's soft return never changes the text),
 * else at the width; the rest goes onto a soft line of its own.  Returns
 * where the break fell, -1 when the line fits. */
static int wrap_line(me_t *m, int i) {
    me_line_t *l = &m->lines[i];
    int brk, cut;
    if (l->len <= m->width) return -1;
    brk = m->width;
    while (brk > 0 && l->s[brk - 1] != ' ') brk--;
    cut = brk > 0 ? brk : m->width;
    {
        int was_soft = l->soft;
        int rest = l->len - cut;
        char *tail = malloc((size_t)rest + 1);
        memcpy(tail, l->s + cut, (size_t)rest);
        l->len = cut;
        l->s[cut] = '\0';
        l->soft = 1;
        lines_insert(m, i + 1, tail, rest, was_soft);
        free(tail);
    }
    return cut;
}

/* Reform the paragraph from line i: its soft lines joined, then wrapped
 * again at the width */
static void reform_paragraph(me_t *m, int i) {
    while (m->lines[i].soft && i + 1 < m->n) {
        me_line_t *a = &m->lines[i], *b = &m->lines[i + 1];
        line_reserve(a, a->len + b->len);
        memcpy(a->s + a->len, b->s, (size_t)b->len + 1);
        a->len += b->len;
        a->soft = b->soft;
        lines_delete(m, i + 1);
    }
    while (wrap_line(m, i) >= 0) i++;
}

static void reform_all(me_t *m) {
    int i;
    for (i = 0; i < m->n; i++) {
        int start = i;
        reform_paragraph(m, start);
        while (i < m->n - 1 && m->lines[i].soft) i++;
    }
}

/* ---- the screen ---- */

static void me_paint(me_t *m) {
    int r;
    char line[1024];
    for (r = 0; r < m->rows; r++) {
        int li = m->topline + r, k;
        for (k = 0; k < m->cols && k < (int)sizeof(line) - 1; k++) {
            int c = m->leftcol + k;
            char ch = ' ';
            if (li < m->n && c < m->lines[li].len) {
                ch = m->lines[li].s[c];
                if (ch == '\t' || (unsigned char)ch < 32) ch = ' ';
            }
            line[k] = ch;
        }
        line[k] = '\0';
        screen_out_at(m->top + r, m->left, line);
    }
}

/* the window follows the cursor; 1 when it moved */
static int me_follow(me_t *m) {
    int moved = 0;
    if (m->row < m->topline) { m->topline = m->row; moved = 1; }
    if (m->row >= m->topline + m->rows) { m->topline = m->row - m->rows + 1; moved = 1; }
    if (m->col < m->leftcol) { m->leftcol = m->col; moved = 1; }
    if (m->col >= m->leftcol + m->cols) { m->leftcol = m->col - m->cols + 1; moved = 1; }
    return moved;
}

static void me_park(me_t *m) {
    screen_setpos(m->top + m->row - m->topline, m->left + m->col - m->leftcol);
}

/* ---- the user function ---- */

static int me_call(me_t *m, int mode) {
    value_t a[3], r;
    a[0] = val_num(mode);
    a[1] = val_num(m->row + 1);
    a[2] = val_num(m->col);
    me_park(m);
    if (func_call(m->ctx, m->udf, a, 3, &r) != 0) { m->ctx->error = NULL; return 0; }
    return r.type == VAL_NUM ? (int)r.num : 0;
}

static int me_insert_mode(me_t *m) {
    value_t r;
    if (func_call(m->ctx, "READINSERT", NULL, 0, &r) != 0) { m->ctx->error = NULL; return 0; }
    return r.type == VAL_LOGIC && r.logic;
}

static void me_toggle_insert(me_t *m) {
    value_t a = val_logic(!me_insert_mode(m)), r;
    if (func_call(m->ctx, "READINSERT", &a, 1, &r) != 0) m->ctx->error = NULL;
    {
        const set_options_t *o = cmd_get_set_opts();
        if (o && o->scoreboard) screen_out_at(0, 60, me_insert_mode(m) ? "<insert>" : "        ");
    }
}

/* ---- the editing actions ---- */

static me_line_t *cur(me_t *m) { return &m->lines[m->row]; }

static void fix_col(me_t *m) {
    if (m->col < 0) m->col = 0;
}

static void type_char(me_t *m, char ch) {
    me_line_t *l = cur(m);
    if (m->col > l->len) {   /* past the end: blanks up to the cursor */
        line_reserve(l, m->col);
        memset(l->s + l->len, ' ', (size_t)(m->col - l->len));
        l->len = m->col;
        l->s[l->len] = '\0';
    }
    if (me_insert_mode(m) || m->col >= l->len) {
        line_reserve(l, l->len + 1);
        memmove(l->s + m->col + 1, l->s + m->col, (size_t)(l->len - m->col) + 1);
        l->len++;
    }
    l->s[m->col] = ch;
    m->col++;
    m->changed = 1;
    if (m->wrap && l->len > m->width) {
        int brk = wrap_line(m, m->row);
        if (brk >= 0 && m->col > brk) { m->row++; m->col -= brk; }
    }
}

static void do_return(me_t *m) {
    me_line_t *l = cur(m);
    if (m->edit && me_insert_mode(m)) {
        int at = m->col < l->len ? m->col : l->len;
        lines_insert(m, m->row + 1, l->s + at, l->len - at, l->soft);
        l = cur(m);
        l->len = at;
        l->s[at] = '\0';
        l->soft = 0;
        m->changed = 1;
    } else if (m->row == m->n - 1) {
        if (!m->edit) return;
        lines_insert(m, m->n, "", 0, 0);
        m->changed = 1;
    }
    m->row++;
    m->col = 0;
}

static void join_next(me_t *m, int i) {
    me_line_t *a = &m->lines[i], *b;
    if (i + 1 >= m->n) return;
    b = &m->lines[i + 1];
    line_reserve(a, a->len + b->len);
    memcpy(a->s + a->len, b->s, (size_t)b->len + 1);
    a->len += b->len;
    a->soft = b->soft;
    lines_delete(m, i + 1);
}

static void do_delete(me_t *m) {
    me_line_t *l = cur(m);
    if (m->col < l->len) {
        memmove(l->s + m->col, l->s + m->col + 1, (size_t)(l->len - m->col));
        l->len--;
    } else {
        if (m->col > l->len) { line_reserve(l, m->col); memset(l->s + l->len, ' ', (size_t)(m->col - l->len)); l->len = m->col; l->s[l->len] = '\0'; }
        join_next(m, m->row);
    }
    m->changed = 1;
}

static void do_backspace(me_t *m) {
    if (m->col > 0) {
        m->col--;
        if (m->col < cur(m)->len) do_delete(m);
    } else if (m->row > 0) {
        m->row--;
        m->col = m->lines[m->row].len;
        join_next(m, m->row);
        m->changed = 1;
    }
}

static int is_word(char c) { return c != ' ' && c != '\t'; }

static void word_left(me_t *m) {
    me_line_t *l = cur(m);
    int c = m->col < l->len ? m->col : l->len;
    if (c == 0) {
        if (m->row > 0) { m->row--; m->col = m->lines[m->row].len; }
        return;
    }
    c--;
    while (c > 0 && !is_word(l->s[c])) c--;
    while (c > 0 && is_word(l->s[c - 1])) c--;
    m->col = c;
}

static void word_right(me_t *m) {
    me_line_t *l = cur(m);
    int c = m->col;
    if (c >= l->len) {
        if (m->row < m->n - 1) { m->row++; m->col = 0; l = cur(m); c = 0; while (c < l->len && !is_word(l->s[c])) c++; m->col = c; }
        return;
    }
    while (c < l->len && is_word(l->s[c])) c++;
    while (c < l->len && !is_word(l->s[c])) c++;
    m->col = c;
}

static void delete_word_right(me_t *m) {
    me_line_t *l = cur(m);
    int c = m->col, e;
    if (c >= l->len) { do_delete(m); return; }
    e = c;
    while (e < l->len && is_word(l->s[e])) e++;
    while (e < l->len && !is_word(l->s[e])) e++;
    memmove(l->s + c, l->s + e, (size_t)(l->len - e) + 1);
    l->len -= e - c;
    m->changed = 1;
}

static void delete_line(me_t *m) {
    lines_delete(m, m->row);
    if (m->row >= m->n) m->row = m->n - 1;
    m->col = 0;
    m->changed = 1;
}

static void tab_key(me_t *m) {
    int stop = (m->col / m->tabsize + 1) * m->tabsize;
    if (!m->edit) { m->col = stop; return; }
    if (me_insert_mode(m)) {
        while (m->col < stop) type_char(m, ' ');
    } else {
        m->col = stop;
    }
}

/* a configurable key's outcome: the edit goes on, or it is done */
enum { ACT_NONE, ACT_DONE };

static int nav_key(me_t *m, int key) {
    switch (key) {
    case K_UP:
        if (m->row > 0) m->row--;
        return 1;
    case K_DOWN:
        if (m->row < m->n - 1) m->row++;
        return 1;
    case K_LEFT:
        if (m->col > 0) m->col--;
        else if (m->row > 0) { m->row--; m->col = m->lines[m->row].len; }
        return 1;
    case K_RIGHT:
        m->col++;
        if (!m->scroll && m->wrap && m->col > m->width && m->row < m->n - 1) { m->row++; m->col = 0; }
        return 1;
    case K_CTRL_LEFT: word_left(m); return 1;
    case K_CTRL_RIGHT: word_right(m); return 1;
    case K_HOME: m->col = 0; return 1;
    case K_END: m->col = cur(m)->len; return 1;
    case K_CTRL_HOME: m->row = m->topline; m->col = 0; return 1;
    case K_PGUP:
        m->row -= m->rows;
        m->topline -= m->rows;
        if (m->row < 0) m->row = 0;
        if (m->topline < 0) m->topline = 0;
        return 1;
    case K_PGDN: {
        /* a window on: the cursor as far, the window kept full where the
         * text allows (a short text does not scroll off the window) */
        int last_top = m->n - m->rows > 0 ? m->n - m->rows : 0;
        m->row += m->rows;
        m->topline += m->rows;
        if (m->row > m->n - 1) m->row = m->n - 1;
        if (m->topline > last_top) m->topline = last_top;
        return 1;
    }
    case K_CTRL_PGUP: m->row = 0; m->col = 0; return 1;
    case K_CTRL_PGDN: m->row = m->n - 1; m->col = cur(m)->len; return 1;
    default: return 0;
    }
}

/* the configurable keys' own actions; returns ACT_DONE with *save set when
 * the edit ends */
static int config_key(me_t *m, int key, int *save) {
    switch (key) {
    case K_CTRL_Y: if (m->edit) delete_line(m); return ACT_NONE;
    case K_CTRL_T: if (m->edit) delete_word_right(m); return ACT_NONE;
    case K_INS: me_toggle_insert(m); return ACT_NONE;
    case K_CTRL_W: *save = 1; return ACT_DONE;
    case K_ESC: {
        const set_options_t *o = cmd_get_set_opts();
        if (m->changed && o && o->scoreboard) {
            /* the guide's question, on the scoreboard */
            int k;
            screen_out_at(0, 60, "Abort Edit? (Y/N)");
            me_park(m);
            k = screen_inkey(-1.0);
            screen_out_at(0, 60, "                 ");
            if (k != 'Y' && k != 'y') return ACT_NONE;
        }
        *save = 0;
        return ACT_DONE;
    }
    default: return ACT_NONE;
    }
}

static int is_config_key(int key) {
    return key == K_CTRL_Y || key == K_CTRL_T || key == K_INS || key == K_CTRL_W || key == K_ESC;
}

static int is_nav_key(int key) {
    switch (key) {
    case K_UP: case K_DOWN: case K_LEFT: case K_RIGHT: case K_CTRL_LEFT: case K_CTRL_RIGHT:
    case K_HOME: case K_END: case K_CTRL_HOME: case K_PGUP: case K_PGDN: case K_CTRL_PGUP: case K_CTRL_PGDN:
        return 1;
    default: return 0;
    }
}

/* a key MEMOEDIT edits with itself (nonconfigurable) */
static int edit_key(me_t *m, int key) {
    if (is_nav_key(key)) { nav_key(m, key); return 1; }
    if (key == K_ENTER) { do_return(m); return 1; }
    if (key == K_TAB) { tab_key(m); return 1; }
    if (key == K_DEL) { if (m->edit) do_delete(m); return 1; }
    if (key == K_BS) { if (m->edit) do_backspace(m); return 1; }
    if (key >= 32 && key <= 255) { if (m->edit) type_char(m, (char)key); return 1; }
    return 0;
}

/* A user function's answer to a key: the action it asks for, 1 when the
 * edit ends */
static int me_answer(me_t *m, int ans, int key, int *save) {
    if (ans == ME_DEFAULT) {
        if (is_config_key(key)) return config_key(m, key, save) == ACT_DONE;
        return 0;   /* an unknown key's default: nothing */
    }
    if (ans == 2) { reform_paragraph(m, m->row); return 0; }   /* Ctrl-B, asked for by number */
    if (ans >= 1 && ans <= 31) {
        if (is_config_key(ans)) return config_key(m, ans, save) == ACT_DONE;
        edit_key(m, ans);
        return 0;
    }
    if (ans == ME_IGNORE) return 0;
    if (ans == ME_DATA) { if (key >= 0 && key <= 255 && m->edit) type_char(m, (char)key); return 0; }
    if (ans == ME_TOGGLEWRAP) { m->wrap = !m->wrap; return 0; }
    if (ans == ME_TOGGLESCROLL) { m->scroll = !m->scroll; return 0; }
    if (ans == ME_WORDRIGHT) { word_right(m); return 0; }
    if (ans == ME_BOTTOMRIGHT) { m->row = m->topline + m->rows - 1; if (m->row > m->n - 1) m->row = m->n - 1; m->col = cur(m)->len; return 0; }
    return 0;
}

static void me_free(me_t *m) {
    int i;
    for (i = 0; i < m->n; i++) free(m->lines[i].s);
    free(m->lines);
}

int fn_memoedit(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    me_t m;
    value_t orig = nargs >= 1 && args[0].type == VAL_CHAR ? args[0] : val_str("");
    int save = 0, i, udf_false = 0, idle_zero = 0;
    value_t mr, mc;
    *result = orig;
    if (!screen_term_available()) return 0;   /* no screen: the string as it was */
    memset(&m, 0, sizeof(m));
    m.ctx = ctx;
    fn_maxrow(ctx, NULL, 0, &mr);
    fn_maxcol(ctx, NULL, 0, &mc);
    m.top = nargs > 1 && args[1].type == VAL_NUM ? (int)args[1].num : 0;
    m.left = nargs > 2 && args[2].type == VAL_NUM ? (int)args[2].num : 0;
    m.bottom = nargs > 3 && args[3].type == VAL_NUM ? (int)args[3].num : (int)mr.num;
    m.right = nargs > 4 && args[4].type == VAL_NUM ? (int)args[4].num : (int)mc.num;
    m.edit = !(nargs > 5 && args[5].type == VAL_LOGIC && !args[5].logic);
    if (nargs > 6 && args[6].type == VAL_CHAR && val_text(&args[6])[0]) m.udf = val_text(&args[6]);
    else if (nargs > 6 && args[6].type == VAL_LOGIC && !args[6].logic) udf_false = 1;
    m.width = nargs > 7 && args[7].type == VAL_NUM ? (int)args[7].num : m.right - m.left;
    m.tabsize = nargs > 8 && args[8].type == VAL_NUM ? (int)args[8].num : 4;
    m.row = nargs > 9 && args[9].type == VAL_NUM ? (int)args[9].num - 1 : 0;
    m.col = nargs > 10 && args[10].type == VAL_NUM ? (int)args[10].num : 0;
    if (m.width < 1) m.width = 1;
    if (m.tabsize < 1) m.tabsize = 4;
    m.rows = m.bottom - m.top + 1;
    m.cols = m.right - m.left + 1;
    if (m.rows < 1 || m.cols < 1) return 0;
    m.wrap = 1;
    load_text(&m, val_text(&orig), val_len(&orig));
    if (m.row < 0) m.row = 0;
    if (m.row > m.n - 1) m.row = m.n - 1;
    fix_col(&m);
    /* the window: the row and column the cursor is to have in it */
    {
        int wr = nargs > 11 && args[11].type == VAL_NUM ? (int)args[11].num : 0;
        int wc = nargs > 12 && args[12].type == VAL_NUM ? (int)args[12].num : -1;
        if (wr < 0) wr = 0;
        if (wr >= m.rows) wr = m.rows - 1;
        m.topline = m.row - wr;
        if (m.topline < 0) m.topline = 0;
        if (wc >= 0 && wc < m.cols) m.leftcol = m.col - wc;
        if (m.leftcol < 0) m.leftcol = 0;
    }
    gc_push_roots(&orig, 1);
    if (m.udf) {
        /* initialization: until the function answers 0 */
        for (i = 0; i < 64; i++) {
            int ans = me_call(&m, ME_INIT);
            if (ans == ME_DEFAULT) break;
            me_answer(&m, ans, 0, &save);
        }
    }
    if (m.wrap) reform_all(&m);
    if (m.row > m.n - 1) m.row = m.n - 1;
    me_follow(&m);
    me_paint(&m);
    me_park(&m);
    if (udf_false) { me_free(&m); gc_pop_roots(); return 0; }   /* shown, not edited */
    for (;;) {
        int key, done = 0;
        if (m.udf && !screen_key_pending()) {
            me_call(&m, ME_IDLE);
            me_park(&m);
        }
        key = screen_inkey(-1.0);
        if (key == 0) {
            /* a SET KEY handler took it -- or the input is gone, when it
             * keeps coming back empty (as ACHOICE() reads it) */
            if (++idle_zero >= 2) { save = 0; break; }
            continue;
        }
        idle_zero = 0;
        if (edit_key(&m, key)) {
            /* nonconfigurable: MEMOEDIT's own */
        } else if (m.udf) {
            int ans = me_call(&m, m.changed ? ME_UNKEYX : ME_UNKEY);
            done = me_answer(&m, ans, key, &save);
        } else if (is_config_key(key)) {
            done = config_key(&m, key, &save) == ACT_DONE;
        }
        if (m.row > m.n - 1) m.row = m.n - 1;
        if (m.row < 0) m.row = 0;
        fix_col(&m);
        me_follow(&m);
        me_paint(&m);
        me_park(&m);
        if (done) break;
    }
    *result = save ? text_value(&m) : orig;
    me_free(&m);
    gc_pop_roots();
    return 0;
}
