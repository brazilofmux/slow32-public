/* Clipper's Get system and TBrowse: the Get, TBrowse and TBColumn classes
 * as objects with exported variables and methods, the functions around
 * them (GETNEW, READMODAL, READVAR ..., TBROWSENEW, TBCOLUMNNEW ...) and
 * the screen functions a Clipper program uses beside them (SETPOS,
 * DISPOUT, SETCOLOR, DISPBOX, SETKEY ...).
 *
 * The shape is Clipper's; the depth is what the programs so far need.  A
 * Get edits its buffer by method (overstrike, insert, delete ...) and
 * assigns through its block or its variable's name; READ and READMODAL
 * still run screen.c's editor over the GETs it recorded.  A TBrowse
 * paints its window from the three navigation blocks and the columns'
 * blocks, all at once on stabilize(). */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <math.h>
#include "getsys.h"
#include "ast.h"
#include "func.h"
#include "screen.h"
#include "program.h"
#include "command.h"
#include "memvar.h"
#include "util.h"
#include "keys.h"
#include "gc.h"

/* ---- objects: variables and method tables ---- */

typedef int (*method_fn)(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs,
                         value_t *result);
typedef struct {
    const char *name;
    method_fn fn;
} method_t;

static value_t *iv(object_t *o, const char *name) {
    return object_ivar(o, name, 1);
}

static void iv_set(object_t *o, const char *name, value_t v) {
    *iv(o, name) = v;
}

static double iv_num(object_t *o, const char *name) {
    value_t *p = object_ivar(o, name, 0);
    return p && p->type == VAL_NUM ? p->num : 0;
}

static int iv_logic(object_t *o, const char *name) {
    value_t *p = object_ivar(o, name, 0);
    return p && p->type == VAL_LOGIC && p->logic;
}

static const char *iv_str(object_t *o, const char *name) {
    value_t *p = object_ivar(o, name, 0);
    return p && p->type == VAL_CHAR ? p->str : "";
}

static value_t vnum(double n) {
    return val_num_dec(n, 0, 0);
}

static int arg_num(value_t *args, int nargs, int i, int dflt) {
    return i < nargs && args[i].type == VAL_NUM ? (int)args[i].num : dflt;
}

static const char *arg_str(value_t *args, int nargs, int i, const char *dflt) {
    return i < nargs && args[i].type == VAL_CHAR ? args[i].str : dflt;
}

/* A block from source text, "{|| expr}" and the like (through the AST
 * evaluator: expr.c's older one has no blocks) */
static value_t block_from_text(expr_ctx_t *ctx, const char *text) {
    value_t v;
    if (ast_eval_dynamic(text, ctx, &v) != 0 || v.type != VAL_BLOCK) {
        ctx->error = NULL;
        return val_nil();
    }
    return v;
}

static int eval_block(expr_ctx_t *ctx, object_t *o, const char *name, value_t *args, int nargs,
                      value_t *result) {
    value_t *b = object_ivar(o, name, 0);
    if (!b || b->type != VAL_BLOCK) {
        *result = val_nil();
        return 0;
    }
    return ast_eval_block(ctx, b, args, nargs, result);
}

/* ---- the Get class ---- */

/* The buffer's text for a value under a picture: the picture's functions
 * (@! upper) and template, numbers in the template's or their own width,
 * dates as DTOC, logicals as T/F */
static void get_format(expr_ctx_t *ctx, const value_t *v, const char *picture, char *out, int size) {
    const char *tpl = picture;
    int upper = 0;
    char tmp[256];
    if (tpl[0] == '@') {
        tpl++;
        while (*tpl && *tpl != ' ') {
            if (*tpl == '!') upper = 1;
            tpl++;
        }
        tpl = skip_ws(tpl);
    }
    if (v->type == VAL_NUM) {
        int w = 0, d = 0, seen = 0;
        const char *p;
        for (p = tpl; *p; p++) {
            if (*p == '.') seen = 1;
            else if (*p == '9' || *p == '#') { if (seen) d++; w++; }
            else if (*p == ',') w++;
        }
        if (w > 0) snprintf(out, size, "%*.*f", w + (d ? 1 : 0), d, v->num);
        else val_to_display(v, out, size);
        return;
    }
    if (v->type == VAL_DATE) {
        value_t s;
        value_t a = *v;
        if (func_call(ctx, "DTOC", &a, 1, &s) == 0 && s.type == VAL_CHAR) snprintf(out, size, "%s", s.str);
        else out[0] = '\0';
        return;
    }
    if (v->type == VAL_LOGIC) {
        snprintf(out, size, "%s", v->logic ? "T" : "F");
        return;
    }
    val_to_string(v, tmp, sizeof(tmp));
    if (*tpl) screen_apply_picture(out, tmp, tpl, size);
    else snprintf(out, size, "%s", tmp);
    if (upper) {
        char *q;
        for (q = out; *q; q++) *q = (char)toupper((unsigned char)*q);
    }
}

/* The value a buffer holds, by the Get's type */
static value_t get_untransform(expr_ctx_t *ctx, object_t *o) {
    const char *buf = iv_str(o, "buffer");
    const char *type = iv_str(o, "type");
    value_t v, a;
    switch (type[0]) {
    case 'N': {
        const char *p = iv_str(o, "picture");
        int d = 0, seen = 0;
        for (; *p; p++) {
            if (*p == '.') seen = 1;
            else if (seen && (*p == '9' || *p == '#')) d++;
        }
        /* as wide as the edit buffer, as the field shows it (Harbour,
         * Clipper: a "999" field gives back a number three wide); nwidth
         * counts the digits before the point, as VAL's does */
        {
            int dec = seen ? d : (strchr(buf, '.') ? (int)strlen(strchr(buf, '.') + 1) : 0);
            int w = (int)strlen(buf) - (dec > 0 ? dec + 1 : 0);
            return val_num_dec(atof(buf), dec, w > 0 ? w : 1);
        }
    }
    case 'D':
        a = val_str(buf);
        if (func_call(ctx, "CTOD", &a, 1, &v) == 0) {
            {
                const char *p;
                int digits = 0;
                for (p = buf; *p; p++) if (*p >= '0' && *p <= '9') digits = 1;
                iv_set(o, "badDate", val_logic(digits && (v.type != VAL_DATE || v.date == 0)));   /* an empty date is a date */
            }
            return v;
        }
        return val_nil();
    case 'L':
        return val_logic(strchr("TtYy", buf[0]) != NULL);
    default:
        return val_str(buf);
    }
}

static int get_varget(expr_ctx_t *ctx, object_t *o, value_t *result) {
    value_t *b = object_ivar(o, "block", 0);
    if (b && b->type == VAL_BLOCK) return ast_eval_block(ctx, b, NULL, 0, result);
    if (iv_str(o, "name")[0]) return ast_eval_name(ctx, iv_str(o, "name"), result);
    *result = val_nil();
    return 0;
}

static int get_varput(expr_ctx_t *ctx, object_t *o, const value_t *v) {
    value_t *b = object_ivar(o, "block", 0);
    value_t a = *v, r;
    if (b && b->type == VAL_BLOCK) return ast_eval_block(ctx, b, &a, 1, &r);
    if (iv_str(o, "name")[0]) return ast_store_name(ctx, iv_str(o, "name"), v);
    return 0;
}

static void get_set_type(expr_ctx_t *ctx, object_t *o) {
    value_t v, t;
    if (get_varget(ctx, o, &v) != 0) { ctx->error = NULL; v = val_nil(); }
    func_call(ctx, "VALTYPE", &v, 1, &t);
    iv_set(o, "type", t);
}

static int get_update_buffer(expr_ctx_t *ctx, object_t *o) {
    value_t v;
    char buf[256];
    if (get_varget(ctx, o, &v) != 0) return -1;
    get_format(ctx, &v, iv_str(o, "picture"), buf, sizeof(buf));
    iv_set(o, "buffer", val_str(buf));
    return 0;
}

static object_t *get_new(expr_ctx_t *ctx, int row, int col, const value_t *block, const char *name,
                         const char *picture, const char *color) {
    object_t *o = object_new("GET");
    if (!o) return NULL;
    iv_set(o, "row", vnum(row));
    iv_set(o, "col", vnum(col));
    iv_set(o, "block", block ? *block : val_nil());
    iv_set(o, "name", val_str(name ? name : ""));
    iv_set(o, "picture", val_str(picture ? picture : ""));
    iv_set(o, "colorSpec", val_str(color ? color : screen_color_spec()));
    iv_set(o, "cargo", val_nil());
    iv_set(o, "preBlock", val_nil());
    iv_set(o, "postBlock", val_nil());
    iv_set(o, "exitState", val_nil());
    iv_set(o, "subScript", val_nil());
    iv_set(o, "reader", val_nil());
    iv_set(o, "hasFocus", val_logic(0));
    iv_set(o, "buffer", val_nil());
    iv_set(o, "original", val_nil());
    iv_set(o, "changed", val_logic(0));
    iv_set(o, "pos", vnum(0));
    iv_set(o, "clear", val_logic(0));
    iv_set(o, "decPos", vnum(0));
    iv_set(o, "minus", val_logic(0));
    iv_set(o, "typeOut", val_logic(0));
    iv_set(o, "badDate", val_logic(0));
    iv_set(o, "rejected", val_logic(0));
    get_set_type(ctx, o);
    return o;
}

static int m_get_vargetm(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    (void)self; (void)args; (void)nargs;
    return get_varget(ctx, o, result);
}

static int m_get_varput(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    value_t v = nargs >= 1 ? args[0] : val_nil();
    (void)self;
    if (get_varput(ctx, o, &v) != 0) return -1;
    *result = v;
    return 0;
}

static int step_editable(object_t *o, int pos, int len);

static int m_get_display(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    value_t *b = object_ivar(o, "buffer", 0);
    (void)args; (void)nargs;
    if (!b || b->type != VAL_CHAR) {
        if (get_update_buffer(ctx, o) != 0) return -1;
    }
    screen_out_at((int)iv_num(o, "row"), (int)iv_num(o, "col"), iv_str(o, "buffer"));
    *result = *self;
    return 0;
}

static int m_get_setfocus(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    value_t v;
    (void)args; (void)nargs;
    if (get_varget(ctx, o, &v) != 0) return -1;
    iv_set(o, "original", v);
    if (get_update_buffer(ctx, o) != 0) return -1;
    iv_set(o, "hasFocus", val_logic(1));
    iv_set(o, "changed", val_logic(0));
    iv_set(o, "rejected", val_logic(0));
    iv_set(o, "typeOut", val_logic(0));
    {
        /* Get:clear for a number or @K (the guide); the cursor on the first
         * position a key fills */
        const char *pic = iv_str(o, "picture");
        int k = 0;
        if (pic[0] == '@') { const char *p; for (p = pic + 1; *p && *p != ' '; p++) if (*p == 'K' || *p == 'k') k = 1; }
        iv_set(o, "clear", val_logic(iv_str(o, "type")[0] == 'N' || k));
        iv_set(o, "pos", vnum(step_editable(o, 1, (int)strlen(iv_str(o, "buffer")))));
    }
    *result = *self;
    return 0;
}

static int m_get_killfocus(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    (void)args; (void)nargs;
    iv_set(o, "hasFocus", val_logic(0));
    iv_set(o, "clear", val_logic(0));
    if (get_update_buffer(ctx, o) != 0)
        ctx->error = NULL;
    else if (screen_term_available())   /* the variable shown as it now is, on a screen */
        screen_out_at((int)iv_num(o, "row"), (int)iv_num(o, "col"), iv_str(o, "buffer"));
    *result = *self;
    return 0;
}

static int m_get_assign(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    value_t v;
    (void)args; (void)nargs;
    if (object_ivar(o, "buffer", 0)->type != VAL_CHAR) { *result = *self; return 0; }
    v = get_untransform(ctx, o);
    if (get_varput(ctx, o, &v) != 0) return -1;
    *result = *self;
    return 0;
}

static int m_get_reset(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    (void)args; (void)nargs;
    if (get_update_buffer(ctx, o) != 0) return -1;
    /* the buffer from the variable, the cursor on the first editable
     * position (the guide); changed stays as it is -- GETSYS's RangeCheck
     * asks it after GetPostValidate has reset the Get */
    iv_set(o, "rejected", val_logic(0));
    iv_set(o, "typeOut", val_logic(0));
    iv_set(o, "pos", vnum(step_editable(o, 1, (int)strlen(iv_str(o, "buffer")))));
    *result = *self;
    return 0;
}

static int m_get_undo(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    value_t *orig = object_ivar(o, "original", 0);
    (void)args; (void)nargs;
    if (orig && orig->type != VAL_NIL) {
        if (get_varput(ctx, o, orig) != 0) return -1;
    }
    return m_get_reset(ctx, o, self, args, nargs, result);
}

static int m_get_updatebuffer(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    (void)args; (void)nargs;
    if (get_update_buffer(ctx, o) != 0) return -1;
    *result = *self;
    return 0;
}

static int m_get_untransform(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    (void)self; (void)args; (void)nargs;
    *result = get_untransform(ctx, o);
    return 0;
}

/* The editing methods work on the buffer at pos (1-based) */
/* The Get's template: the picture's, or one made from the buffer -- a
 * date's digits and separators, a number's digits, a string's X's */
static void get_template(object_t *o, char *t, int size) {
    const char *pic = iv_str(o, "picture");
    const char *buf = iv_str(o, "buffer");
    const char *type = iv_str(o, "type");
    int k;
    if (pic[0] == '@') { pic = strchr(pic, ' '); pic = pic ? pic + 1 : ""; }
    if (*pic) { str_copy(t, pic, size); return; }
    for (k = 0; buf[k] && k < size - 1; k++) {
        if (type[0] == 'D') t[k] = (buf[k] == '/' || buf[k] == '.' || buf[k] == '-') ? buf[k] : '9';
        else if (type[0] == 'N') t[k] = buf[k] == '.' ? '.' : '9';
        else t[k] = 'X';
    }
    t[k] = '\0';
}

static int tmpl_literal(const char *t, int pos) {   /* pos is 1-based */
    int n = (int)strlen(t);
    if (pos < 1 || pos > n) return 0;
    return !strchr("9#ANXLY!anxly", t[pos - 1]);
}

/* the cursor onto an editable position, from pos forward */
static int step_editable(object_t *o, int pos, int len) {
    char t[256];
    get_template(o, t, sizeof(t));
    while (pos <= len && tmpl_literal(t, pos)) pos++;
    return pos;
}

/* Get:clear, set by setFocus() for a number or @K: the first character
 * typed blanks the buffer, the template's literals kept */
static void get_apply_clear(object_t *o, char *buf, int len, int *pos) {
    char t[256];
    int k;
    if (!iv_logic(o, "clear")) return;
    get_template(o, t, sizeof(t));
    for (k = 1; k <= len; k++) buf[k - 1] = tmpl_literal(t, k) ? t[k - 1] : ' ';
    *pos = step_editable(o, 1, len);
    iv_set(o, "clear", val_logic(0));
}

static char *edit_buf(object_t *o, int *pos, int *len) {
    value_t *b = object_ivar(o, "buffer", 0);
    if (!b || b->type != VAL_CHAR) return NULL;
    *pos = (int)iv_num(o, "pos");
    *len = (int)strlen(b->str);
    if (*pos < 1) *pos = 1;
    return b->str;
}

/* A typed character under the picture's functions: @! makes it upper case */
static char edit_char(object_t *o, const char *s) {
    char c = s[0];
    const char *pic = iv_str(o, "picture");
    if (pic[0] == '@') {
        const char *p;
        for (p = pic + 1; *p && *p != ' '; p++) {
            if (*p == '!') c = (char)toupper((unsigned char)c);
        }
    }
    return c;
}

static int m_get_overstrike(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    int pos, len;
    char *buf = edit_buf(o, &pos, &len);
    (void)ctx;
    if (buf && nargs >= 1 && args[0].type == VAL_CHAR && args[0].str[0]) {
        get_apply_clear(o, buf, len, &pos);
        pos = step_editable(o, pos, len);
        if (pos > len) {
            iv_set(o, "typeOut", val_logic(1));
        } else {
            buf[pos - 1] = edit_char(o, args[0].str);
            pos = step_editable(o, pos + 1, len);   /* a literal is stepped over */
            iv_set(o, "pos", vnum(pos));
            iv_set(o, "changed", val_logic(1));
            if (pos > len) iv_set(o, "typeOut", val_logic(1));
        }
    }
    *result = *self;
    return 0;
}

static int m_get_insert(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    int pos, len;
    char *buf = edit_buf(o, &pos, &len);
    (void)ctx;
    if (buf && nargs >= 1 && args[0].type == VAL_CHAR && args[0].str[0]) {
        get_apply_clear(o, buf, len, &pos);
        pos = step_editable(o, pos, len);
        if (pos > len) {
            iv_set(o, "typeOut", val_logic(1));
        } else {
            memmove(buf + pos, buf + pos - 1, (size_t)(len - pos));
            buf[pos - 1] = edit_char(o, args[0].str);
            pos = step_editable(o, pos + 1, len);   /* a literal is stepped over */
            iv_set(o, "pos", vnum(pos));
            iv_set(o, "changed", val_logic(1));
            if (pos > len) iv_set(o, "typeOut", val_logic(1));
        }
    }
    *result = *self;
    return 0;
}

static int m_get_delete(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    int pos, len;
    char *buf = edit_buf(o, &pos, &len);
    (void)ctx; (void)args; (void)nargs;
    if (buf && pos <= len) {
        memmove(buf + pos - 1, buf + pos, (size_t)(len - pos));
        buf[len - 1] = ' ';
        iv_set(o, "changed", val_logic(1));
    }
    *result = *self;
    return 0;
}

static int m_get_backspace(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    int pos = (int)iv_num(o, "pos");
    if (pos > 1) {
        iv_set(o, "pos", vnum(pos - 1));
        return m_get_delete(ctx, o, self, args, nargs, result);
    }
    *result = *self;
    return 0;
}

/* The word moves and deletions GETSYS.PRG's reader binds to Ctrl-Left,
 * Ctrl-Right, Ctrl-Backspace, Ctrl-T and Ctrl-Y: a word is a run of
 * non-blanks */
static int word_start_before(const char *buf, int len, int pos) {   /* 1-based pos; the start of the word before it */
    int i = pos - 1;   /* 0-based index just left of the cursor */
    if (i > len) i = len;
    while (i > 0 && buf[i - 1] == ' ') i--;
    while (i > 0 && buf[i - 1] != ' ') i--;
    return i + 1;
}

static int word_start_after(const char *buf, int len, int pos) {
    int i = pos - 1;
    while (i < len && buf[i] != ' ') i++;
    while (i < len && buf[i] == ' ') i++;
    return i < len ? i + 1 : len;
}

static int m_get_wordleft(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    int pos, len;
    char *buf = edit_buf(o, &pos, &len);
    (void)ctx; (void)args; (void)nargs;
    if (buf) iv_set(o, "pos", vnum(word_start_before(buf, len, pos)));
    iv_set(o, "typeOut", val_logic(0));
    *result = *self;
    return 0;
}

static int m_get_wordright(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    int pos, len;
    char *buf = edit_buf(o, &pos, &len);
    (void)ctx; (void)args; (void)nargs;
    if (buf) iv_set(o, "pos", vnum(word_start_after(buf, len, pos)));
    iv_set(o, "typeOut", val_logic(0));
    *result = *self;
    return 0;
}

static int m_get_delwordleft(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    int pos, len;
    char *buf = edit_buf(o, &pos, &len);
    (void)ctx; (void)args; (void)nargs;
    if (buf && pos > 1) {
        /* Harbour's rule: a blank just before the cursor goes first, then
         * the run of non-blanks before that */
        if (buf[pos - 2] == ' ') {
            memmove(buf + pos - 2, buf + pos - 1, (size_t)(len - pos + 1));
            buf[len - 1] = ' ';
            pos--;
        }
        while (pos > 1 && buf[pos - 2] != ' ') {
            memmove(buf + pos - 2, buf + pos - 1, (size_t)(len - pos + 1));
            buf[len - 1] = ' ';
            pos--;
        }
        iv_set(o, "pos", vnum(pos));
        iv_set(o, "changed", val_logic(1));
    }
    *result = *self;
    return 0;
}

static int m_get_delwordright(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    int pos, len;
    char *buf = edit_buf(o, &pos, &len);
    (void)ctx; (void)args; (void)nargs;
    if (buf && pos <= len) {
        /* Harbour's rule: through the first blank after the cursor (the
         * word and one blank; on a blank, that blank alone) */
        int n = 0;
        while (pos - 1 + n < len && buf[pos - 1 + n] != ' ') n++;
        if (pos - 1 + n < len) n++;
        memmove(buf + pos - 1, buf + pos - 1 + n, (size_t)(len - pos + 1 - n));
        memset(buf + len - n, ' ', (size_t)n);
        iv_set(o, "changed", val_logic(1));
    }
    *result = *self;
    return 0;
}

static int m_get_delend(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    int pos, len;
    char *buf = edit_buf(o, &pos, &len);
    (void)ctx; (void)args; (void)nargs;
    if (buf && pos <= len) {
        memset(buf + pos - 1, ' ', (size_t)(len - pos + 1));
        iv_set(o, "changed", val_logic(1));
    }
    *result = *self;
    return 0;
}

static int m_get_home(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    iv_set(o, "pos", vnum(1));
    iv_set(o, "typeOut", val_logic(0));
    *result = *self;
    return 0;
}

static int m_get_end(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    int pos, len;
    char *buf = edit_buf(o, &pos, &len);
    (void)ctx; (void)args; (void)nargs;
    if (buf) {
        int e = len;
        while (e > 0 && buf[e - 1] == ' ') e--;
        iv_set(o, "pos", vnum(e < len ? e + 1 : len));
        iv_set(o, "typeOut", val_logic(0));
    }
    *result = *self;
    return 0;
}

static int m_get_left(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    int pos = (int)iv_num(o, "pos");
    (void)ctx; (void)args; (void)nargs;
    if (pos > 1) iv_set(o, "pos", vnum(pos - 1));
    iv_set(o, "typeOut", val_logic(0));
    *result = *self;
    return 0;
}

static int m_get_right(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    int pos, len;
    char *buf = edit_buf(o, &pos, &len);
    (void)ctx; (void)args; (void)nargs;
    if (buf) {
        if (pos < len) iv_set(o, "pos", vnum(pos + 1));
        else iv_set(o, "typeOut", val_logic(1));
    }
    *result = *self;
    return 0;
}

static int m_get_todecpos(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    int pos, len;
    char *buf = edit_buf(o, &pos, &len);
    (void)ctx; (void)args; (void)nargs;
    if (buf) {
        const char *d = strchr(buf, '.');
        if (d) iv_set(o, "pos", vnum((int)(d - buf) + 2));
    }
    *result = *self;
    return 0;
}

/* badDate(): the buffer of a date Get holds digits that make no date */
static int m_get_baddate(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    const char *buf = iv_str(o, "buffer"), *p;
    int digits = 0, bad = 0;
    (void)self; (void)args; (void)nargs;
    if (iv_str(o, "type")[0] == 'D') {
        for (p = buf; *p; p++) if (*p >= '0' && *p <= '9') digits = 1;
        if (digits) {
            value_t a = val_str(buf), v;
            if (func_call(ctx, "CTOD", &a, 1, &v) != 0) { ctx->error = NULL; bad = 1; }
            else bad = v.type != VAL_DATE || v.date == 0;
        }
    }
    *result = val_logic(bad);
    return 0;
}

static const method_t get_methods[] = {
    { "VARGET", m_get_vargetm }, { "BADDATE", m_get_baddate }, { "VARPUT", m_get_varput }, { "DISPLAY", m_get_display },
    { "SETFOCUS", m_get_setfocus }, { "KILLFOCUS", m_get_killfocus }, { "ASSIGN", m_get_assign },
    { "RESET", m_get_reset }, { "UNDO", m_get_undo }, { "UPDATEBUFFER", m_get_updatebuffer },
    { "UNTRANSFORM", m_get_untransform }, { "OVERSTRIKE", m_get_overstrike }, { "INSERT", m_get_insert },
    { "DELETE", m_get_delete }, { "BACKSPACE", m_get_backspace }, { "HOME", m_get_home }, { "WORDLEFT", m_get_wordleft }, { "WORDRIGHT", m_get_wordright }, { "DELWORDLEFT", m_get_delwordleft }, { "DELWORDRIGHT", m_get_delwordright }, { "DELEND", m_get_delend },
    { "END", m_get_end }, { "LEFT", m_get_left }, { "RIGHT", m_get_right }, { "TODECPOS", m_get_todecpos },
    { NULL, NULL }
};

/* GETNEW([row], [col], [block], [name], [picture], [color]) */
int fn_getnew(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    object_t *o = get_new(ctx, arg_num(args, nargs, 0, 0), arg_num(args, nargs, 1, 0),
                          nargs >= 3 && args[2].type == VAL_BLOCK ? &args[2] : NULL,
                          arg_str(args, nargs, 3, ""), arg_str(args, nargs, 4, ""),
                          nargs >= 6 && args[5].type == VAL_CHAR ? args[5].str : NULL);
    if (!o) { ctx->error = "Out of memory"; return -1; }
    *result = val_object(o);
    return 0;
}

value_t getsys_note_get(int row, int col, const char *varname, const char *picture,
                        const char *valid_expr, const char *when_expr) {
    expr_ctx_t *ctx = cmd_get_expr_ctx();
    memvar_store_t *store = cmd_get_memvar_store();
    object_t *o = get_new(ctx, row, col, NULL, varname, picture, NULL);
    value_t v, list;
    char text[600];
    if (!o) return val_nil();
    if (valid_expr && valid_expr[0]) {
        snprintf(text, sizeof(text), "{|| %s}", valid_expr);
        iv_set(o, "postBlock", block_from_text(ctx, text));
    }
    if (when_expr && when_expr[0]) {
        snprintf(text, sizeof(text), "{|| %s}", when_expr);
        iv_set(o, "preBlock", block_from_text(ctx, text));
    }
    v = val_object(o);
    /* GETLIST collects the Gets as Clipper's does; Clipper's compiler gives
     * every function a LOCAL GetList := {} of its own, so one that is not
     * visible here is made, a LOCAL of this depth (a program's ReadModal --
     * CA's GETSYS.PRG -- reads it) */
    if (memvar_find(store, "GETLIST", &list) != 0 || list.type != VAL_ARRAY || !list.array) {
        if (ast_make_array(ctx, NULL, 0, &list) != 0) return v;
        memvar_set_local(store, "GETLIST", &list);
    }
    {
        value_t a[2], r;
        a[0] = list;
        a[1] = v;
        func_call(ctx, "AADD", a, 2, &r);
    }
    return v;
}

/* @ GET on something that is not a plain variable -- an array element, an
 * alias->field -- gets the block Clipper's preprocessor would give it,
 * {|x| IIF(PCOUNT() == 0, <target>, <target> := x)}, reading and storing
 * through the expression each time (RL's @ 11,09 GET chdr_lines[ar_index]) */
value_t getsys_note_get_expr(int row, int col, const char *target, const char *picture,
                             const char *valid_expr, const char *when_expr) {
    expr_ctx_t *ctx = cmd_get_expr_ctx();
    memvar_store_t *store = cmd_get_memvar_store();
    char text[1200];
    value_t block, v, list;
    object_t *o;
    snprintf(text, sizeof(text), "{|x| IIF(PCOUNT() == 0, %s, %s := x)}", target, target);
    block = block_from_text(ctx, text);
    o = get_new(ctx, row, col, block.type == VAL_BLOCK ? &block : NULL, target, picture, NULL);
    if (!o) return val_nil();
    if (valid_expr && valid_expr[0]) {
        snprintf(text, sizeof(text), "{|| %s}", valid_expr);
        iv_set(o, "postBlock", block_from_text(ctx, text));
    }
    if (when_expr && when_expr[0]) {
        snprintf(text, sizeof(text), "{|| %s}", when_expr);
        iv_set(o, "preBlock", block_from_text(ctx, text));
    }
    v = val_object(o);
    if (memvar_find(store, "GETLIST", &list) != 0 || list.type != VAL_ARRAY || !list.array) {
        if (ast_make_array(ctx, NULL, 0, &list) != 0) return v;
        memvar_set_local(store, "GETLIST", &list);
    }
    {
        value_t a[2], r;
        a[0] = list;
        a[1] = v;
        func_call(ctx, "AADD", a, 2, &r);
    }
    return v;
}

/* After READ: Clipper's READ is ReadModal(GetList) then GetList := {}, so
 * the Gets read are released; left in the array they accumulated -- RL's
 * editor loop issues eight GETs a pass, and 8 KB Get objects piled up to
 * the heap's end */
void getsys_release_getlist(void) {
    expr_ctx_t *ctx = cmd_get_expr_ctx();
    memvar_store_t *store = cmd_get_memvar_store();
    value_t list, empty;
    if (memvar_find(store, "GETLIST", &list) == 0 && list.type == VAL_ARRAY) {
        if (ast_make_array(ctx, NULL, 0, &empty) == 0) memvar_set(store, "GETLIST", &empty);
    }
}

/* A Get's postBlock from text (RANGE's VALID {|_1| RangeCheck(...)}) */
void getsys_set_postblock(value_t *obj, const char *text) {
    if (obj->type == VAL_OBJECT && obj->obj) iv_set(obj->obj, "postBlock", block_from_text(cmd_get_expr_ctx(), text));
}

/* RANGECHECK(oGet, , lo, hi): RANGE's VALID -- an unedited value passes;
 * an edited one outside lo..hi gets "Range: lo - hi" on the scoreboard and
 * fails (GETSYS's own; a program's RangeCheck replaces it) */
int fn_rangecheck(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    value_t v, lt, gt, a[2], t;
    char lo[64], hi[64], msg[160];
    *result = val_logic(1);
    if (nargs < 4 || args[0].type != VAL_OBJECT) return 0;
    if (!iv_logic(args[0].obj, "changed")) return 0;
    if (get_varget(ctx, args[0].obj, &v) != 0) { ctx->error = NULL; return 0; }
    if (ast_eval_binop(ctx, OP_GE, &v, &args[2], &lt) == 0 && lt.type == VAL_LOGIC && lt.logic &&
        ast_eval_binop(ctx, OP_LE, &v, &args[3], &gt) == 0 && gt.type == VAL_LOGIC && gt.logic) return 0;
    ctx->error = NULL;
    a[1] = val_str("");
    a[0] = args[2];
    if (func_call(ctx, "TRANSFORM", a, 2, &t) == 0 && t.type == VAL_CHAR) str_copy(lo, val_text(&t), sizeof(lo)); else lo[0] = '\0';
    a[0] = args[3];
    if (func_call(ctx, "TRANSFORM", a, 2, &t) == 0 && t.type == VAL_CHAR) str_copy(hi, val_text(&t), sizeof(hi)); else hi[0] = '\0';
    ctx->error = NULL;
    snprintf(msg, sizeof(msg), "Range: %s - %s", skip_ws(lo), skip_ws(hi));
    msg[79] = '\0';
    scoreboard_message(msg);
    *result = val_logic(0);
    return 0;
}

/* READ when the program supplies ReadModal() (CA's GETSYS.PRG linked in):
 * ReadModal(GetList) runs the Gets through it, then the list and the
 * recorded GETs are cleared.  1 when it did; 0 for the built-in reader. */
int getsys_read_through_program(void) {
    expr_ctx_t *ctx = cmd_get_expr_ctx();
    memvar_store_t *store = cmd_get_memvar_store();
    value_t list, r, empty;
    if (!func_is_overridden("READMODAL")) return 0;
    if (memvar_find(store, "GETLIST", &list) != 0 || list.type != VAL_ARRAY) {
        if (ast_make_array(ctx, NULL, 0, &list) != 0) return 0;
        memvar_set_local(store, "GETLIST", &list);
    }
    if (func_call(ctx, "READMODAL", &list, 1, &r) != 0) { ctx->error = NULL; }
    screen_clear_gets();
    if (ast_make_array(ctx, NULL, 0, &empty) == 0) memvar_set(store, "GETLIST", &empty);
    return 1;
}

/* ---- READ around the Gets ---- */

/* The Get object's value and its store, for screen.c's editor */
int getsys_has_block(const value_t *obj) {
    value_t *b = obj->type == VAL_OBJECT ? object_ivar(obj->obj, "block", 0) : NULL;
    return b && b->type == VAL_BLOCK;
}

int getsys_varget(const value_t *obj, value_t *out) {
    if (obj->type != VAL_OBJECT) { *out = val_nil(); return -1; }
    return get_varget(cmd_get_expr_ctx(), obj->obj, out);
}

int getsys_varput(const value_t *obj, const value_t *v) {
    if (obj->type != VAL_OBJECT) return -1;
    return get_varput(cmd_get_expr_ctx(), obj->obj, v);
}

/* ---- The Get system: READMODAL and the functions a reader uses ----
 *
 * Clipper's READ is a loop over the Get objects of a GetList: each is
 * handed to its own reader block, or to the standard reader, and where
 * the read goes next is the exitState that reader leaves.  The standard
 * reader is GetPreValidate (WHEN), keys through GetApplyKey until an
 * exit, then GetPostValidate (assign, VALID).  These are written here
 * from the Get system's documented behaviour, and a program's own reader
 * calls the same functions, as Clipper programs do to extend it. */

#define GE_NOEXIT 0
#define GE_UP     1
#define GE_DOWN   2
#define GE_TOP    3
#define GE_BOTTOM 4
#define GE_ENTER  5
#define GE_WRITE  6
#define GE_ESCAPE 7
#define GE_WHEN   8

static int gs_send(expr_ctx_t *ctx, value_t *g, const char *method, value_t *args, int nargs,
                   value_t *result) {
    value_t r;
    if (!result) result = &r;
    if (object_send(ctx, g, method, args, nargs, 1, result) != 0) { ctx->error = NULL; return -1; }
    return 0;
}

static int gs_exit_state(object_t *o) {
    value_t *v = object_ivar(o, "exitState", 0);
    return v && v->type == VAL_NUM ? (int)v->num : GE_NOEXIT;
}

static void gs_set_exit(object_t *o, int state) {
    iv_set(o, "exitState", vnum(state));
}

static int gs_updated;   /* UPDATED() of the READMODAL running */

/* GETPREVALIDATE(oGet): its WHEN (preBlock); .F. leaves exitState GE_WHEN */
static int gs_prevalidate(expr_ctx_t *ctx, value_t *g) {
    object_t *o = g->obj;
    value_t *pre = object_ivar(o, "preBlock", 0), r;
    int when = 1;
    gs_set_exit(o, GE_NOEXIT);
    if (pre && pre->type == VAL_BLOCK) {
        value_t arg = *g;
        if (ast_eval_block(ctx, pre, &arg, 1, &r) != 0) { ctx->error = NULL; r = val_logic(1); }
        when = r.type != VAL_LOGIC || r.logic;
        gs_send(ctx, g, "DISPLAY", NULL, 0, NULL);
    }
    if (!when) gs_set_exit(o, GE_WHEN);
    return when;
}

/* GETPOSTVALIDATE(oGet): a date that is not one is refused; a changed
 * buffer is assigned (UPDATED() turns true); then its VALID (postBlock) */
static int gs_postvalidate(expr_ctx_t *ctx, value_t *g) {
    object_t *o = g->obj;
    value_t *post, r, bad;
    int valid = 1;
    if (gs_exit_state(o) == GE_ESCAPE) return 1;
    if (gs_send(ctx, g, "BADDATE", NULL, 0, &bad) == 0 && bad.type == VAL_LOGIC && bad.logic) {
        gs_send(ctx, g, "HOME", NULL, 0, NULL);
        scoreboard_message("Invalid Date");
        return 0;
    }
    if (iv_logic(o, "changed")) {
        gs_send(ctx, g, "ASSIGN", NULL, 0, NULL);
        gs_updated = 1;
        screen_set_read_updated(1);
    }
    gs_send(ctx, g, "RESET", NULL, 0, NULL);
    post = object_ivar(o, "postBlock", 0);
    if (post && post->type == VAL_BLOCK) {
        value_t arg = *g;
        gs_send(ctx, g, "DISPLAY", NULL, 0, NULL);
        if (ast_eval_block(ctx, post, &arg, 1, &r) != 0) { ctx->error = NULL; r = val_logic(1); }
        valid = r.type != VAL_LOGIC || r.logic;
        gs_send(ctx, g, "UPDATEBUFFER", NULL, 0, NULL);
    }
    return valid;
}

/* GETAPPLYKEY(oGet, nKey): one key, as the standard reader takes it */
static void gs_applykey(expr_ctx_t *ctx, value_t *g, int key) {
    object_t *o = g->obj;
    const set_options_t *so = cmd_get_set_opts();
    value_t k = vnum(key), blk, r;
    /* a key SET KEY has given a block: the block, not the Get */
    if (func_call(ctx, "SETKEY", &k, 1, &blk) == 0 && blk.type == VAL_BLOCK) {
        value_t a[3];
        a[0] = val_str(screen_read_var());   /* procedure, line, variable: as SET KEY's */
        a[1] = vnum(0);
        a[2] = val_str(screen_read_var());
        (void)ast_eval_block(ctx, &blk, a, 3, &r);
        ctx->error = NULL;
        return;
    }
    ctx->error = NULL;
    switch (key) {
    case 5:   /* K_UP */
    case 271: /* K_SH_TAB */
        gs_set_exit(o, GE_UP); return;
    case 24:  /* K_DOWN */
    case 9:   /* K_TAB */
        gs_set_exit(o, GE_DOWN); return;
    case 13:  /* K_ENTER */
        gs_set_exit(o, GE_ENTER); return;
    case 27:  /* K_ESC */
        if (so && so->escape) {
            gs_send(ctx, g, "UNDO", NULL, 0, NULL);
            gs_set_exit(o, GE_ESCAPE);
        }
        return;
    case 18:  /* K_PGUP */
    case 3:   /* K_PGDN */
    case 23:  /* K_CTRL_W */
        gs_set_exit(o, GE_WRITE); return;
    case 29:  /* K_CTRL_HOME */
        gs_set_exit(o, GE_TOP); return;
    case 22:  /* K_INS */
        {
            value_t on;
            (void)func_call(ctx, "READINSERT", NULL, 0, &on);
            on = val_logic(!(on.type == VAL_LOGIC && on.logic));
            (void)func_call(ctx, "READINSERT", &on, 1, &r);
        }
        return;
    case 21:  /* K_CTRL_U */ gs_send(ctx, g, "UNDO", NULL, 0, NULL); return;
    case 1:   /* K_HOME */   gs_send(ctx, g, "HOME", NULL, 0, NULL); return;
    case 6:   /* K_END */    gs_send(ctx, g, "END", NULL, 0, NULL); return;
    case 4:   /* K_RIGHT */  gs_send(ctx, g, "RIGHT", NULL, 0, NULL); return;
    case 19:  /* K_LEFT */   gs_send(ctx, g, "LEFT", NULL, 0, NULL); return;
    case 2:   /* K_CTRL_RIGHT */ gs_send(ctx, g, "WORDRIGHT", NULL, 0, NULL); return;
    case 26:  /* K_CTRL_LEFT */  gs_send(ctx, g, "WORDLEFT", NULL, 0, NULL); return;
    case 8:   /* K_BS */     gs_send(ctx, g, "BACKSPACE", NULL, 0, NULL); return;
    case 7:   /* K_DEL */    gs_send(ctx, g, "DELETE", NULL, 0, NULL); return;
    case 20:  /* K_CTRL_T */ gs_send(ctx, g, "DELWORDRIGHT", NULL, 0, NULL); return;
    case 25:  /* K_CTRL_Y */ gs_send(ctx, g, "DELEND", NULL, 0, NULL); return;
    case 127: /* K_CTRL_BS */ gs_send(ctx, g, "DELWORDLEFT", NULL, 0, NULL); return;
    default:
        break;
    }
    if (key >= 32 && key <= 255) {
        const char *type = iv_str(o, "type");
        if (type[0] == 'N' && (key == '.' || key == ',')) {
            gs_send(ctx, g, "TODECPOS", NULL, 0, NULL);
        } else {
            char c[2];
            value_t on, a;
            c[0] = (char)key;
            c[1] = '\0';
            a = val_str(c);
            (void)func_call(ctx, "READINSERT", NULL, 0, &on);
            ctx->error = NULL;
            gs_send(ctx, g, on.type == VAL_LOGIC && on.logic ? "INSERT" : "OVERSTRIKE", &a, 1, NULL);
            if (iv_logic(o, "typeOut")) {
                /* past the field's end: on to the next, unless SET CONFIRM */
                if (!(so && so->confirm)) gs_set_exit(o, GE_ENTER);
            }
        }
    }
}

/* GETREADER(oGet): the standard reader */
static void gs_reader(expr_ctx_t *ctx, value_t *g) {
    object_t *o = g->obj;
    if (!gs_prevalidate(ctx, g)) return;
    gs_send(ctx, g, "SETFOCUS", NULL, 0, NULL);
    while (gs_exit_state(o) == GE_NOEXIT) {
        if (iv_logic(o, "typeOut")) gs_set_exit(o, GE_ENTER);   /* nothing to edit */
        while (gs_exit_state(o) == GE_NOEXIT) {
            int key = screen_inkey(-1.0);
            if (key == 0 && !screen_term_available()) key = 27;   /* the end of the input: Esc */
            gs_applykey(ctx, g, key);
        }
        if (!gs_postvalidate(ctx, g)) gs_set_exit(o, GE_NOEXIT);
    }
    gs_send(ctx, g, "KILLFOCUS", NULL, 0, NULL);
}

static int read_exit_flag, read_insert_flag = 0, read_kill_flag;

/* Where READMODAL goes after the Get at pos (1-based): 0 to end the READ */
static int gs_settle(array_t *a, int pos, int *last_dir) {
    object_t *o = a->el[pos - 1].obj;
    int st = gs_exit_state(o);
    if (st == GE_ESCAPE || st == GE_WRITE) return 0;
    if (st == GE_WHEN) st = *last_dir;   /* skipped: on the way it was going */
    else if (st != GE_NOEXIT) *last_dir = st;
    switch (st) {
    case GE_UP:     pos--; break;
    case GE_DOWN:   pos++; break;
    case GE_TOP:    pos = 1; break;
    case GE_BOTTOM: pos = a->len; break;
    case GE_ENTER:  pos++; break;
    default:        break;   /* GE_NOEXIT: the same Get again */
    }
    if (pos < 1) {
        if (read_exit_flag) return 0;   /* above the first: out only with READEXIT() */
        pos = 1;
        *last_dir = GE_DOWN;
    }
    if (pos > a->len) {
        if (st == GE_ENTER || read_exit_flag) return 0;   /* Enter past the last ends the READ */
        pos = a->len;
        *last_dir = GE_UP;
    }
    return pos;
}

/* READMODAL(aGetList [, nPos]): the Gets read in turn, each by its own
 * reader block or the standard reader, moving as each one's exitState
 * says.  Answers UPDATED().  Without an array, the GETs recorded by
 * @ ... GET, as READ runs them. */
int fn_readmodal(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    if (nargs >= 1 && args[0].type == VAL_ARRAY && args[0].array) {
        array_t *a = args[0].array;
        value_t saved_get = screen_active_get();
        char saved_var[MEMVAR_NAMELEN];
        int saved_updated = gs_updated, pos, last_dir = GE_DOWN, i, guard = 0;
        str_copy(saved_var, screen_read_var(), sizeof(saved_var));
        gs_updated = 0;
        read_kill_flag = 0;
        screen_set_read_updated(0);
        pos = (nargs >= 2 && args[1].type == VAL_NUM) ? (int)args[1].num : 1;
        if (pos < 1 || pos > a->len) pos = 1;
        for (i = 0; i < a->len; i++)   /* the fields as they stand, as @ ... GET shows them */
            if (a->el[i].type == VAL_OBJECT) gs_send(ctx, &a->el[i], "DISPLAY", NULL, 0, NULL);
        while (pos >= 1 && pos <= a->len && guard++ < 100000) {
            value_t *g = &a->el[pos - 1], *rd;
            if (g->type != VAL_OBJECT) {   /* not a Get: past it */
                pos = last_dir == GE_UP ? pos - 1 : pos + 1;
                continue;
            }
            {
                char var[MEMVAR_NAMELEN];
                int k;
                str_copy(var, iv_str(g->obj, "name"), sizeof(var));
                for (k = 0; var[k]; k++) var[k] = (char)toupper((unsigned char)var[k]);
                screen_set_modal_get(g, var);
            }
            rd = object_ivar(g->obj, "reader", 0);
            if (rd && rd->type == VAL_BLOCK) {
                value_t arg = *g, r;
                if (ast_eval_block(ctx, rd, &arg, 1, &r) != 0) {
                    screen_set_modal_get(&saved_get, saved_var);
                    return -1;
                }
            } else {
                gs_reader(ctx, g);
            }
            if (read_kill_flag) break;
            pos = gs_settle(a, pos, &last_dir);
        }
        *result = val_logic(gs_updated);
        screen_set_read_updated(gs_updated);
        gs_updated = saved_updated || gs_updated;
        screen_set_modal_get(&saved_get, saved_var);
        return 0;
    }
    screen_read();   /* the GETs recorded so far, as READ runs them */
    *result = val_logic(screen_read_updated());
    return 0;
}

static int gs_get_arg(value_t *args, int nargs) {
    return nargs >= 1 && args[0].type == VAL_OBJECT && args[0].obj;
}

int fn_getreader(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    *result = val_nil();
    if (gs_get_arg(args, nargs)) gs_reader(ctx, &args[0]);
    return 0;
}

int fn_getapplykey(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    *result = val_nil();
    if (gs_get_arg(args, nargs) && nargs >= 2 && args[1].type == VAL_NUM)
        gs_applykey(ctx, &args[0], (int)args[1].num);
    return 0;
}

int fn_getprevalidate(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    *result = val_logic(gs_get_arg(args, nargs) ? gs_prevalidate(ctx, &args[0]) : 1);
    return 0;
}

int fn_getpostvalidate(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    *result = val_logic(gs_get_arg(args, nargs) ? gs_postvalidate(ctx, &args[0]) : 1);
    return 0;
}

int fn_readvar(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_str(screen_read_var());
    return 0;
}

int fn_updated(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_logic(screen_read_updated());
    return 0;
}

static int flag_fn(int *flag, value_t *args, int nargs, value_t *result) {
    *result = val_logic(*flag);
    if (nargs >= 1 && args[0].type == VAL_LOGIC) *flag = args[0].logic;
    return 0;
}

int fn_readexit(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    return flag_fn(&read_exit_flag, args, nargs, result);
}

int fn_readinsert(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    return flag_fn(&read_insert_flag, args, nargs, result);
}

int fn_readkill(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    return flag_fn(&read_kill_flag, args, nargs, result);
}

int fn_getactive(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = screen_active_get();
    return 0;
}

/* ---- the screen functions ---- */

int fn_setpos(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    screen_setpos(arg_num(args, nargs, 0, 0), arg_num(args, nargs, 1, 0));
    *result = val_nil();
    return 0;
}

int fn_dispout(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    char buf[512];
    (void)ctx;
    if (nargs >= 1) {
        if (args[0].type == VAL_NUM) val_to_display(&args[0], buf, sizeof(buf));
        else val_to_string(&args[0], buf, sizeof(buf));
        screen_out(buf);
    }
    *result = val_nil();
    return 0;
}

int fn_maxrow(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = vnum(24);
    return 0;
}

int fn_maxcol(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = vnum(79);
    return 0;
}

int fn_setcolor(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    *result = val_str(screen_color_spec());
    if (nargs >= 1 && args[0].type == VAL_CHAR) screen_set_color(args[0].str);
    return 0;
}

int fn_setcursor(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    char line[32];
    (void)ctx;
    *result = vnum(cmd_get_set_opts()->cursor);
    if (nargs >= 1 && args[0].type == VAL_NUM) {   /* the shape, kept with SET CURSOR's switch */
        snprintf(line, sizeof(line), "SET CURSOR %s", args[0].num != 0 ? "ON" : "OFF");
        cmd_execute(NULL, line);
        ((set_options_t *)cmd_get_set_opts())->cursor = (int)args[0].num;
    }
    return 0;
}

/* SCROLL([top], [left], [bottom], [right], [n]): the region cleared (the
 * rows a scroll would vacate are not kept apart) */
int fn_scroll(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    screen_clear_region(arg_num(args, nargs, 0, 0), arg_num(args, nargs, 1, 0),
                        arg_num(args, nargs, 2, 24), arg_num(args, nargs, 3, 79));
    *result = val_nil();
    return 0;
}

/* DISPBOX(top, left, bottom, right, [frame]): frame 2 or a double-line
 * frame string draws double */
int fn_dispbox(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    int dbl = 0;
    (void)ctx;
    if (nargs >= 5) {
        if (args[4].type == VAL_NUM) dbl = args[4].num == 2;
        else if (args[4].type == VAL_CHAR) dbl = (unsigned char)args[4].str[0] == 0xC9 || strstr(args[4].str, "\xe2\x95\x94") == args[4].str;
    }
    screen_box(arg_num(args, nargs, 0, 0), arg_num(args, nargs, 1, 0),
               arg_num(args, nargs, 2, 0), arg_num(args, nargs, 3, 0), dbl);
    *result = val_nil();
    return 0;
}

/* SAVESCREEN / RESTSCREEN: no screen memory to copy; the string is empty
 * and restoring it paints nothing */
/* SAVESCREEN([t, l, b, r]) --> the region's cells, as a handle string;
 * RESTSCREEN([t, l, b, r], saved) paints them back.  The whole screen
 * when the corners are omitted. */
int fn_savescreen(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    *result = screen_save_region(arg_num(args, nargs, 0, 0), arg_num(args, nargs, 1, 0),
                                 arg_num(args, nargs, 2, 24), arg_num(args, nargs, 3, 79));
    return 0;
}

int fn_restscreen(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs >= 5)
        screen_restore_region(arg_num(args, nargs, 0, 0), arg_num(args, nargs, 1, 0),
                              arg_num(args, nargs, 2, 24), arg_num(args, nargs, 3, 79), &args[4]);
    else if (nargs >= 1)
        screen_restore_region(0, 0, 24, 79, &args[nargs - 1]);
    *result = val_nil();
    return 0;
}

/* SETKEY(key, [block]): the handler for a key during a wait state;
 * returns the one before; NIL (or none) removes it */
int fn_setkey(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_NUM) { *result = val_nil(); return 0; }
    *result = screen_set_key_block((int)args[0].num, nargs >= 2 && args[1].type == VAL_BLOCK ? &args[1] : NULL);
    return 0;
}

static int out_values(FILE *f, value_t *args, int nargs) {
    int i;
    char buf[512];
    for (i = 0; i < nargs; i++) {
        if (args[i].type == VAL_NUM) val_to_display(&args[i], buf, sizeof(buf));
        else val_to_string(&args[i], buf, sizeof(buf));
        fprintf(f, "%s%s", i ? " " : "", buf);
    }
    return 0;
}

int fn_outstd(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    out_values(stdout, args, nargs);
    *result = val_nil();
    return 0;
}

int fn_outerr(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    out_values(stderr, args, nargs);
    *result = val_nil();
    return 0;
}

int fn_noop_nil(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_nil();
    return 0;
}

int fn_iscolor(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_logic(screen_term_available());
    return 0;
}

/* ---- the database functions TBrowseDB's blocks use ---- */

int fn_dbgotop(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    cmd_api_go("TOP");
    *result = val_nil();
    return 0;
}

int fn_dbgobottom(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    cmd_api_go("BOTTOM");
    *result = val_nil();
    return 0;
}

int fn_dbskip(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    char n[32];
    (void)ctx;
    snprintf(n, sizeof(n), "%d", arg_num(args, nargs, 0, 1));
    cmd_api_skip(n);
    *result = val_nil();
    return 0;
}

static int at_end(expr_ctx_t *ctx, const char *fn) {
    value_t v;
    return func_call(ctx, fn, NULL, 0, &v) == 0 && v.type == VAL_LOGIC && v.logic;
}

/* DBSKIPPER(n): skips n records, fewer at the ends; returns the count
 * (the skip block TBrowseDB installs, as Clipper's sample does) */
int fn_dbskipper(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    int n = arg_num(args, nargs, 0, 1), i = 0;
    if (n > 0) {
        while (i < n && !at_end(ctx, "EOF")) {
            cmd_api_skip("1");
            if (at_end(ctx, "EOF")) { cmd_api_skip("-1"); break; }
            i++;
        }
    } else {
        while (i > n && !at_end(ctx, "BOF")) {
            cmd_api_skip("-1");
            if (at_end(ctx, "BOF")) break;
            i--;
        }
    }
    *result = vnum(i);
    return 0;
}

/* ---- the TBColumn and TBrowse classes ---- */

static object_t *tbcolumn_new(const char *heading, const value_t *block) {
    object_t *o = object_new("TBCOLUMN");
    if (!o) return NULL;
    iv_set(o, "heading", val_str(heading ? heading : ""));
    iv_set(o, "footing", val_str(""));
    iv_set(o, "block", block ? *block : val_nil());
    iv_set(o, "width", val_nil());
    iv_set(o, "colSep", val_nil());
    iv_set(o, "headSep", val_nil());
    iv_set(o, "footSep", val_nil());
    iv_set(o, "picture", val_nil());
    iv_set(o, "cargo", val_nil());
    iv_set(o, "colorBlock", val_nil());
    iv_set(o, "defColor", val_nil());
    return o;
}

int fn_tbcolumnnew(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    object_t *o = tbcolumn_new(arg_str(args, nargs, 0, ""), nargs >= 2 && args[1].type == VAL_BLOCK ? &args[1] : NULL);
    if (!o) { ctx->error = "Out of memory"; return -1; }
    *result = val_object(o);
    return 0;
}

static array_t *tb_columns(object_t *o) {
    value_t *p = object_ivar(o, "_columns", 0);
    return p && p->type == VAL_ARRAY ? p->array : NULL;
}

static object_t *tb_column(object_t *o, int i) {
    array_t *a = tb_columns(o);
    if (!a || i < 1 || i > a->len || a->el[i - 1].type != VAL_OBJECT) return NULL;
    return a->el[i - 1].obj;
}

/* rowCount: the window's rows less the heading row (when a column has a
 * heading), the headSep row, the footing row and the footSep row */
static void tb_configure(object_t *o) {
    array_t *a = tb_columns(o);
    int i, heads = 0, foots = 0, rows;
    for (i = 0; a && i < a->len; i++) {
        object_t *c = a->el[i].type == VAL_OBJECT ? a->el[i].obj : NULL;
        if (c && iv_str(c, "heading")[0]) heads = 1;
        if (c && iv_str(c, "footing")[0]) foots = 1;
    }
    rows = (int)iv_num(o, "nBottom") - (int)iv_num(o, "nTop") + 1;
    rows -= heads + (heads && iv_str(o, "headSep")[0] ? 1 : 0);
    rows -= foots + (foots && iv_str(o, "footSep")[0] ? 1 : 0);
    iv_set(o, "rowCount", vnum(rows < 1 ? 1 : rows));
    iv_set(o, "colCount", vnum(a ? a->len : 0));
    iv_set(o, "stable", val_logic(0));
}

static object_t *tbrowse_new(int t, int l, int b, int r) {
    object_t *o = object_new("TBROWSE");
    value_t cols;
    if (!o) return NULL;
    iv_set(o, "nTop", vnum(t));
    iv_set(o, "nLeft", vnum(l));
    iv_set(o, "nBottom", vnum(b));
    iv_set(o, "nRight", vnum(r));
    iv_set(o, "goTopBlock", val_nil());
    iv_set(o, "goBottomBlock", val_nil());
    iv_set(o, "skipBlock", val_nil());
    iv_set(o, "headSep", val_str(""));
    iv_set(o, "colSep", val_str(" "));
    iv_set(o, "footSep", val_str(""));
    iv_set(o, "autoLite", val_logic(1));
    iv_set(o, "cargo", val_nil());
    iv_set(o, "colorSpec", val_str(screen_color_spec()));
    iv_set(o, "freeze", vnum(0));
    iv_set(o, "leftVisible", vnum(0));    /* set by the layout (tb_set_left) */
    iv_set(o, "rightVisible", vnum(0));
    iv_set(o, "colPos", vnum(1));
    iv_set(o, "rowPos", vnum(1));
    iv_set(o, "hitTop", val_logic(0));
    iv_set(o, "hitBottom", val_logic(0));
    iv_set(o, "stable", val_logic(0));
    iv_set(o, "_req", vnum(0));
    iv_set(o, "_moved", vnum(0));
    if (ast_make_array(cmd_get_expr_ctx(), NULL, 0, &cols) != 0) cols = val_nil();
    iv_set(o, "_columns", cols);
    tb_configure(o);
    return o;
}

int fn_tbrowsenew(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    object_t *o = tbrowse_new(arg_num(args, nargs, 0, 0), arg_num(args, nargs, 1, 0),
                              arg_num(args, nargs, 2, 24), arg_num(args, nargs, 3, 79));
    if (!o) { ctx->error = "Out of memory"; return -1; }
    *result = val_object(o);
    return 0;
}

/* TBROWSEDB: the navigation blocks on the current work area */
int fn_tbrowsedb(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    if (fn_tbrowsenew(ctx, args, nargs, result) != 0) return -1;
    iv_set(result->obj, "goTopBlock", block_from_text(ctx, "{|| DBGOTOP()}"));
    iv_set(result->obj, "goBottomBlock", block_from_text(ctx, "{|| DBGOBOTTOM()}"));
    iv_set(result->obj, "skipBlock", block_from_text(ctx, "{|n| DBSKIPPER(n)}"));
    return 0;
}

/* The skip block's answer: how many rows the source moved */
static int tb_skip(expr_ctx_t *ctx, object_t *o, int n, int *moved) {
    value_t a = vnum(n), r;
    *moved = 0;
    if (n == 0) return 0;
    if (eval_block(ctx, o, "skipBlock", &a, 1, &r) != 0) return -1;
    *moved = r.type == VAL_NUM ? (int)r.num : 0;
    return 0;
}

/* A movement: Clipper defers them to stabilize(), so hitTop / hitBottom
 * answer for the movement requested since the last stabilize as a whole
 * (three up() from row 3 move two rows and hit nothing).  The rows are
 * skipped at once here; the requests and the rows moved are summed and
 * the flags settled at stabilize(). */
static void tb_moved(object_t *o, int requested, int moved) {
    iv_set(o, "_req", vnum(iv_num(o, "_req") + requested));
    iv_set(o, "_moved", vnum(iv_num(o, "_moved") + moved));
    iv_set(o, "hitTop", val_logic(requested < 0 && moved == 0));
    iv_set(o, "hitBottom", val_logic(requested > 0 && moved == 0));
    iv_set(o, "stable", val_logic(0));
}

static void tb_settle_hits(object_t *o) {
    int req = (int)iv_num(o, "_req"), moved = (int)iv_num(o, "_moved");
    iv_set(o, "hitTop", val_logic(req < 0 && moved == 0));
    iv_set(o, "hitBottom", val_logic(req > 0 && moved == 0));
    iv_set(o, "_req", vnum(0));
    iv_set(o, "_moved", vnum(0));
}

static int m_tb_addcolumn(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    value_t a[2], r;
    value_t *cols = object_ivar(o, "_columns", 0);
    (void)self;
    if (nargs < 1 || args[0].type != VAL_OBJECT) { ctx->error = "addColumn requires a TBColumn"; return -1; }
    a[0] = *cols;
    a[1] = args[0];
    if (func_call(ctx, "AADD", a, 2, &r) != 0) return -1;
    tb_configure(o);
    *result = vnum(tb_columns(o)->len);
    return 0;
}

static int m_tb_inscolumn(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    array_t *a = tb_columns(o);
    int n = arg_num(args, nargs, 0, 0), i;
    value_t grow[2], r;
    (void)self;
    if (!a || nargs < 2 || args[1].type != VAL_OBJECT || n < 1 || n > a->len + 1) {
        ctx->error = "insColumn requires (position, TBColumn)";
        return -1;
    }
    grow[0] = *object_ivar(o, "_columns", 0);
    grow[1] = val_nil();
    if (func_call(ctx, "AADD", grow, 2, &r) != 0) return -1;
    for (i = a->len - 1; i >= n; i--) a->el[i] = a->el[i - 1];
    a->el[n - 1] = args[1];
    tb_configure(o);
    *result = args[1];
    return 0;
}

static int m_tb_delcolumn(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    array_t *a = tb_columns(o);
    int n = arg_num(args, nargs, 0, 0), i;
    (void)self;
    if (!a || n < 1 || n > a->len) { ctx->error = "delColumn: no such column"; return -1; }
    *result = a->el[n - 1];
    for (i = n - 1; i < a->len - 1; i++) a->el[i] = a->el[i + 1];
    a->len--;
    tb_configure(o);
    if (iv_num(o, "colPos") > a->len) iv_set(o, "colPos", vnum(a->len > 0 ? a->len : 1));
    return 0;
}

static int m_tb_getcolumn(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    object_t *c = tb_column(o, arg_num(args, nargs, 0, 0));
    (void)self;
    if (!c) { ctx->error = "getColumn: no such column"; return -1; }
    *result = val_object(c);
    return 0;
}

static int m_tb_setcolumn(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    array_t *a = tb_columns(o);
    int n = arg_num(args, nargs, 0, 0);
    (void)self;
    if (!a || n < 1 || n > a->len || nargs < 2 || args[1].type != VAL_OBJECT) {
        ctx->error = "setColumn requires (position, TBColumn)";
        return -1;
    }
    *result = a->el[n - 1];
    a->el[n - 1] = args[1];
    tb_configure(o);
    return 0;
}

static int m_tb_gotop(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    value_t r;
    (void)args; (void)nargs;
    if (eval_block(ctx, o, "goTopBlock", NULL, 0, &r) != 0) return -1;
    iv_set(o, "rowPos", vnum(1));
    tb_moved(o, 0, 0);
    *result = *self;
    return 0;
}

static int m_tb_gobottom(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    value_t r;
    int rows = (int)iv_num(o, "rowCount"), back, fwd;
    (void)args; (void)nargs;
    if (eval_block(ctx, o, "goBottomBlock", NULL, 0, &r) != 0) return -1;
    /* the last row at the bottom of the window, or as far down as the rows go */
    if (tb_skip(ctx, o, -(rows - 1), &back) != 0) return -1;
    if (tb_skip(ctx, o, -back, &fwd) != 0) return -1;
    iv_set(o, "rowPos", vnum(-back + 1));
    tb_moved(o, 0, 0);
    *result = *self;
    return 0;
}

static int m_tb_up(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    int moved;
    (void)args; (void)nargs;
    if (tb_skip(ctx, o, -1, &moved) != 0) return -1;
    tb_moved(o, -1, moved);
    if (moved != 0 && iv_num(o, "rowPos") > 1) iv_set(o, "rowPos", vnum(iv_num(o, "rowPos") - 1));
    *result = *self;
    return 0;
}

static int m_tb_down(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    int moved;
    (void)args; (void)nargs;
    if (tb_skip(ctx, o, 1, &moved) != 0) return -1;
    tb_moved(o, 1, moved);
    if (moved != 0 && iv_num(o, "rowPos") < iv_num(o, "rowCount")) iv_set(o, "rowPos", vnum(iv_num(o, "rowPos") + 1));
    *result = *self;
    return 0;
}

static int m_tb_pageup(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    int rows = (int)iv_num(o, "rowCount"), moved, pos;
    (void)args; (void)nargs;
    if (tb_skip(ctx, o, -rows, &moved) != 0) return -1;
    tb_moved(o, -rows, moved);
    if (moved != 0) {
        pos = (int)iv_num(o, "rowPos") - (rows + moved);   /* the rows short of a page */
        iv_set(o, "rowPos", vnum(pos < 1 ? 1 : pos));
    }
    *result = *self;
    return 0;
}

static int m_tb_pagedown(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    int rows = (int)iv_num(o, "rowCount"), moved, pos;
    (void)args; (void)nargs;
    if (tb_skip(ctx, o, rows, &moved) != 0) return -1;
    tb_moved(o, rows, moved);
    if (moved != 0) {
        pos = (int)iv_num(o, "rowPos") + (rows - moved);
        iv_set(o, "rowPos", vnum(pos > rows ? rows : pos));
    }
    *result = *self;
    return 0;
}








static int m_tb_invalidate(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    iv_set(o, "stable", val_logic(0));
    *result = *self;
    return 0;
}

static int m_tb_configure(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    tb_configure(o);
    *result = *self;
    return 0;
}

static int m_tb_self(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)o; (void)args; (void)nargs;
    *result = *self;
    return 0;
}

/* A cell's text: the column's block, through its picture when it has
 * one; numbers in their display width */
static int tb_cell(expr_ctx_t *ctx, object_t *c, char *out, int size) {
    value_t v;
    value_t *pic = object_ivar(c, "picture", 0);
    if (eval_block(ctx, c, "block", NULL, 0, &v) != 0) return -1;
    if (pic && pic->type == VAL_CHAR && pic->str[0]) {
        value_t a[2], r;
        a[0] = v;
        a[1] = *pic;
        if (func_call(ctx, "TRANSFORM", a, 2, &r) != 0) return -1;
        snprintf(out, size, "%s", r.type == VAL_CHAR ? r.str : "");
    } else if (v.type == VAL_NUM) {
        val_to_display(&v, out, size);
    } else {
        val_to_string(&v, out, size);
    }
    return 0;
}

static void pad_to(char *s, int width, int size) {
    int n = (int)strlen(s);
    if (n > width) s[width] = '\0';
    while (n < width && n < size - 1) s[n++] = ' ';
    if (n < size) s[n] = '\0';
}

static void center_to(char *s, int width, int size) {
    char tmp[256];
    int n = (int)strlen(s), lead = (width - n) / 2, i;
    if (n >= width) { pad_to(s, width, size); return; }
    for (i = 0; i < lead; i++) tmp[i] = ' ';
    snprintf(tmp + lead, sizeof(tmp) - (size_t)lead, "%s", s);
    pad_to(tmp, width, (int)sizeof(tmp));
    snprintf(s, (size_t)size, "%s", tmp);
}

/* stabilize(): the movement settled (hitTop / hitBottom), then the window
 * painted -- heading, separator, the rows around the current one, the
 * source left where it was; .T. when done.  Without a terminal nothing is
 * painted (a browse repainted into a stream of lines says nothing, and
 * Clipper's own stream output drops positioned text); the source is
 * still placed and the row position settled. */
/* The columns' widths: a column's own, or the longer of its heading and
 * its current cell; heads says whether any column has a heading */
static int tb_widths(expr_ctx_t *ctx, object_t *o, int *width, int ncol, int *heads) {
    array_t *cols = tb_columns(o);
    char cell[256];
    int i;
    *heads = 0;
    for (i = 0; i < ncol; i++) {
        object_t *c = cols->el[i].type == VAL_OBJECT ? cols->el[i].obj : NULL;
        value_t *w = c ? object_ivar(c, "width", 0) : NULL;
        width[i] = 0;
        if (!c) continue;
        if (w && w->type == VAL_NUM) {
            width[i] = (int)w->num;
        } else {
            if (tb_cell(ctx, c, cell, sizeof(cell)) != 0) return -1;
            width[i] = (int)strlen(cell);
            if ((int)strlen(iv_str(c, "heading")) > width[i]) width[i] = (int)strlen(iv_str(c, "heading"));
        }
        if (iv_str(c, "heading")[0]) *heads = 1;
    }
    return 0;
}

/* ---- Columns on the screen ----
 * The frozen columns (1 .. freeze) always, then from leftVisible as many
 * as fit the window with the separators between them; rightVisible is
 * the last of those.  The column moves keep colPos among them, panning
 * as Harbour's TBrowse does (and Clipper's). */
static int tb_freeze(object_t *o, int ncol) {
    int f = (int)iv_num(o, "freeze");
    if (f < 0) f = 0;
    if (f > ncol) f = ncol;
    return f;
}

/* the last column that fits when lv is the first scrolling one */
static int tb_right_from(object_t *o, const int *width, int ncol, int lv) {
    int avail = (int)iv_num(o, "nRight") - (int)iv_num(o, "nLeft") + 1;
    int sep = (int)strlen(iv_str(o, "colSep")), used = 0, nvis = 0, i, rv = lv;
    int freeze = tb_freeze(o, ncol);
    for (i = 0; i < freeze; i++) { used += (nvis ? sep : 0) + width[i]; nvis++; }
    for (i = lv; i <= ncol; i++) {
        int add = (nvis ? sep : 0) + width[i - 1];
        if (used + add > avail && i > lv) break;
        used += add;
        nvis++;
        rv = i;
    }
    return rv;
}

static void tb_set_left(object_t *o, const int *width, int ncol, int lv) {
    int freeze = tb_freeze(o, ncol);
    if (freeze >= ncol) {   /* all frozen: nothing scrolls */
        iv_set(o, "leftVisible", vnum(ncol));
        iv_set(o, "rightVisible", vnum(ncol));
        return;
    }
    if (lv < freeze + 1) lv = freeze + 1;
    if (lv > ncol) lv = ncol;
    iv_set(o, "leftVisible", vnum(lv));
    iv_set(o, "rightVisible", vnum(tb_right_from(o, width, ncol, lv)));
}

/* colPos on the screen: a scrolling column left of the window brings
 * the window to it, one right of it moves the window on until it fits */
static void tb_show_col(object_t *o, const int *width, int ncol) {
    int c = (int)iv_num(o, "colPos"), guard = 0;
    if (c <= tb_freeze(o, ncol)) return;
    if (c < (int)iv_num(o, "leftVisible")) tb_set_left(o, width, ncol, c);
    while (c > (int)iv_num(o, "rightVisible") && guard++ < 64)
        tb_set_left(o, width, ncol, (int)iv_num(o, "leftVisible") + 1);
}

/* the widths, and the layout made good for them (a frozen count or a
 * column changed since): 0, or -1 on an error */
static int tb_layout(expr_ctx_t *ctx, object_t *o, int *width, int *ncol, int *heads) {
    array_t *cols = tb_columns(o);
    int n = cols ? cols->len : 0;
    if (n > 64) n = 64;
    *ncol = n;
    if (n == 0) return 0;
    if (tb_widths(ctx, o, width, n, heads) != 0) return -1;
    tb_set_left(o, width, n, (int)iv_num(o, "leftVisible"));
    tb_show_col(o, width, n);
    return 0;
}

static int tb_col_method(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *result, int which) {
    int width[64], ncol, heads, c, lv, rv, freeze;
    *result = *self;
    if (tb_layout(ctx, o, width, &ncol, &heads) != 0) return -1;
    if (ncol == 0) return 0;
    c = (int)iv_num(o, "colPos");
    lv = (int)iv_num(o, "leftVisible");
    rv = (int)iv_num(o, "rightVisible");
    freeze = tb_freeze(o, ncol);
    switch (which) {
    case 'L': if (c > 1) c--; break;                         /* left() */
    case 'R': if (c < ncol) c++; break;                      /* right() */
    case 'h': c = lv; break;                                 /* home(): the leftmost scrolling column on the screen */
    case 'e': c = rv; break;                                 /* end(): the rightmost */
    case 'H':                                                /* panHome() */
        c = 1;
        tb_set_left(o, width, ncol, freeze + 1);
        break;
    case 'E':                                                /* panEnd() */
        c = ncol;
        lv = ncol;
        while (lv - 1 >= freeze + 1 && tb_right_from(o, width, ncol, lv - 1) >= ncol) lv--;
        tb_set_left(o, width, ncol, lv);
        break;
    case '<':                                                /* panLeft() */
        if (lv > freeze + 1) tb_set_left(o, width, ncol, lv - 1);
        if (c > (int)iv_num(o, "rightVisible")) c = (int)iv_num(o, "rightVisible");
        break;
    case '>':                                                /* panRight() */
        if (rv < ncol) tb_set_left(o, width, ncol, lv + 1);
        if (c < (int)iv_num(o, "leftVisible")) c = (int)iv_num(o, "leftVisible");
        if (c > (int)iv_num(o, "rightVisible")) c = (int)iv_num(o, "rightVisible");
        break;
    }
    if (c < 1) c = 1;
    if (c > ncol) c = ncol;
    iv_set(o, "colPos", vnum(c));
    tb_show_col(o, width, ncol);
    iv_set(o, "stable", val_logic(0));
    return 0;
}

static int m_tb_left(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    (void)args; (void)nargs;
    return tb_col_method(ctx, o, self, result, 'L');
}
static int m_tb_right(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    (void)args; (void)nargs;
    return tb_col_method(ctx, o, self, result, 'R');
}
static int m_tb_home(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    (void)args; (void)nargs;
    return tb_col_method(ctx, o, self, result, 'h');
}
static int m_tb_end(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    (void)args; (void)nargs;
    return tb_col_method(ctx, o, self, result, 'e');
}
static int m_tb_panhome(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    (void)args; (void)nargs;
    return tb_col_method(ctx, o, self, result, 'H');
}
static int m_tb_panend(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    (void)args; (void)nargs;
    return tb_col_method(ctx, o, self, result, 'E');
}
static int m_tb_panleft(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    (void)args; (void)nargs;
    return tb_col_method(ctx, o, self, result, '<');
}
static int m_tb_panright(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    (void)args; (void)nargs;
    return tb_col_method(ctx, o, self, result, '>');
}

/* The columns on the screen, in order: the frozen ones, then
 * leftVisible .. rightVisible.  Returns how many. */
static int tb_visible(object_t *o, int ncol, int *vis) {
    int n = 0, i, freeze = tb_freeze(o, ncol);
    int lv = (int)iv_num(o, "leftVisible"), rv = (int)iv_num(o, "rightVisible");
    for (i = 1; i <= freeze; i++) vis[n++] = i;
    for (i = lv > freeze ? lv : freeze + 1; i <= rv && i <= ncol; i++) vis[n++] = i;
    return n;
}

/* The screen column of column col's cell (nLeft when it is not shown) */
static int tb_col_x(object_t *o, const int *width, int ncol, int col) {
    int vis[64], nvis = tb_visible(o, ncol, vis), x = (int)iv_num(o, "nLeft"), i;
    int seplen = (int)strlen(iv_str(o, "colSep"));
    for (i = 0; i < nvis; i++) {
        if (vis[i] == col) return x;
        x += width[vis[i] - 1] + seplen;
    }
    return (int)iv_num(o, "nLeft");
}

/* The screen row of the current row's cell */
static int tb_cell_y(object_t *o, int heads) {
    int y = (int)iv_num(o, "nTop") + (int)iv_num(o, "rowPos") - 1;
    if (heads) y += 1 + (iv_str(o, "headSep")[0] ? 1 : 0);
    return y;
}

/* The colour pair a cell is shown in: its column's colorBlock, given the
 * cell's value, answers {standard, enhanced} -- else the column's
 * defColor, else 1 and 2.  which: 1 standard, 2 enhanced. */
static int tb_cell_color(expr_ctx_t *ctx, object_t *c, int which) {
    value_t *cb = object_ivar(c, "colorBlock", 0), *dc = object_ivar(c, "defColor", 0), r, v;
    if (cb && cb->type == VAL_BLOCK && eval_block(ctx, c, "block", NULL, 0, &v) == 0 &&
        ast_eval_block(ctx, cb, &v, 1, &r) == 0 && r.type == VAL_ARRAY && r.array &&
        r.array->len >= which && r.array->el[which - 1].type == VAL_NUM)
        return (int)r.array->el[which - 1].num;
    ctx->error = NULL;
    if (dc && dc->type == VAL_ARRAY && dc->array && dc->array->len >= which &&
        dc->array->el[which - 1].type == VAL_NUM)
        return (int)dc->array->el[which - 1].num;
    return which;
}

/* hilite() / deHilite(): the current cell in the enhanced (or standard)
 * colour, the cursor on it -- DBU makes its editing GET at ROW(), COL()
 * after hilite() */
static int tb_hilite_cell(expr_ctx_t *ctx, object_t *o, int enhanced) {
    array_t *cols = tb_columns(o);
    int ncol, colpos = (int)iv_num(o, "colPos");
    int width[64], heads, x, y;
    char cell[256];
    if (!screen_term_available() || !cols || cols->len == 0) return 0;
    if (tb_layout(ctx, o, width, &ncol, &heads) != 0) return -1;
    if (colpos < 1 || colpos > ncol) return 0;
    if (tb_cell(ctx, cols->el[colpos - 1].obj, cell, sizeof(cell)) != 0) return -1;
    pad_to(cell, width[colpos - 1], sizeof(cell));
    x = tb_col_x(o, width, ncol, colpos);
    y = tb_cell_y(o, heads);
    screen_color_pair(tb_cell_color(ctx, cols->el[colpos - 1].obj, enhanced ? 2 : 1));
    screen_out_at(y, x, cell);
    screen_color_pair(1);
    screen_setpos(y, x);
    return 0;
}

static int m_tb_hilite(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    (void)args; (void)nargs;
    *result = *self;
    return tb_hilite_cell(ctx, o, 1);
}

static int m_tb_dehilite(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    (void)args; (void)nargs;
    *result = *self;
    return tb_hilite_cell(ctx, o, 0);
}

/* A separator row (headSep, footSep) across the window: its last
 * character repeats */
static void tb_sep_row(object_t *o, const char *ivar, int y) {
    const char *sep = iv_str(o, ivar);
    int left = (int)iv_num(o, "nLeft"), w = (int)iv_num(o, "nRight") - left + 1, n = (int)strlen(sep), i;
    char line[512];
    for (i = 0; i < w && i < (int)sizeof(line) - 1; i++) line[i] = sep[i < n ? i : n - 1];
    line[i] = '\0';
    screen_out_at(y, left, line);
}

/* A row of the window built from the visible columns: text(i) for column
 * i, padded or centred to its width, the colSep between; painted cell by
 * cell in colours[] when given */
static void tb_paint_row(object_t *o, int y, const int *vis, int nvis, const int *width,
                         char cells[][256], const int *colours) {
    int left = (int)iv_num(o, "nLeft"), right = (int)iv_num(o, "nRight");
    int x = left, i;
    const char *sep = iv_str(o, "colSep");
    for (i = 0; i < nvis; i++) {
        char cell[256];
        if (i) {
            screen_color_pair(1);
            screen_out_at(y, x, sep);
            x += (int)strlen(sep);
        }
        snprintf(cell, sizeof(cell), "%s", cells[i]);
        pad_to(cell, width[vis[i] - 1], sizeof(cell));
        if (x + (int)strlen(cell) > right + 1) cell[right + 1 - x > 0 ? right + 1 - x : 0] = '\0';
        screen_color_pair(colours ? colours[i] : 1);
        screen_out_at(y, x, cell);
        x += (int)strlen(cell);
    }
    screen_color_pair(1);
    if (x <= right) {   /* the rest of the row, blank */
        char blank[512];
        int n = right - x + 1;
        if (n > (int)sizeof(blank) - 1) n = (int)sizeof(blank) - 1;
        memset(blank, ' ', (size_t)n);
        blank[n] = '\0';
        screen_out_at(y, x, blank);
    }
}

static int m_tb_stabilize(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    array_t *cols = tb_columns(o);
    int ncol = 0;
    int rows = (int)iv_num(o, "rowCount"), rowpos = (int)iv_num(o, "rowPos");
    int top = (int)iv_num(o, "nTop");
    int width[64], heads = 0, foots = 0, i, r, moved, painted, y, vis[64], nvis, colours[64];
    static char cells[64][256];
    int paint = screen_term_available();
    (void)args; (void)nargs;
    *result = val_logic(1);
    if (iv_logic(o, "stable")) {
        /* nothing to paint, but the cursor goes back to the current cell
         * each time, as Clipper's does: DBU writes its status line and
         * calls stabilize() again before making a GET at ROW(), COL() */
        return tb_hilite_cell(ctx, o, 1);
    }
    tb_settle_hits(o);
    /* headings, footings and separators may have changed since the
     * columns were added: rowCount follows them (as configure() would) */
    tb_configure(o);
    rows = (int)iv_num(o, "rowCount");
    if (rowpos > rows) {
        rowpos = rows;
        iv_set(o, "rowPos", vnum(rowpos));
    }
    if (tb_layout(ctx, o, width, &ncol, &heads) != 0) return -1;
    nvis = tb_visible(o, ncol, vis);
    for (i = 0; i < ncol; i++) {
        object_t *c = cols->el[i].type == VAL_OBJECT ? cols->el[i].obj : NULL;
        if (c && iv_str(c, "footing")[0]) foots = 1;
    }

    y = top;
    if (heads) {
        for (i = 0; i < nvis; i++) {
            object_t *c = cols->el[vis[i] - 1].obj;
            snprintf(cells[i], sizeof(cells[i]), "%s", iv_str(c, "heading"));
            center_to(cells[i], width[vis[i] - 1], sizeof(cells[i]));
        }
        if (paint) tb_paint_row(o, y, vis, nvis, width, cells, NULL);
        y++;
        if (iv_str(o, "headSep")[0]) {
            if (paint) tb_sep_row(o, "headSep", y);
            y++;
        }
    }

    /* up to the window's first row, as far as the rows go */
    if (tb_skip(ctx, o, -(rowpos - 1), &moved) != 0) return -1;
    if (-moved < rowpos - 1) {
        rowpos = -moved + 1;
        iv_set(o, "rowPos", vnum(rowpos));
    }
    painted = 0;
    for (r = 0; r < rows; r++) {
        if (r > 0) {
            if (tb_skip(ctx, o, 1, &moved) != 0) return -1;
            if (moved == 0) break;
        }
        for (i = 0; i < nvis; i++) {
            object_t *c = cols->el[vis[i] - 1].obj;
            if (tb_cell(ctx, c, cells[i], sizeof(cells[i])) != 0) return -1;
            colours[i] = paint ? tb_cell_color(ctx, c, 1) : 1;
        }
        if (paint) tb_paint_row(o, y + r, vis, nvis, width, cells, colours);
        painted = r + 1;
    }
    for (; paint && r < rows; r++) {   /* the rows below the data, blank */
        for (i = 0; i < nvis; i++) cells[i][0] = '\0';
        tb_paint_row(o, y + r, vis, nvis, width, cells, NULL);
    }
    if (foots) {   /* the footSep row, then the footings */
        int fy = y + rows;
        if (iv_str(o, "footSep")[0]) {
            if (paint) tb_sep_row(o, "footSep", fy);
            fy++;
        }
        for (i = 0; i < nvis; i++) {
            object_t *c = cols->el[vis[i] - 1].obj;
            snprintf(cells[i], sizeof(cells[i]), "%s", iv_str(c, "footing"));
            center_to(cells[i], width[vis[i] - 1], sizeof(cells[i]));
        }
        if (paint) tb_paint_row(o, fy, vis, nvis, width, cells, NULL);
    }
    /* back to the current row, the cursor on the current cell */
    if (tb_skip(ctx, o, rowpos - painted, &moved) != 0) return -1;
    if (paint) screen_setpos(y + rowpos - 1, tb_col_x(o, width, ncol, (int)iv_num(o, "colPos")));
    iv_set(o, "stable", val_logic(1));
    *result = val_logic(1);
    (void)self;
    /* stable, the current cell shown highlighted unless autoLite is off */
    if (paint && iv_logic(o, "autoLite")) return tb_hilite_cell(ctx, o, 1);
    return 0;
}

static const method_t tbrowse_methods[] = {
    { "ADDCOLUMN", m_tb_addcolumn }, { "INSCOLUMN", m_tb_inscolumn }, { "DELCOLUMN", m_tb_delcolumn },
    { "GETCOLUMN", m_tb_getcolumn }, { "SETCOLUMN", m_tb_setcolumn },
    { "GOTOP", m_tb_gotop }, { "GOBOTTOM", m_tb_gobottom }, { "UP", m_tb_up }, { "DOWN", m_tb_down },
    { "PAGEUP", m_tb_pageup }, { "PAGEDOWN", m_tb_pagedown }, { "HOME", m_tb_home }, { "END", m_tb_end },
    { "LEFT", m_tb_left }, { "RIGHT", m_tb_right }, { "PANLEFT", m_tb_panleft }, { "PANRIGHT", m_tb_panright },
    { "PANHOME", m_tb_panhome }, { "PANEND", m_tb_panend },
    { "STABILIZE", m_tb_stabilize }, { "FORCESTABLE", m_tb_stabilize }, { "REFRESHALL", m_tb_invalidate }, { "REFRESHCURRENT", m_tb_invalidate },
    { "INVALIDATE", m_tb_invalidate }, { "CONFIGURE", m_tb_configure },
    { "HILITE", m_tb_hilite }, { "DEHILITE", m_tb_dehilite }, { "COLORRECT", m_tb_self },
    { NULL, NULL }
};

/* ---- dispatch ---- */

static const method_t *class_methods(const object_t *o) {
    if (str_icmp(o->cls, "GET") == 0) return get_methods;
    if (str_icmp(o->cls, "TBROWSE") == 0) return tbrowse_methods;
    return NULL;
}

static method_fn find_method(const object_t *o, const char *name) {
    const method_t *m = class_methods(o);
    for (; m && m->name; m++) {
        if (str_icmp(m->name, name) == 0) return m->fn;
    }
    return NULL;
}

static int m_classname(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)self; (void)args; (void)nargs;
    *result = val_str(o->cls);
    return 0;
}

int object_send(expr_ctx_t *ctx, const value_t *base, const char *name, value_t *args, int nargs,
                int is_call, value_t *result) {
    object_t *o;
    method_fn m;
    value_t *p;
    value_t self = *base;
    if (base->type != VAL_OBJECT || !base->obj) {
        ctx->error = "Not an object";
        return -1;
    }
    o = base->obj;
    m = str_icmp(name, "CLASSNAME") == 0 ? m_classname : find_method(o, name);
    p = object_ivar(o, name, 0);
    if (is_call && m) {
        int rc = m(ctx, o, &self, args, nargs, result);
        if (rc == 0 && str_icmp(o->cls, "GET") == 0 && iv_logic(o, "hasFocus") && screen_term_available()) {
            /* a focused Get keeps its field on the screen as it edits */
            value_t *b = object_ivar(o, "buffer", 0);
            int row = (int)iv_num(o, "row"), col = (int)iv_num(o, "col");
            if (b && b->type == VAL_CHAR) {
                int pos = (int)iv_num(o, "pos"), len = (int)strlen(b->str);
                screen_out_at(row, col, b->str);
                if (pos < 1) pos = 1;
                if (pos > len) pos = len;
                screen_setpos(row, col + pos - 1);
            }
        }
        return rc;
    }
    if (p) {
        *result = *p;
        return 0;
    }
    if (m) return m(ctx, o, &self, args, nargs, result);
    snprintf(ctx->err_msg, sizeof(ctx->err_msg), "No exported %s: %s", is_call ? "method" : "variable", name);
    ctx->error = ctx->err_msg;
    return -1;
}

/* ---- DBEDIT() ----
 * DBEDIT([t], [l], [b], [r], [acColumns], [cUserFunction], [pictures],
 * [headers], [headSeps], [colSeps], [footSeps], [footings]): a TBrowse
 * over the current work area, as Clipper's own later is.  The cursor keys
 * are DBEDIT's; with no user function Enter and Esc end it, with one every
 * other key is an exception the function answers (0 abort, 1 continue,
 * 2 repaint).  When no key is waiting the function is called once, with
 * DE_IDLE, DE_HITTOP, DE_HITBOTTOM or DE_EMPTY, before DBEDIT waits. */
static const char *dbedit_item(const value_t *arg, int i) {
    if (arg->type == VAL_CHAR) return val_text(arg);
    if (arg->type == VAL_ARRAY && arg->array && i < arg->array->len && arg->array->el[i].type == VAL_CHAR)
        return val_text(&arg->array->el[i]);
    return NULL;
}

static int dbedit_call(expr_ctx_t *ctx, const char *udf, int mode, object_t *o) {
    value_t a[2], r;
    a[0] = vnum(mode);
    a[1] = vnum(iv_num(o, "colPos"));
    if (func_call(ctx, udf, a, 2, &r) != 0) { ctx->error = NULL; return 0; }
    return r.type == VAL_NUM ? (int)r.num : 1;
}

static int dbedit_send(expr_ctx_t *ctx, value_t *tb, const char *msg) {
    value_t r;
    if (object_send(ctx, tb, msg, NULL, 0, 1, &r) != 0) { ctx->error = NULL; return -1; }
    return 0;
}

int fn_dbedit(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    value_t tb, nil = val_nil();
    const value_t *cols = nargs > 4 ? &args[4] : &nil;
    const char *udf = nargs > 5 && args[5].type == VAL_CHAR && val_text(&args[5])[0] ? val_text(&args[5]) : NULL;
    const value_t *pics = nargs > 6 ? &args[6] : &nil;
    const value_t *heads = nargs > 7 ? &args[7] : &nil;
    const value_t *hseps = nargs > 8 ? &args[8] : &nil;
    const value_t *cseps = nargs > 9 ? &args[9] : &nil;
    value_t tbargs[4];
    char exprs[64][128];
    int ncols = 0, i;
    dbf_t *db = ctx->db;
    char uname[MEMVAR_NAMELEN];
    *result = val_nil();
    if (!db || !dbf_is_open(db)) return 0;
    /* the columns: the array's expressions, or every field */
    if (cols->type == VAL_ARRAY && cols->array) {
        for (i = 0; i < cols->array->len && ncols < 64; i++)
            if (cols->array->el[i].type == VAL_CHAR) str_copy(exprs[ncols++], val_text(&cols->array->el[i]), 128);
    } else {
        for (i = 0; i < db->field_count && ncols < 64; i++) str_copy(exprs[ncols++], db->fields[i].name, 128);
    }
    tbargs[0] = vnum(arg_num(args, nargs, 0, 0));
    tbargs[1] = vnum(arg_num(args, nargs, 1, 0));
    {
        value_t mr, mc;
        fn_maxrow(ctx, NULL, 0, &mr);
        fn_maxcol(ctx, NULL, 0, &mc);
        tbargs[2] = vnum(arg_num(args, nargs, 2, (int)mr.num));
        tbargs[3] = vnum(arg_num(args, nargs, 3, (int)mc.num));
    }
    if (fn_tbrowsedb(ctx, tbargs, 4, &tb) != 0) return -1;
    gc_push_roots(&tb, 1);
    {
        const char *hs = dbedit_item(hseps, 0), *cs = dbedit_item(cseps, 0);
        char dflt_h[4] = { (char)0xCD, (char)0xD1, (char)0xCD, 0 };   /* the double line, a single-line junction */
        char dflt_c[4] = { ' ', (char)0xB3, ' ', 0 };
        iv_set(tb.obj, "headSep", val_str(hs ? hs : dflt_h));
        iv_set(tb.obj, "colSep", val_str(cs ? cs : dflt_c));
    }
    for (i = 0; i < ncols; i++) {
        char text[200];
        value_t block, col, r;
        const char *h = dbedit_item(heads, i), *p = dbedit_item(pics, i);
        object_t *c;
        snprintf(text, sizeof(text), "{|| %s}", exprs[i]);
        block = block_from_text(ctx, text);
        c = tbcolumn_new(h ? h : exprs[i], block.type == VAL_BLOCK ? &block : NULL);
        if (!c) continue;
        if (p) iv_set(c, "picture", val_str(p));
        col = val_object(c);
        object_send(ctx, &tb, "ADDCOLUMN", &col, 1, 1, &r);
        ctx->error = NULL;
    }
    str_copy(uname, udf ? udf : "", sizeof(uname));
    for (;;) {
        int key, rc;
        dbedit_send(ctx, &tb, "FORCESTABLE");
        if (!screen_key_pending()) {
            if (udf) {
                int mode = db->record_count == 0 ? 3
                         : iv_logic(tb.obj, "hitTop") ? 1
                         : iv_logic(tb.obj, "hitBottom") ? 2 : 0;
                iv_set(tb.obj, "hitTop", val_logic(0));
                iv_set(tb.obj, "hitBottom", val_logic(0));
                rc = dbedit_call(ctx, uname, mode, tb.obj);
                if (rc == 0) break;
                if (rc == 2) { dbedit_send(ctx, &tb, "REFRESHALL"); continue; }
                dbedit_send(ctx, &tb, "FORCESTABLE");   /* the function may have moved the record or painted over */
            }
            key = screen_inkey(-1.0);
        } else {
            key = screen_inkey(0.0);
        }
        if (key == 0) continue;   /* a SET KEY handler took it */
        switch (key) {
        case K_UP: dbedit_send(ctx, &tb, "UP"); continue;
        case K_DOWN: dbedit_send(ctx, &tb, "DOWN"); continue;
        case K_LEFT: dbedit_send(ctx, &tb, "LEFT"); continue;
        case K_RIGHT: dbedit_send(ctx, &tb, "RIGHT"); continue;
        case K_CTRL_LEFT: dbedit_send(ctx, &tb, "PANLEFT"); continue;
        case K_CTRL_RIGHT: dbedit_send(ctx, &tb, "PANRIGHT"); continue;
        case K_HOME: dbedit_send(ctx, &tb, "HOME"); continue;
        case K_END: dbedit_send(ctx, &tb, "END"); continue;
        case K_CTRL_HOME: dbedit_send(ctx, &tb, "PANHOME"); continue;
        case K_CTRL_END: dbedit_send(ctx, &tb, "PANEND"); continue;
        case K_PGUP: dbedit_send(ctx, &tb, "PAGEUP"); continue;
        case K_PGDN: dbedit_send(ctx, &tb, "PAGEDOWN"); continue;
        case K_CTRL_PGUP: dbedit_send(ctx, &tb, "GOTOP"); continue;
        case K_CTRL_PGDN: dbedit_send(ctx, &tb, "GOBOTTOM"); continue;
        default: break;
        }
        if (!udf) {
            if (key == K_ENTER || key == K_ESC) break;
            continue;
        }
        rc = dbedit_call(ctx, uname, 4, tb.obj);
        if (rc == 0) break;
        if (rc == 2) dbedit_send(ctx, &tb, "REFRESHALL");
    }
    gc_pop_roots();
    return 0;
}
