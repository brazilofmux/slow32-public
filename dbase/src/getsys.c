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
        return val_num_dec(atof(buf), seen ? d : (strchr(buf, '.') ? (int)strlen(strchr(buf, '.') + 1) : 0), 0);
    }
    case 'D':
        a = val_str(buf);
        if (func_call(ctx, "CTOD", &a, 1, &v) == 0) {
            iv_set(o, "badDate", val_logic(v.type != VAL_DATE || v.date == 0));
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
    iv_set(o, "pos", vnum(1));
    *result = *self;
    return 0;
}

static int m_get_killfocus(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    iv_set(o, "hasFocus", val_logic(0));
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
    iv_set(o, "changed", val_logic(0));
    iv_set(o, "rejected", val_logic(0));
    iv_set(o, "typeOut", val_logic(0));
    iv_set(o, "pos", vnum(1));
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
        if (pos > len) {
            iv_set(o, "typeOut", val_logic(1));
        } else {
            buf[pos - 1] = edit_char(o, args[0].str);
            iv_set(o, "pos", vnum(pos + 1));
            iv_set(o, "changed", val_logic(1));
            if (pos + 1 > len) iv_set(o, "typeOut", val_logic(1));
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
        if (pos > len) {
            iv_set(o, "typeOut", val_logic(1));
        } else {
            memmove(buf + pos, buf + pos - 1, (size_t)(len - pos));
            buf[pos - 1] = edit_char(o, args[0].str);
            iv_set(o, "pos", vnum(pos + 1));
            iv_set(o, "changed", val_logic(1));
            if (pos + 1 > len) iv_set(o, "typeOut", val_logic(1));
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

static const method_t get_methods[] = {
    { "VARGET", m_get_vargetm }, { "VARPUT", m_get_varput }, { "DISPLAY", m_get_display },
    { "SETFOCUS", m_get_setfocus }, { "KILLFOCUS", m_get_killfocus }, { "ASSIGN", m_get_assign },
    { "RESET", m_get_reset }, { "UNDO", m_get_undo }, { "UPDATEBUFFER", m_get_updatebuffer },
    { "UNTRANSFORM", m_get_untransform }, { "OVERSTRIKE", m_get_overstrike }, { "INSERT", m_get_insert },
    { "DELETE", m_get_delete }, { "BACKSPACE", m_get_backspace }, { "HOME", m_get_home },
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
    /* GETLIST, when the program keeps one, collects the Gets as Clipper's does */
    if (memvar_find(store, "GETLIST", &list) == 0 && list.type == VAL_ARRAY && list.array) {
        value_t a[2], r;
        a[0] = list;
        a[1] = v;
        func_call(ctx, "AADD", a, 2, &r);
    }
    return v;
}

/* ---- READ around the Gets ---- */

static int read_exit_flag, read_insert_flag = 0, read_kill_flag;

int fn_readmodal(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    screen_read();   /* the GETs recorded so far, as READ runs them */
    *result = val_logic(screen_read_updated());
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
int fn_savescreen(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_str("");
    return 0;
}

int fn_restscreen(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
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

static int m_tb_home(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    int moved;
    (void)args; (void)nargs;
    if (tb_skip(ctx, o, -((int)iv_num(o, "rowPos") - 1), &moved) != 0) return -1;
    iv_set(o, "rowPos", vnum(iv_num(o, "rowPos") + moved));
    tb_moved(o, 0, 0);
    *result = *self;
    return 0;
}

static int m_tb_end(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    int moved;
    (void)args; (void)nargs;
    if (tb_skip(ctx, o, (int)iv_num(o, "rowCount") - (int)iv_num(o, "rowPos"), &moved) != 0) return -1;
    iv_set(o, "rowPos", vnum(iv_num(o, "rowPos") + moved));
    tb_moved(o, 0, 0);
    *result = *self;
    return 0;
}

static int tb_colmove(object_t *o, int to, value_t *self, value_t *result) {
    int n = (int)iv_num(o, "colCount");
    if (to < 1) to = 1;
    if (to > n) to = n > 0 ? n : 1;
    iv_set(o, "colPos", vnum(to));
    iv_set(o, "stable", val_logic(0));
    *result = *self;
    return 0;
}

static int m_tb_left(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    return tb_colmove(o, (int)iv_num(o, "colPos") - 1, self, result);
}

static int m_tb_right(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    return tb_colmove(o, (int)iv_num(o, "colPos") + 1, self, result);
}

static int m_tb_panhome(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    return tb_colmove(o, 1, self, result);
}

static int m_tb_panend(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    return tb_colmove(o, (int)iv_num(o, "colCount"), self, result);
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
static int m_tb_stabilize(expr_ctx_t *ctx, object_t *o, value_t *self, value_t *args, int nargs, value_t *result) {
    array_t *cols = tb_columns(o);
    int ncol = cols ? cols->len : 0;
    int rows = (int)iv_num(o, "rowCount"), rowpos = (int)iv_num(o, "rowPos");
    int top = (int)iv_num(o, "nTop"), left = (int)iv_num(o, "nLeft"), right = (int)iv_num(o, "nRight");
    int width[64], heads = 0, i, r, moved, painted, y;
    char cell[256], line[512];
    int paint = screen_term_available();
    (void)args; (void)nargs;
    *result = val_logic(1);
    if (iv_logic(o, "stable")) return 0;
    tb_settle_hits(o);
    if (ncol > 64) ncol = 64;

    /* widths: the column's, or the longer of heading and current cell */
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
        if (iv_str(c, "heading")[0]) heads = 1;
    }

    y = top;
    if (heads) {
        line[0] = '\0';
        for (i = 0; i < ncol; i++) {
            object_t *c = cols->el[i].obj;
            snprintf(cell, sizeof(cell), "%s", iv_str(c, "heading"));
            center_to(cell, width[i], sizeof(cell));
            if (i) strncat(line, iv_str(o, "colSep"), sizeof(line) - strlen(line) - 1);
            strncat(line, cell, sizeof(line) - strlen(line) - 1);
        }
        pad_to(line, right - left + 1, sizeof(line));
        if (paint) screen_out_at(y, left, line);
        y++;
        if (iv_str(o, "headSep")[0]) {
            const char *sep = iv_str(o, "headSep");
            int n = (int)strlen(sep), k = 0;
            for (i = 0; i < right - left + 1 && i < (int)sizeof(line) - 1; i++) {
                line[i] = sep[n > 1 ? (k < n - 1 ? k : n - 1) : 0];
                if (n > 1) k = 0;   /* the last char repeats across the row */
            }
            line[i] = '\0';
            if (paint) screen_out_at(y, left, line);
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
        line[0] = '\0';
        for (i = 0; i < ncol; i++) {
            if (tb_cell(ctx, cols->el[i].obj, cell, sizeof(cell)) != 0) return -1;
            pad_to(cell, width[i], sizeof(cell));
            if (i) strncat(line, iv_str(o, "colSep"), sizeof(line) - strlen(line) - 1);
            strncat(line, cell, sizeof(line) - strlen(line) - 1);
        }
        pad_to(line, right - left + 1, sizeof(line));
        if (paint) screen_out_at(y + r, left, line);
        painted = r + 1;
    }
    for (; paint && r < rows; r++) {   /* the rows below the data, blank */
        line[0] = '\0';
        pad_to(line, right - left + 1, sizeof(line));
        screen_out_at(y + r, left, line);
    }
    /* back to the current row */
    if (tb_skip(ctx, o, rowpos - painted, &moved) != 0) return -1;
    if (paint) screen_setpos(y + rowpos - 1, left);
    iv_set(o, "stable", val_logic(1));
    *result = val_logic(1);
    (void)self;
    return 0;
}

static const method_t tbrowse_methods[] = {
    { "ADDCOLUMN", m_tb_addcolumn }, { "INSCOLUMN", m_tb_inscolumn }, { "DELCOLUMN", m_tb_delcolumn },
    { "GETCOLUMN", m_tb_getcolumn }, { "SETCOLUMN", m_tb_setcolumn },
    { "GOTOP", m_tb_gotop }, { "GOBOTTOM", m_tb_gobottom }, { "UP", m_tb_up }, { "DOWN", m_tb_down },
    { "PAGEUP", m_tb_pageup }, { "PAGEDOWN", m_tb_pagedown }, { "HOME", m_tb_home }, { "END", m_tb_end },
    { "LEFT", m_tb_left }, { "RIGHT", m_tb_right }, { "PANLEFT", m_tb_left }, { "PANRIGHT", m_tb_right },
    { "PANHOME", m_tb_panhome }, { "PANEND", m_tb_panend },
    { "STABILIZE", m_tb_stabilize }, { "REFRESHALL", m_tb_invalidate }, { "REFRESHCURRENT", m_tb_invalidate },
    { "INVALIDATE", m_tb_invalidate }, { "CONFIGURE", m_tb_configure },
    { "HILITE", m_tb_self }, { "DEHILITE", m_tb_self }, { "COLORRECT", m_tb_self },
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
    if (is_call && m) return m(ctx, o, &self, args, nargs, result);
    if (p) {
        *result = *p;
        return 0;
    }
    if (m) return m(ctx, o, &self, args, nargs, result);
    snprintf(ctx->err_msg, sizeof(ctx->err_msg), "No exported %s: %s", is_call ? "method" : "variable", name);
    ctx->error = ctx->err_msg;
    return -1;
}
