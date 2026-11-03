/* bc.c -- s32-bc, SLOW BASIC compiled the QuickBASIC way: the program's
 * shape fixed before it runs, lowered to HIR, and stage08's backend (a
 * copy, hir/) making SLOW-32 assembly.  docs/plans/basic-compiler.md, "A
 * compiler that decides up front".
 *
 *     s32-bc prog.bas prog.s
 *
 * The front end is sbasic's: its loader, lexer and parser (linked in
 * from sbasic/src), and its checker (strict.c) -- a program the
 * compiler takes is one QuickBASIC loads.  Values are computed the
 * interpreter's way by its own code, through bc_rt.c: a value crosses as
 * a double and a run-time type; variables, labels and loops are the
 * compiler's own.  What it does not do yet it says, and stops ("not
 * yet: ...").
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include "value.h"
#include "error.h"
#include "ast.h"
#include "parser.h"
#include "program.h"
#include "strict.h"
#include "builtin.h"

/* --- what the backend reads of a C-like front end ------------------ */

typedef struct Node {
    char        *name;
    int          locals_size;
    int          nparams;
    int          is_varargs;
    int          is_static;
    int          bit_width;     /* C's _Alignas on a function's locals: 0 */
    struct Node *next;
    struct Node *body;
} Node;

#include "bc_contract.h"
#include "hir/hir.h"
#include "hir/hir_alias.h"
#include "hir/hir_ssa.h"
#include "hir/hir_opt.h"
#include "hir/hir_licm.h"
#include "hir/hir_burg.h"
#include "hir/hir_regalloc.h"

/* --- diagnostics -------------------------------------------------- */

static int bc_nerr;

static void bc_error(int line, const char *what) {
    fprintf(stderr, "s32-bc: line %d: %s\n", line, what);
    bc_nerr++;
}

static void bc_not_yet(int line, const char *what) {
    char m[160];
    snprintf(m, sizeof m, "not yet: %s", what);
    bc_error(line, m);
}

/* --- HIR helpers --------------------------------------------------- */

static int bc_frame;            /* bytes of the frame so far */
static int bc_live;             /* the current block can be reached */

static int iconst(int v) { return hi_emit(HI_ICONST, TY_INT, -1, -1, v, NULL); }

static int dconst(double d, int *hi) {
    int w[2];
    memcpy(w, &d, 8);
    *hi = hi_emit(HI_ICONST, TY_INT, -1, -1, w[1], NULL);
    return hi_emit(HI_ICONST, TY_INT, -1, -1, w[0], NULL);
}

static void carg(int v, int tag) {
    hir_carg_room(1);
    h_carg[h_ncarg] = v;
    h_carg_tag[h_ncarg] = tag;
    h_ncarg = h_ncarg + 1;
}

/* a call; its result's hi word in *hi when it is a double (hi != NULL) */
static int call_end(int cb, int nwords, const char *name, int *hi) {
    int r = hi_emit(HI_CALL, TY_INT, -1, -1, nwords, (char *)name);
    h_cbase[r] = cb;
    if (hi) *hi = hi_emit(HI_CALLHI, TY_INT, r, -1, 0, NULL);
    return r;
}

static int new_blk(void) { return hir_new_block(); }

static void begin_blk(int b) {
    hl_switch_block(b);
    bc_live = 1;
}

static void goto_blk(int b) {
    if (bc_live) hi_emit(HI_BR, TY_VOID, -1, -1, b, NULL);
    bc_live = 0;
}

/* statements after a jump: a block nothing reaches but its label */
static void dead_blk(void) {
    begin_blk(new_blk());
}

static int alloca_of(int ty, int size) {
    int a;
    bc_frame = bc_frame + size;
    a = hi_emit(HI_ALLOCA, ty, -1, -1, 0 - bc_frame, NULL);
    hl_alloca_room(1);
    hl_ainst[hl_nalloca] = a;
    hl_aoff[hl_nalloca] = 0 - bc_frame;
    hl_aslot[hl_nalloca] = 0;
    hl_nalloca = hl_nalloca + 1;
    return a;
}

/* --- values: a double (lo, hi) and its run-time type --------------- */

/* A value: three HIR words, lo, hi and its run-time val_type_t (bc_rt.c).
 * A number's lo and hi are its double's words; a string's lo is its
 * sb_string_t, a reference this value owns -- whatever takes the value
 * (a bcrt_ function, a variable) takes the reference. */
typedef struct { int lo, hi, t; } bv_t;

static void carg_bv(bv_t x) { carg(x.lo, 0); carg(x.hi, 0); carg(x.t, 0); }

/* a call returning a value: its words in r1:r2, its type bcrt_type()'s */
static bv_t bv_call(const char *name, int cb, int nwords) {
    bv_t r;
    r.lo = call_end(cb, nwords, name, &r.hi);
    r.t = call_end(h_ncarg, 0, "bcrt_type", NULL);
    return r;
}

/* --- variables ---------------------------------------------------- */

typedef struct {
    char name[64];
    int type;                   /* VAL_INTEGER, VAL_LONG, VAL_SINGLE, VAL_DOUBLE, VAL_STRING */
    int a, ahi;                 /* its slots: one, or a double's two words */
} var_t;

/* names the program uses as arrays (DIM, A(...) = , a call to no
 * builtin) or assigns: a builtin of that name is not called */
static char (*bc_shadow)[64];
static int bc_nshadow, bc_capshadow;

static void shadow_add(const char *name) {
    for (int i = 0; i < bc_nshadow; i++)
        if (strcmp(bc_shadow[i], name) == 0) return;
    if (bc_nshadow == bc_capshadow) {
        bc_capshadow = bc_capshadow ? bc_capshadow * 2 : 32;
        bc_shadow = realloc(bc_shadow, (size_t)bc_capshadow * sizeof(*bc_shadow));
        if (!bc_shadow) { fprintf(stderr, "s32-bc: out of memory\n"); exit(1); }
    }
    strncpy(bc_shadow[bc_nshadow], name, 63);
    bc_shadow[bc_nshadow][63] = '\0';
    bc_nshadow++;
}

static int shadowed(const char *name) {
    for (int i = 0; i < bc_nshadow; i++)
        if (strcmp(bc_shadow[i], name) == 0) return 1;
    return 0;
}

/* a builtin the interpreter would call for this name */
static int is_builtin_call(const char *name) {
    return builtin_exists(name) && !shadowed(name) &&
           strcmp(name, "LBOUND") != 0 && strcmp(name, "UBOUND") != 0 && strcmp(name, "JOIN$") != 0;
}

static var_t *bc_vars;
static int bc_nvars, bc_capvars;

static var_t *var_find(const char *name) {
    for (int i = 0; i < bc_nvars; i++)
        if (strcmp(bc_vars[i].name, name) == 0) return &bc_vars[i];
    return NULL;
}

/* made in the entry block, 0 (a string, empty) there, as every BASIC
 * variable starts */
static var_t *var_get(const char *name, int type, int line) {
    var_t *v = var_find(name);
    if (v) return v;
    if (type != VAL_INTEGER && type != VAL_LONG && type != VAL_SINGLE &&
        type != VAL_DOUBLE && type != VAL_STRING) {
        bc_not_yet(line, "a variable of this type");
        type = VAL_SINGLE;
    }
    if (bc_nvars == bc_capvars) {
        bc_capvars = bc_capvars ? bc_capvars * 2 : 32;
        bc_vars = realloc(bc_vars, (size_t)bc_capvars * sizeof(var_t));
        if (!bc_vars) { fprintf(stderr, "s32-bc: out of memory\n"); exit(1); }
    }
    v = &bc_vars[bc_nvars++];
    strncpy(v->name, name, 63);
    v->name[63] = '\0';
    v->type = type;
    v->ahi = -1;
    if (type == VAL_DOUBLE) {
        v->a = alloca_of(TY_INT, 4);
        v->ahi = alloca_of(TY_INT, 4);
    } else
        v->a = alloca_of(type == VAL_SINGLE ? TY_FLOAT : TY_INT, 4);
    return v;
}

static bv_t var_load(var_t *v) {
    bv_t r;
    int cb, x;
    r.t = iconst(v->type);
    if (v->type == VAL_DOUBLE) {
        r.lo = hi_emit(HI_LOAD, TY_INT, v->a, -1, 0, NULL);
        r.hi = hi_emit(HI_LOAD, TY_INT, v->ahi, -1, 0, NULL);
        return r;
    }
    if (v->type == VAL_STRING) {        /* a reference of the value's own */
        x = hi_emit(HI_LOAD, TY_INT, v->a, -1, 0, NULL);
        cb = h_ncarg;
        carg(x, 0);
        r.lo = call_end(cb, 1, "bcrt_ref", NULL);
        r.hi = iconst(0);
        return r;
    }
    x = hi_emit(HI_LOAD, v->type == VAL_SINGLE ? TY_FLOAT : TY_INT, v->a, -1, 0, NULL);
    cb = h_ncarg;
    carg(x, 0);
    r.lo = call_end(cb, 1, v->type == VAL_SINGLE ? "__fp64_cvt_ftoD" : "__fp64_cvt_itoD", &r.hi);
    return r;
}

/* x, of any type, stored as v holds it (bcrt_conv: rounding, Overflow,
 * Type mismatch); a string variable lets its old string go */
static void var_store(var_t *v, bv_t x) {
    int cb, lo, hi, w;
    cb = h_ncarg;
    carg_bv(x);
    carg(iconst(v->type), 0);
    lo = call_end(cb, 4, "bcrt_conv", &hi);
    if (v->type == VAL_DOUBLE) {
        hi_emit(HI_STORE, TY_INT, v->a, lo, 0, NULL);
        hi_emit(HI_STORE, TY_INT, v->ahi, hi, 0, NULL);
        return;
    }
    if (v->type == VAL_STRING) {
        int old = hi_emit(HI_LOAD, TY_INT, v->a, -1, 0, NULL);
        hi_emit(HI_STORE, TY_INT, v->a, lo, 0, NULL);
        cb = h_ncarg;
        carg(old, 0);
        call_end(cb, 1, "bcrt_unref", NULL);
        return;
    }
    cb = h_ncarg;
    carg(lo, 0); carg(hi, 0);
    w = call_end(cb, 2, v->type == VAL_SINGLE ? "__fp64_cvt_DtoF" : "__fp64_cvt_DtoI", NULL);
    h_ty[w] = v->type == VAL_SINGLE ? TY_FLOAT : TY_INT;
    hi_emit(HI_STORE, v->type == VAL_SINGLE ? TY_FLOAT : TY_INT, v->a, w, 0, NULL);
}

static void var_zero(var_t *v) {
    if (v->type == VAL_DOUBLE) {
        hi_emit(HI_STORE, TY_INT, v->a, iconst(0), 0, NULL);
        hi_emit(HI_STORE, TY_INT, v->ahi, iconst(0), 0, NULL);
    } else if (v->type == VAL_SINGLE) {
        int z = hi_emit(HI_ICONST, TY_FLOAT, -1, -1, 0, NULL);
        hi_emit(HI_STORE, TY_FLOAT, v->a, z, 0, NULL);
    } else if (v->type == VAL_STRING) {
        int e = call_end(h_ncarg, 0, "bcrt_str_empty", NULL);
        hi_emit(HI_STORE, TY_INT, v->a, e, 0, NULL);
    } else
        hi_emit(HI_STORE, TY_INT, v->a, iconst(0), 0, NULL);
}

/* --- expressions -------------------------------------------------- */

static bv_t expr(expr_t *e);

static int saddr(const char *s, int len) {
    return hi_emit(HI_SADDR, TY_INT, -1, -1, bc_intern_str((char *)s, len), NULL);
}

/* the numeric zero, standing for an argument not given */
static bv_t zero(void) {
    bv_t r;
    r.lo = dconst(0, &r.hi);
    r.t = iconst(VAL_INTEGER);
    return r;
}

static bv_t builtin_fn(expr_t *e, const char *name, int n, expr_t **args) {
    bv_t a[3];
    int cb;
    if (n > 3 || strcmp(name, "TAB") == 0 || strcmp(name, "SPC") == 0) {   /* (TAB, SPC in a PRINT list: bc_print) */
        bc_not_yet(e->line, n > 3 ? "a builtin of more than three arguments" : "TAB and SPC outside PRINT");
        n = 0;
    }
    for (int i = 0; i < 3; i++) a[i] = i < n ? expr(args[i]) : zero();
    cb = h_ncarg;
    carg(saddr(name, (int)strlen(name)), 0);
    carg(iconst(n), 0);
    for (int i = 0; i < 3; i++) carg_bv(a[i]);
    return bv_call("bcrt_fn", cb, 11);
}

static bv_t bin_call(const char *name, int op, bv_t a, bv_t b) {
    int cb = h_ncarg;
    carg(iconst(op), 0);
    carg_bv(a);
    carg_bv(b);
    return bv_call(name, cb, 7);
}

/* name(i[, j[, k]]): subscripts evaluated left to right, then the
 * element (bcrt_aget: the array, or an implicit one, as an expression
 * reads it) */
static void sub_args(int line, int n, expr_t **ix, bv_t *a) {
    if (n > 3) bc_not_yet(line, "arrays of more than three dimensions");
    for (int i = 0; i < 3; i++) a[i] = i < n && n <= 3 ? expr(ix[i]) : zero();
}

static bv_t array_read(expr_t *e) {
    bv_t a[3];
    int cb;
    sub_args(e->line, e->call.nargs, e->call.args, a);
    cb = h_ncarg;
    carg(saddr(e->call.name, (int)strlen(e->call.name)), 0);
    carg(iconst(e->call.nargs), 0);
    for (int i = 0; i < 3; i++) carg_bv(a[i]);
    return bv_call("bcrt_aget", cb, 11);
}

static bv_t expr(expr_t *e) {
    bv_t r;
    switch (e->type) {
    case EXPR_LITERAL:
        if (e->literal.type == VAL_STRING) {
            int cb = h_ncarg;
            carg(saddr(e->literal.sval->data, e->literal.sval->len), 0);
            carg(iconst(e->literal.sval->len), 0);
            r.lo = call_end(cb, 2, "bcrt_str_lit", NULL);
            r.hi = iconst(0);
            r.t = iconst(VAL_STRING);
            return r;
        } else {
            double d = 0;
            val_to_double(&e->literal, &d);
            r.lo = dconst(d, &r.hi);
            r.t = iconst(e->literal.type);
            return r;
        }
    case EXPR_VARIABLE:
        if (is_builtin_call(e->var.name))       /* RND, TIMER... with no ( ) */
            return builtin_fn(e, e->var.name, 0, NULL);
        return var_load(var_get(e->var.name, e->var.var_type, e->line));
    case EXPR_BINARY:
        return bin_call("bcrt_bin", e->binary.op, expr(e->binary.left), expr(e->binary.right));
    case EXPR_COMPARE:
        return bin_call("bcrt_cmp", e->compare.op, expr(e->compare.left), expr(e->compare.right));
    case EXPR_UNARY: {
        bv_t a = expr(e->unary.operand);
        int cb = h_ncarg;
        carg(iconst(e->unary.op), 0);
        carg_bv(a);
        return bv_call("bcrt_un", cb, 4);
    }
    case EXPR_CALL:
        if (is_builtin_call(e->call.name))
            return builtin_fn(e, e->call.name, e->call.nargs, e->call.args);
        if (strncmp(e->call.name, "FN", 2) == 0) { bc_not_yet(e->line, "DEF FN"); break; }
        return array_read(e);
    default:
        bc_not_yet(e->line, "this expression");
        break;
    }
    return zero();
}

/* the condition's truth, an int */
static int cond(expr_t *e) {
    bv_t c = expr(e);
    int cb = h_ncarg;
    carg_bv(c);
    return call_end(cb, 3, "bcrt_true", NULL);
}

/* --- labels ------------------------------------------------------- */

typedef struct { char name[64]; int blk; } label_t;
static label_t *bc_labels;
static int bc_nlabels, bc_caplabels;

static int label_blk(const char *name) {
    for (int i = 0; i < bc_nlabels; i++)
        if (strcmp(bc_labels[i].name, name) == 0) return bc_labels[i].blk;
    if (bc_nlabels == bc_caplabels) {
        bc_caplabels = bc_caplabels ? bc_caplabels * 2 : 64;
        bc_labels = realloc(bc_labels, (size_t)bc_caplabels * sizeof(label_t));
        if (!bc_labels) { fprintf(stderr, "s32-bc: out of memory\n"); exit(1); }
    }
    strncpy(bc_labels[bc_nlabels].name, name, 63);
    bc_labels[bc_nlabels].name[63] = '\0';
    bc_labels[bc_nlabels].blk = new_blk();
    return bc_labels[bc_nlabels++].blk;
}

/* --- statements --------------------------------------------------- */

/* a FOR open in its block: its variable, its limit's and step's slots,
 * its body, and the block after its NEXT */
typedef struct {
    var_t *v;
    int lim, limhi, stp, stphi;
    int body, exit;
} for_t;

static void bc_stmts(stmt_t *s);

/* GOSUB's return points, and the block every RETURN goes to: it pops
 * one (bcrt_return) and jumps through a table of them, made last, when
 * all are known */
static int *bc_rets;
static int bc_nrets, bc_caprets;
static int bc_ret_blk = -1;

static void gosub_to(int blk) {
    int cb, back;
    if (bc_nrets == bc_caprets) {
        bc_caprets = bc_caprets ? bc_caprets * 2 : 32;
        bc_rets = realloc(bc_rets, (size_t)bc_caprets * sizeof(int));
        if (!bc_rets) { fprintf(stderr, "s32-bc: out of memory\n"); exit(1); }
    }
    back = new_blk();
    bc_rets[bc_nrets] = back;
    cb = h_ncarg;
    carg(iconst(bc_nrets), 0);
    call_end(cb, 1, "bcrt_gosub", NULL);
    bc_nrets++;
    goto_blk(blk);
    begin_blk(back);
}

static void ret_dispatch(void) {
    int k, ok, bt, bd;
    if (bc_ret_blk < 0) return;
    begin_blk(bc_ret_blk);
    k = call_end(h_ncarg, 0, "bcrt_return", NULL);
    bt = new_blk();
    bd = new_blk();
    ok = hi_emit(HI_SLTU, TY_INT, k, iconst(bc_nrets), 0, NULL);
    hi_emit(HI_BRC, TY_VOID, ok, bt, bd, NULL);
    bc_live = 0;
    begin_blk(bt);
    if (bc_nrets > 0) hi_emit_jmptab(k, bd, bc_rets, bc_nrets);
    else hi_emit(HI_BR, TY_VOID, -1, -1, bd, NULL);
    bc_live = 0;
    begin_blk(bd);              /* (only a number not pushed reaches here) */
    hi_emit(HI_RET, TY_INT, iconst(0), -1, 0, NULL);
    bc_live = 0;
}

/* what a statement s32-bc does not do yet is called, for the message */
static const char *stmt_word(int t) {
    switch (t) {
    case STMT_INPUT: return "INPUT";
    case STMT_GOSUB: return "GOSUB";
    case STMT_RETURN: return "RETURN";
    case STMT_DIM: return "DIM";
    case STMT_ARRAY_ASSIGN: return "arrays";
    case STMT_DATA: return "DATA";
    case STMT_READ: return "READ";
    case STMT_RESTORE: return "RESTORE";
    case STMT_ON_GOTO: return "ON GOTO";
    case STMT_ON_GOSUB: return "ON GOSUB";
    case STMT_SELECT: return "SELECT CASE";
    case STMT_CALL: return "CALL";
    case STMT_SUB_DEF: return "SUB";
    case STMT_FUNC_DEF: return "FUNCTION";
    case STMT_DEF_FN: return "DEF FN";
    case STMT_RANDOMIZE: return "RANDOMIZE";
    case STMT_CLS: return "CLS";
    case STMT_LOCATE: return "LOCATE";
    case STMT_COLOR: return "COLOR";
    case STMT_EXIT: return "EXIT";
    case STMT_SWAP: return "SWAP";
    case STMT_DEFTYPE: return "DEFINT and its kind";
    case STMT_CONST: return "CONST";
    case STMT_DIM_SCALAR: return "DIM AS";
    case STMT_OPEN: case STMT_CLOSE: case STMT_PRINT_FILE: case STMT_INPUT_FILE:
    case STMT_LINE_INPUT: case STMT_WRITE_FILE: return "files";
    case STMT_ON_ERROR: return "ON ERROR";
    case STMT_RESUME: return "RESUME";
    case STMT_ERROR_RAISE: return "ERROR";
    case STMT_LINE_INPUT_CONSOLE: return "LINE INPUT";
    case STMT_WRITE_CONSOLE: return "WRITE";
    case STMT_MID_ASSIGN: return "MID$ =";
    case STMT_SLEEP: return "SLEEP";
    case STMT_WIDTH: return "WIDTH";
    case STMT_CLEAR: return "CLEAR";
    case STMT_OPTION_BASE: return "OPTION BASE";
    case STMT_ERASE: return "ERASE";
    case STMT_TYPE_DEF: case STMT_DIM_AS_TYPE: case STMT_FIELD_ASSIGN: return "TYPE";
    default: return "this statement";
    }
}

static void call_void(const char *name) {
    call_end(h_ncarg, 0, name, NULL);
}

static void bc_print(stmt_t *s) {
    int nl = 1;
    if (s->print.using_fmt || s->print.using_expr) { bc_not_yet(s->line, "PRINT USING"); return; }
    for (int i = 0; i < s->print.nitems; i++) {
        print_item_t *it = &s->print.items[i];
        if (it->expr) {
            expr_t *e = it->expr;
            int cb;
            if (e->type == EXPR_CALL && e->call.nargs == 1 && is_builtin_call(e->call.name) &&
                (strcmp(e->call.name, "TAB") == 0 || strcmp(e->call.name, "SPC") == 0)) {
                bv_t a = expr(e->call.args[0]);
                cb = h_ncarg;
                carg(saddr(e->call.name, 3), 0);
                carg_bv(a);
                call_end(cb, 4, "bcrt_print_tab", NULL);
            } else {
                bv_t v = expr(e);
                cb = h_ncarg;
                carg_bv(v);
                call_end(cb, 3, "bcrt_print_val", NULL);
            }
        }
        if (it->sep == ';') nl = 0;
        else if (it->sep == ',') { call_void("bcrt_print_comma"); nl = 0; }
        else nl = 1;
    }
    if (nl) call_void("bcrt_print_nl");
}

/* the open FORs of the block being lowered, innermost last */
static for_t *bc_fors;
static int bc_nfors, bc_capfors;

static int dslot_store(bv_t x, int *hi) {
    int lo = alloca_of(TY_INT, 4);
    *hi = alloca_of(TY_INT, 4);
    hi_emit(HI_STORE, TY_INT, lo, x.lo, 0, NULL);
    hi_emit(HI_STORE, TY_INT, *hi, x.hi, 0, NULL);
    return lo;
}

/* whether the loop runs with its variable at x (for_in_range) */
static int for_runs(for_t *f, bv_t x) {
    int cb = h_ncarg;
    carg(x.lo, 1); carg(x.hi, 2);
    carg(hi_emit(HI_LOAD, TY_INT, f->lim, -1, 0, NULL), 1);
    carg(hi_emit(HI_LOAD, TY_INT, f->limhi, -1, 0, NULL), 2);
    carg(hi_emit(HI_LOAD, TY_INT, f->stp, -1, 0, NULL), 1);
    carg(hi_emit(HI_LOAD, TY_INT, f->stphi, -1, 0, NULL), 2);
    return call_end(cb, 6, "bcrt_for_in", NULL);
}

/* FOR v = start TO limit [STEP step], as the interpreter's for_start:
 * the three evaluated once, in that order; limit and step kept as
 * doubles in the FOR's own slots; the variable set as an assignment
 * sets it; the loop skipped when it starts out of range */
static void bc_for(stmt_t *s) {
    for_t *f;
    bv_t start, lim, stp;
    int runs;
    var_t *v = var_get(s->for_stmt.var_name, s->for_stmt.var_type, s->line);
    if (s->for_stmt.body) { bc_not_yet(s->line, "a FOR with a body"); return; }
    if (bc_nfors == bc_capfors) {
        bc_capfors = bc_capfors ? bc_capfors * 2 : 16;
        bc_fors = realloc(bc_fors, (size_t)bc_capfors * sizeof(for_t));
        if (!bc_fors) { fprintf(stderr, "s32-bc: out of memory\n"); exit(1); }
    }
    start = expr(s->for_stmt.start);
    lim = expr(s->for_stmt.end);
    if (s->for_stmt.step) stp = expr(s->for_stmt.step);
    else { stp.lo = dconst(1.0, &stp.hi); stp.t = iconst(VAL_INTEGER); }
    f = &bc_fors[bc_nfors++];
    f->v = v;
    f->lim = dslot_store(lim, &f->limhi);
    f->stp = dslot_store(stp, &f->stphi);
    var_store(v, start);
    runs = for_runs(f, var_load(v));
    f->body = new_blk();
    f->exit = new_blk();
    hi_emit(HI_BRC, TY_VOID, runs, f->body, f->exit, NULL);
    bc_live = 0;
    begin_blk(f->body);
}

/* NEXT [v, ...]: each closes the innermost open FOR (the checker made
 * sure it is the one named), steps its variable where it lives
 * (for_next), and loops while it is in range */
static void bc_next(stmt_t *s) {
    int k = s->next_stmt.nvars ? s->next_stmt.nvars : 1;
    for (int i = 0; i < k; i++) {
        for_t *f;
        bv_t nv;
        int cb, runs;
        if (bc_nfors == 0) { bc_error(s->line, "NEXT without FOR"); return; }
        f = &bc_fors[--bc_nfors];
        nv = var_load(f->v);
        cb = h_ncarg;
        carg(nv.lo, 1); carg(nv.hi, 2); carg(iconst(f->v->type), 0);
        carg(hi_emit(HI_LOAD, TY_INT, f->stp, -1, 0, NULL), 1);
        carg(hi_emit(HI_LOAD, TY_INT, f->stphi, -1, 0, NULL), 2);
        nv.lo = call_end(cb, 5, "bcrt_for_step", &nv.hi);
        nv.t = iconst(f->v->type);
        var_store(f->v, nv);
        runs = for_runs(f, nv);
        hi_emit(HI_BRC, TY_VOID, runs, f->body, f->exit, NULL);
        bc_live = 0;
        begin_blk(f->exit);
    }
}

/* DIM's with constant bounds are made before the program runs, in the
 * order of the text, with the OPTION BASE of their place in it -- as
 * QuickBASIC compiles them and the interpreter's scan makes them
 * (scan_static_dim) */
static stmt_t **bc_statics;
static int bc_nstatics, bc_capstatics;

static int is_static_dim(stmt_t *s) {
    for (int i = 0; i < bc_nstatics; i++)
        if (bc_statics[i] == s) return 1;
    return 0;
}

static void bc_dim(stmt_t *s, int is_static, int base) {
    bv_t a[3];
    int cb;
    if (s->dim_stmt.ndims > 3) { bc_not_yet(s->line, "arrays of more than three dimensions"); return; }
    for (int i = 0; i < 3; i++) a[i] = i < s->dim_stmt.ndims ? expr(s->dim_stmt.dims[i]) : zero();
    cb = h_ncarg;
    carg(saddr(s->dim_stmt.name, (int)strlen(s->dim_stmt.name)), 0);
    carg(iconst(s->dim_stmt.elem_type), 0);
    carg(iconst(s->dim_stmt.ndims), 0);
    carg(iconst(is_static), 0);
    carg(iconst(base), 0);
    carg(iconst(s->dim_stmt.is_redim), 0);
    carg(iconst(s->dim_stmt.preserve), 0);
    for (int i = 0; i < 3; i++) carg_bv(a[i]);
    call_end(cb, 16, "bcrt_dim", NULL);
}

static int bc_base;             /* the OPTION BASE of the text, as it is read */

static int static_names_has(const char *name) {
    for (int i = 0; i < bc_nstatics; i++)
        if (strcmp(bc_statics[i]->dim_stmt.name, name) == 0) return 1;
    return 0;
}

static void hoist_dims(stmt_t *s) {
    for (; s; s = s->next) {
        if (s->type == STMT_OPTION_BASE) bc_base = s->option_base.base;
        if (s->type == STMT_DIM && !s->dim_stmt.is_redim && s->dim_stmt.ndims <= 3 &&
            !static_names_has(s->dim_stmt.name)) {
            int ok = 1;
            for (int i = 0; i < s->dim_stmt.ndims && ok; i++) {
                expr_t *d = s->dim_stmt.dims[i];
                int upper;
                if (!d || d->type != EXPR_LITERAL || d->literal.type == VAL_STRING ||
                    val_to_integer(&d->literal, &upper) != ERR_NONE || upper - bc_base + 1 <= 0)
                    ok = 0;
            }
            if (ok) {
                if (bc_nstatics == bc_capstatics) {
                    bc_capstatics = bc_capstatics ? bc_capstatics * 2 : 16;
                    bc_statics = realloc(bc_statics, (size_t)bc_capstatics * sizeof(stmt_t *));
                    if (!bc_statics) { fprintf(stderr, "s32-bc: out of memory\n"); exit(1); }
                }
                bc_statics[bc_nstatics++] = s;
                bc_dim(s, 1, bc_base);
            }
        }
        switch (s->type) {
        case STMT_FOR: hoist_dims(s->for_stmt.body); break;
        case STMT_IF: hoist_dims(s->if_stmt.then_body); hoist_dims(s->if_stmt.else_body); break;
        case STMT_WHILE: hoist_dims(s->while_stmt.body); break;
        case STMT_DO_LOOP: hoist_dims(s->do_loop.body); break;
        case STMT_SELECT:
            for (case_clause_t *c = s->select_stmt.clauses; c; c = c->next) hoist_dims(c->body);
            break;
        default: break;
        }
    }
}

static void bc_stmt(stmt_t *s) {
    if (!bc_live && s->type != STMT_LABEL) dead_blk();
    switch (s->type) {
    case STMT_REM:
    case STMT_NOOP:
        break;
    case STMT_LABEL: {
        int b = label_blk(s->label.name);
        goto_blk(b);
        begin_blk(b);
        break;
    }
    case STMT_PRINT:
        bc_print(s);
        break;
    case STMT_ASSIGN:
        var_store(var_get(s->assign.name, s->assign.var_type, s->line), expr(s->assign.value));
        break;
    case STMT_GOTO:
        goto_blk(label_blk(s->goto_stmt.label));
        break;
    case STMT_GOSUB:
        gosub_to(label_blk(s->goto_stmt.label));
        break;
    case STMT_RETURN:
        if (bc_ret_blk < 0) bc_ret_blk = new_blk();
        goto_blk(bc_ret_blk);
        break;
    case STMT_END:
        call_void("bcrt_end");
        goto_blk(new_blk());    /* (never taken: bcrt_end does not return) */
        break;
    case STMT_IF: {
        int c = cond(s->if_stmt.condition);
        int bt = new_blk(), be = new_blk(), bj = new_blk();
        hi_emit(HI_BRC, TY_VOID, c, bt, be, NULL);
        bc_live = 0;
        begin_blk(bt);
        bc_stmts(s->if_stmt.then_body);
        goto_blk(bj);
        begin_blk(be);
        bc_stmts(s->if_stmt.else_body);
        goto_blk(bj);
        begin_blk(bj);
        break;
    }
    case STMT_WHILE: {
        int bh = new_blk(), bb = new_blk(), bx = new_blk(), c;
        goto_blk(bh);
        begin_blk(bh);
        c = cond(s->while_stmt.condition);
        hi_emit(HI_BRC, TY_VOID, c, bb, bx, NULL);
        bc_live = 0;
        begin_blk(bb);
        bc_stmts(s->while_stmt.body);
        goto_blk(bh);
        begin_blk(bx);
        break;
    }
    case STMT_FOR:
        bc_for(s);
        break;
    case STMT_DIM:
        bc_dim(s, is_static_dim(s), -1);
        break;
    case STMT_ARRAY_ASSIGN: {
        bv_t a[3], x;
        int cb;
        sub_args(s->line, s->array_assign.nindices, s->array_assign.indices, a);
        x = expr(s->array_assign.value);
        cb = h_ncarg;
        carg(saddr(s->array_assign.name, (int)strlen(s->array_assign.name)), 0);
        carg(iconst(s->array_assign.nindices), 0);
        for (int i = 0; i < 3; i++) carg_bv(a[i]);
        carg_bv(x);
        call_end(cb, 14, "bcrt_aset", NULL);
        break;
    }
    case STMT_OPTION_BASE: {
        int cb = h_ncarg;
        carg(iconst(s->option_base.base), 0);
        call_end(cb, 1, "bcrt_option_base", NULL);
        break;
    }
    case STMT_ERASE:
        for (int i = 0; i < s->erase_stmt.nnames; i++) {
            int cb = h_ncarg;
            carg(saddr(s->erase_stmt.names[i], (int)strlen(s->erase_stmt.names[i])), 0);
            call_end(cb, 1, "bcrt_erase", NULL);
        }
        break;
    case STMT_NEXT:
        bc_next(s);
        break;
    case STMT_DO_LOOP: {
        /* DO [WHILE|UNTIL c] ... LOOP [WHILE|UNTIL c] */
        int bh = new_blk(), bb = new_blk(), bt = new_blk(), bx = new_blk(), c;
        goto_blk(bh);
        begin_blk(bh);
        if (s->do_loop.pre_cond) {
            c = cond(s->do_loop.pre_cond);
            if (s->do_loop.pre_until) hi_emit(HI_BRC, TY_VOID, c, bx, bb, NULL);
            else hi_emit(HI_BRC, TY_VOID, c, bb, bx, NULL);
            bc_live = 0;
        }
        goto_blk(bb);
        begin_blk(bb);
        bc_stmts(s->do_loop.body);
        goto_blk(bt);
        begin_blk(bt);
        if (s->do_loop.post_cond) {
            c = cond(s->do_loop.post_cond);
            if (s->do_loop.post_until) hi_emit(HI_BRC, TY_VOID, c, bx, bh, NULL);
            else hi_emit(HI_BRC, TY_VOID, c, bh, bx, NULL);
            bc_live = 0;
        } else
            goto_blk(bh);
        begin_blk(bx);
        break;
    }
    default:
        bc_not_yet(s->line, stmt_word(s->type));
        break;
    }
}

static void bc_stmts(stmt_t *s) {
    for (; s; s = s->next) bc_stmt(s);
}

/* --- variables first ---------------------------------------------- */

/* Every variable the program names gets its slot and its 0 in the entry
 * block, before anything runs -- wherever its first mention falls. */
static void prescan_expr(expr_t *e, void *ctx) {
    (void)ctx;
    if (e->type == EXPR_VARIABLE && !is_builtin_call(e->var.name))
        var_get(e->var.name, e->var.var_type, e->line);
    if (e->type == EXPR_CALL && !builtin_exists(e->call.name)) shadow_add(e->call.name);
}

/* names used as arrays or assigned, before anything is lowered */
static void preshadow(stmt_t *s) {
    for (; s; s = s->next) {
        switch (s->type) {
        case STMT_ASSIGN: if (builtin_exists(s->assign.name)) shadow_add(s->assign.name); break;
        case STMT_DIM: shadow_add(s->dim_stmt.name); break;
        case STMT_ARRAY_ASSIGN: shadow_add(s->array_assign.name); break;
        case STMT_FOR: preshadow(s->for_stmt.body); break;
        case STMT_IF: preshadow(s->if_stmt.then_body); preshadow(s->if_stmt.else_body); break;
        case STMT_WHILE: preshadow(s->while_stmt.body); break;
        case STMT_DO_LOOP: preshadow(s->do_loop.body); break;
        case STMT_SELECT:
            for (case_clause_t *c = s->select_stmt.clauses; c; c = c->next) preshadow(c->body);
            break;
        default: break;
        }
    }
}
static void prescan_top(expr_t *e, void *ctx) { expr_walk(e, prescan_expr, ctx); }

static void prescan(stmt_t *s) {
    for (; s; s = s->next) {
        stmt_exprs(s, prescan_top, NULL);
        switch (s->type) {
        case STMT_ASSIGN: var_get(s->assign.name, s->assign.var_type, s->line); break;
        case STMT_FOR:
            var_get(s->for_stmt.var_name, s->for_stmt.var_type, s->line);
            prescan(s->for_stmt.body);
            break;
        case STMT_IF: prescan(s->if_stmt.then_body); prescan(s->if_stmt.else_body); break;
        case STMT_WHILE: prescan(s->while_stmt.body); break;
        case STMT_DO_LOOP: prescan(s->do_loop.body); break;
        case STMT_SELECT:
            for (case_clause_t *c = s->select_stmt.clauses; c; c = c->next) prescan(c->body);
            break;
        default: break;
        }
    }
}

/* --- the program -------------------------------------------------- */

static stmt_t *bc_program;
static Node bc_main;
static Node bc_root;

static void hl_func(Node *fn) {
    int entry;
    hir_reset();
    hl_nalloca = 0;
    hl_temp_stack = 0;
    hl_nparams = 0;
    hl_param_nflat = 0;
    bc_frame = 8;               /* the saved r31 and r30, as the C front end keeps them */
    bc_nvars = 0;
    bc_nlabels = 0;
    bc_nfors = 0;
    bc_nrets = 0;
    bc_ret_blk = -1;
    entry = new_blk();
    begin_blk(entry);
    bc_nshadow = 0;
    preshadow(bc_program);
    prescan(bc_program);
    for (int i = 0; i < bc_nvars; i++) var_zero(&bc_vars[i]);
    call_void("bcrt_start");
    /* the static arrays, then the OPTION BASE the text ends with in
     * force until an OPTION BASE runs (the interpreter's scan leaves it) */
    bc_nstatics = 0;
    bc_base = 0;
    hoist_dims(bc_program);
    {
        int cb = h_ncarg;
        carg(iconst(bc_base), 0);
        call_end(cb, 1, "bcrt_option_base", NULL);
    }
    bc_stmts(bc_program);
    if (bc_live) call_void("bcrt_end");
    if (bc_live) hi_emit(HI_RET, TY_INT, iconst(0), -1, 0, NULL);
    bc_live = 0;
    ret_dispatch();
    fn->locals_size = bc_frame;
}

#include "hir/hir_codegen.h"

static void strict_report(void *ctx, int line, const char *code, const char *detail) {
    (void)ctx;
    fprintf(stderr, "s32-bc: line %d: %s: %s\n", line, code, detail);
}

int main(int argc, char **argv) {
    parser_t p;
    char *text;
    int fd;
    if (argc != 3) {
        fprintf(stderr, "usage: s32-bc prog.bas prog.s\n");
        return 2;
    }
    if (program_load_file(argv[1]) < 0) {
        fprintf(stderr, "s32-bc: cannot read %s\n", argv[1]);
        return 1;
    }
    text = program_get_text();
    parser_init(&p, text, 1);
    bc_program = parser_parse(&p);
    if (parser_had_error(&p)) {
        fprintf(stderr, "s32-bc: line %d: %s%s%s\n", p.error_line, error_message(p.error),
                p.error_detail[0] ? ": " : "", p.error_detail);
        return 1;
    }
    if (strict_check(bc_program, strict_report, NULL, NULL) > 0) return 1;
    fd = open(argv[2], O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (fd < 0) {
        fprintf(stderr, "s32-bc: cannot write %s\n", argv[2]);
        return 1;
    }
    bc_main.name = "main";
    bc_main.next = NULL;
    bc_root.body = &bc_main;
    cg_fd = fd;
    cg_outpath = argv[2];
    gen_program(&bc_root);
    cg_flush();
    close(fd);
    if (bc_nerr > 0) {
        unlink(argv[2]);
        return 1;
    }
    return 0;
}
