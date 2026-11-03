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

typedef struct { int lo, hi, t; } bv_t;

static bv_t bv_dcall(const char *name, int cb, int nwords) {
    bv_t r;
    r.lo = call_end(cb, nwords, name, &r.hi);
    r.t = call_end(h_ncarg, 0, "bcrt_type", NULL);
    return r;
}

/* --- variables ---------------------------------------------------- */

typedef struct {
    char name[64];
    int type;                   /* VAL_INTEGER, VAL_LONG, VAL_SINGLE, VAL_DOUBLE */
    int a, ahi;                 /* its slots: one, or a double's two words */
} var_t;

static var_t *bc_vars;
static int bc_nvars, bc_capvars;

static var_t *var_find(const char *name) {
    for (int i = 0; i < bc_nvars; i++)
        if (strcmp(bc_vars[i].name, name) == 0) return &bc_vars[i];
    return NULL;
}

/* made in the entry block, 0 there, as every BASIC variable starts */
static var_t *var_get(const char *name, int type, int line) {
    var_t *v = var_find(name);
    if (v) return v;
    if (type == VAL_STRING) { bc_not_yet(line, "string variables"); type = VAL_SINGLE; }
    if (type != VAL_INTEGER && type != VAL_LONG && type != VAL_SINGLE && type != VAL_DOUBLE) {
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
    x = hi_emit(HI_LOAD, v->type == VAL_SINGLE ? TY_FLOAT : TY_INT, v->a, -1, 0, NULL);
    cb = h_ncarg;
    carg(x, 0);
    r.lo = call_end(cb, 1, v->type == VAL_SINGLE ? "__fp64_cvt_ftoD" : "__fp64_cvt_itoD", &r.hi);
    return r;
}

/* x, of any type, stored as v holds it */
static void var_store(var_t *v, bv_t x) {
    int cb, lo, hi, w;
    cb = h_ncarg;
    carg(x.lo, 1); carg(x.hi, 2); carg(x.t, 0); carg(iconst(v->type), 0);
    lo = call_end(cb, 4, "bcrt_conv", &hi);
    if (v->type == VAL_DOUBLE) {
        hi_emit(HI_STORE, TY_INT, v->a, lo, 0, NULL);
        hi_emit(HI_STORE, TY_INT, v->ahi, hi, 0, NULL);
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
    } else
        hi_emit(HI_STORE, TY_INT, v->a, iconst(0), 0, NULL);
}

/* --- expressions -------------------------------------------------- */

static bv_t expr(expr_t *e);

static bv_t bin_call(const char *name, int op, bv_t a, bv_t b) {
    int cb = h_ncarg;
    carg(iconst(op), 0);
    carg(a.lo, 1); carg(a.hi, 2); carg(a.t, 0);
    carg(b.lo, 1); carg(b.hi, 2); carg(b.t, 0);
    return bv_dcall(name, cb, 7);
}

static bv_t expr(expr_t *e) {
    bv_t r;
    r.lo = r.hi = r.t = -1;
    switch (e->type) {
    case EXPR_LITERAL: {
        double d = 0;
        if (e->literal.type == VAL_STRING) {
            bc_not_yet(e->line, "a string in an expression");
            d = 0;
        } else
            val_to_double(&e->literal, &d);
        r.lo = dconst(d, &r.hi);
        r.t = iconst(e->literal.type == VAL_STRING ? VAL_SINGLE : e->literal.type);
        return r;
    }
    case EXPR_VARIABLE:
        if (builtin_exists(e->var.name)) {      /* RND, TIMER, INKEY$... with no ( ) */
            bc_not_yet(e->line, "functions");
            break;
        }
        return var_load(var_get(e->var.name, e->var.var_type, e->line));
    case EXPR_BINARY:
        return bin_call("bcrt_bin", e->binary.op, expr(e->binary.left), expr(e->binary.right));
    case EXPR_COMPARE:
        return bin_call("bcrt_cmp", e->compare.op, expr(e->compare.left), expr(e->compare.right));
    case EXPR_UNARY: {
        bv_t a = expr(e->unary.operand);
        int cb = h_ncarg;
        carg(iconst(e->unary.op), 0);
        carg(a.lo, 1); carg(a.hi, 2); carg(a.t, 0);
        return bv_dcall("bcrt_un", cb, 4);
    }
    case EXPR_CALL:
        bc_not_yet(e->line, e->call.nargs ? "arrays and functions" : "functions");
        break;
    default:
        bc_not_yet(e->line, "this expression");
        break;
    }
    r.lo = dconst(0, &r.hi);
    r.t = iconst(VAL_SINGLE);
    return r;
}

/* the condition's truth, an int */
static int cond(expr_t *e) {
    bv_t c = expr(e);
    int cb = h_ncarg;
    carg(c.lo, 1); carg(c.hi, 2); carg(c.t, 0);
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
            if (e->type == EXPR_LITERAL && e->literal.type == VAL_STRING) {
                int idx = bc_intern_str(e->literal.sval->data, e->literal.sval->len);
                int cb, p = hi_emit(HI_SADDR, TY_INT, -1, -1, idx, NULL);
                cb = h_ncarg;
                carg(p, 0); carg(iconst(e->literal.sval->len), 0);
                call_end(cb, 2, "bcrt_print_lit", NULL);
            } else {
                bv_t v = expr(e);
                int cb = h_ncarg;
                carg(v.lo, 1); carg(v.hi, 2); carg(v.t, 0);
                call_end(cb, 3, "bcrt_print_num", NULL);
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
    if (e->type == EXPR_VARIABLE && !builtin_exists(e->var.name))
        var_get(e->var.name, e->var.var_type, e->line);
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
    entry = new_blk();
    begin_blk(entry);
    prescan(bc_program);
    for (int i = 0; i < bc_nvars; i++) var_zero(&bc_vars[i]);
    call_void("bcrt_start");
    bc_stmts(bc_program);
    if (bc_live) call_void("bcrt_end");
    if (bc_live) hi_emit(HI_RET, TY_INT, iconst(0), -1, 0, NULL);
    bc_live = 0;
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
