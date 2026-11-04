/* bc.c -- s32-bc, SLOW BASIC compiled the QuickBASIC way: the program's
 * shape fixed before it runs, lowered to HIR, and stage08's backend (a
 * copy, hir/) making SLOW-32 assembly.  docs/plans/basic-compiler.md, "A
 * compiler that decides up front".
 *
 *     s32-bc prog.bas prog.s
 *
 * The front end is sbasic's: its loader, lexer and parser (linked in
 * from sbasic/src), and its checker (strict.c) -- a program the
 * compiler takes is one QuickBASIC loads.  Where the program fixes a
 * value's type, the compiler computes it itself, natively, as value.c
 * does (an INTEGER's + with its Overflow inline, a SINGLE's * on the
 * hardware's doubles), and reaches an array's elements through a cell
 * of their address; elsewhere values are computed the interpreter's
 * way by its own code, through bc_rt.c, a value crossing as a double
 * and a run-time type.  Variables, labels and loops are the compiler's
 * own.  What it does not do yet it says, and stops ("not yet: ...").
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
/*
 * Where the program fixes a value's type, it has a static one (st), and
 * the value lives natively: an INTEGER or LONG is its int in lo, a
 * SINGLE or DOUBLE its double's words in lo and hi (a SINGLE unrounded,
 * as the interpreter keeps one between operators), a STRING its
 * sb_string_t in lo.  Its type is then a constant, and t is not made
 * until a runtime function wants the three words (dynv).  Where the
 * type is the run time's (st == ST_DYN), the three words are the value.
 * A constant (k) has its number in kv as well, so its conversions fold.
 */
#define ST_DYN (-1)
typedef struct { int lo, hi, t; int st, k; double kv; } bv_t;

static int st_int(int st) { return st == VAL_INTEGER || st == VAL_LONG; }
static int st_flt(int st) { return st == VAL_SINGLE || st == VAL_DOUBLE; }
static int st_num(int st) { return st_int(st) || st_flt(st); }

static bv_t mk(int lo, int hi, int t, int st) {
    bv_t r;
    r.lo = lo; r.hi = hi; r.t = t; r.st = st; r.k = 0; r.kv = 0;
    return r;
}

static bv_t kint(long long v, int st) {
    bv_t r = mk(iconst((int)v), -1, -1, st);
    r.k = 1; r.kv = (double)v;
    return r;
}

static bv_t kdbl(double d, int st) {
    bv_t r;
    r = mk(0, 0, -1, st);
    r.lo = dconst(d, &r.hi);
    r.k = 1; r.kv = d;
    return r;
}

/* a call to a two-word (double) result helper of two words */
static int call_d1(const char *name, int a, int b, int *hi) {
    int cb = h_ncarg;
    carg(a, 0); carg(b, 0);
    return call_end(cb, 2, name, hi);
}

static int call_d2(const char *name, int alo, int ahi, int blo, int bhi, int *hi) {
    int cb = h_ncarg;
    carg(alo, 0); carg(ahi, 0); carg(blo, 0); carg(bhi, 0);
    return call_end(cb, 4, name, hi);
}

/* a number's double words (an integer's converted, a constant's folded) */
static void dwords(bv_t x, int *lo, int *hi) {
    if (st_int(x.st)) {
        if (x.k) { *lo = dconst(x.kv, hi); return; }
        int cb = h_ncarg;
        carg(x.lo, 0);
        *lo = call_end(cb, 1, "__fp64_cvt_itoD", hi);
        return;
    }
    *lo = x.lo;
    *hi = x.hi;
}

/* the three words a runtime function takes */
static bv_t dynv(bv_t x) {
    if (x.st == ST_DYN) return x;
    if (st_int(x.st)) dwords(x, &x.lo, &x.hi);
    else if (x.st == VAL_STRING) x.hi = iconst(0);
    x.t = iconst(x.st);
    x.st = ST_DYN;
    x.k = 0;
    return x;
}

/* (every argument made dynv before the call's first carg: a conversion
 * is a call, and a call's arguments are contiguous) */
static void carg_bv(bv_t x) {
    if (x.st != ST_DYN) { fprintf(stderr, "s32-bc: internal: a typed value passed\n"); exit(1); }
    carg(x.lo, 0); carg(x.hi, 0); carg(x.t, 0);
}

/* a call returning a value: its words in r1:r2, its type bcrt_type()'s */
static bv_t bv_call(const char *name, int cb, int nwords) {
    bv_t r = mk(0, 0, 0, ST_DYN);
    r.lo = call_end(cb, nwords, name, &r.hi);
    r.t = call_end(h_ncarg, 0, "bcrt_type", NULL);
    return r;
}

/* a run-time function's result whose type the program fixes: st's */
static bv_t typed(bv_t r, int st) {
    if (st_int(st)) r.lo = call_d1("__fp64_cvt_DtoI", r.lo, r.hi, NULL);
    r.st = st;
    return r;
}

/* --- run-time errors: one block each, the program's end -------------- */

static int bc_fail_err[4], bc_fail_blk[4], bc_nfail;

static int fail_blk(int err) {
    for (int i = 0; i < bc_nfail; i++)
        if (bc_fail_err[i] == err) return bc_fail_blk[i];
    bc_fail_err[bc_nfail] = err;
    bc_fail_blk[bc_nfail] = new_blk();
    return bc_fail_blk[bc_nfail++];
}

/* go on where ok is not 0; else the error */
static void guard(int ok, int err) {
    int cont = new_blk();
    hi_emit(HI_BRC, TY_VOID, ok, cont, fail_blk(err), NULL);
    bc_live = 0;
    begin_blk(cont);
}

static void fail_blocks(void) {
    for (int i = 0; i < bc_nfail; i++) {
        int cb;
        begin_blk(bc_fail_blk[i]);
        cb = h_ncarg;
        carg(iconst(bc_fail_err[i]), 0);
        call_end(cb, 1, "bcrt_err", NULL);
        hi_emit(HI_RET, TY_INT, iconst(0), -1, 0, NULL);
        bc_live = 0;
    }
}

static int bin(int op, int a, int b) { return hi_emit(op, TY_INT, a, b, 0, NULL); }

/* an INTEGER's range, -32768..32767, else Overflow */
static void check16(int r) {
    guard(bin(HI_SLTU, bin(HI_ADD, r, iconst(32768)), iconst(65536)), ERR_OVERFLOW);
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

/* a DEF FN's parameters, while its body is lowered: they shadow the
 * program's variables of their names (call_def_fn's scope of its own) */
static var_t *bc_scope[64];
static int bc_nscope;

static var_t *var_find(const char *name) {
    for (int i = bc_nscope - 1; i >= 0; i--)
        if (strcmp(bc_scope[i]->name, name) == 0) return bc_scope[i];
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

/* the type a name has by its suffix (resolve_var_type, no DEFtype) */
static int name_type(const char *name) {
    size_t n = strlen(name);
    switch (n ? name[n - 1] : 0) {
    case '$': return VAL_STRING;
    case '%': return VAL_INTEGER;
    case '&': return VAL_LONG;
    case '#': return VAL_DOUBLE;
    default: return VAL_SINGLE;
    }
}

/* a variable of a DEF FN's own: slots, outside the program's list */
static var_t *var_param(const char *name, int type) {
    var_t *v = malloc(sizeof(var_t));
    if (!v) { fprintf(stderr, "s32-bc: out of memory\n"); exit(1); }
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
    int x;
    if (v->type == VAL_DOUBLE)
        return mk(hi_emit(HI_LOAD, TY_INT, v->a, -1, 0, NULL),
                  hi_emit(HI_LOAD, TY_INT, v->ahi, -1, 0, NULL), -1, VAL_DOUBLE);
    if (v->type == VAL_STRING) {        /* a reference of the value's own */
        int cb;
        x = hi_emit(HI_LOAD, TY_INT, v->a, -1, 0, NULL);
        cb = h_ncarg;
        carg(x, 0);
        return mk(call_end(cb, 1, "bcrt_ref", NULL), -1, -1, VAL_STRING);
    }
    if (v->type == VAL_SINGLE) {
        bv_t r = mk(0, 0, -1, VAL_SINGLE);
        int cb;
        x = hi_emit(HI_LOAD, TY_FLOAT, v->a, -1, 0, NULL);
        cb = h_ncarg;
        carg(x, 0);
        r.lo = call_end(cb, 1, "__fp64_cvt_ftoD", &r.hi);
        return r;
    }
    return mk(hi_emit(HI_LOAD, TY_INT, v->a, -1, 0, NULL), -1, -1, v->type);
}

/* whether a double's words are a finite number of at most m (hi:lo the
 * largest's), as store_into's fabs(d) <= m */
static int in_range(int lo, int hi, unsigned mhi, unsigned mlo) {
    int a = bin(HI_AND, hi, iconst(0x7FFFFFFF));
    int lt = bin(HI_SLTU, a, iconst((int)mhi));
    if (mlo == 0xFFFFFFFFu) return lt;
    return bin(HI_OR, lt, bin(HI_AND, bin(HI_SEQ, a, iconst((int)mhi)),
                               bin(HI_SLEU, lo, iconst((int)mlo))));
}
#define FLT_MAX_HI 0x47EFFFFFu  /* 3.4028234663852886e38 */
#define FLT_MAX_LO 0xE0000000u
#define DBL_INF_HI 0x7FF00000u  /* below it: finite */

/* a float made an integer of type ty, as val_to_integer makes it: in
 * range or Overflow, a whole number as it is, else rounded half to even
 * -- and an INTEGER's 16 bits.  The truncation converted back is the
 * value only when the value is a whole number in range (fcvt.w.d's
 * result out of range is Unspecified, SPEC 4, but no int converts back
 * to a number out of range, nor to a NaN); anything else is bcrt_d2i's,
 * the range and val_rint. */
static bv_t flt_to_int(bv_t x, int ty) {
    int clo, chi, i, whole, frac, join, slot, cb;
    if (x.k && x.kv == (double)(int)x.kv && (ty == VAL_LONG || INT16_OK(x.kv)))
        return kint((int)x.kv, ty);
    i = call_d1("__fp64_cvt_DtoI", x.lo, x.hi, NULL);
    cb = h_ncarg;
    carg(i, 0);
    clo = call_end(cb, 1, "__fp64_cvt_itoD", &chi);
    whole = call_d2("__fp64_eq", clo, chi, x.lo, x.hi, NULL);
    slot = alloca_of(TY_INT, 4);
    hi_emit(HI_STORE, TY_INT, slot, i, 0, NULL);
    frac = new_blk();
    join = new_blk();
    hi_emit(HI_BRC, TY_VOID, whole, join, frac, NULL);
    bc_live = 0;
    begin_blk(frac);
    hi_emit(HI_STORE, TY_INT, slot, call_d1("bcrt_d2i", x.lo, x.hi, NULL), 0, NULL);
    goto_blk(join);
    begin_blk(join);
    i = hi_emit(HI_LOAD, TY_INT, slot, -1, 0, NULL);
    if (ty == VAL_INTEGER) check16(i);
    return mk(i, -1, -1, ty);
}

/* x as a variable of type ty holds it (store_into: rounding, Overflow,
 * Type mismatch), a value of static type ty.  A SINGLE's value is its
 * float as a double. */
static int to_float(bv_t x);
static bv_t coerce(bv_t x, int ty) {
    int cb, lo, hi;
    if (st_int(ty) && st_int(x.st)) {
        if (ty == VAL_INTEGER && x.st == VAL_LONG && !(x.k && INT16_OK(x.kv))) check16(x.lo);
        x.st = ty;
        return x;
    }
    if (st_int(ty) && st_flt(x.st)) return flt_to_int(x, ty);
    if (ty == VAL_DOUBLE && st_num(x.st)) {
        if (st_int(x.st)) {
            int k = x.k;
            double kv = x.kv;
            dwords(x, &lo, &hi);
            x = mk(lo, hi, -1, VAL_DOUBLE);
            x.k = k; x.kv = kv;
            return x;
        }
        if (x.st == VAL_DOUBLE && !x.k) guard(in_range(x.lo, x.hi, DBL_INF_HI, 0xFFFFFFFFu), ERR_OVERFLOW);
        x.st = VAL_DOUBLE;
        return x;
    }
    if (ty == VAL_SINGLE && st_num(x.st)) {
        bv_t r;
        if (x.k && x.kv <= 3.4028234663852886e38 && x.kv >= -3.4028234663852886e38)
            return kdbl((double)(float)x.kv, VAL_SINGLE);
        r = mk(0, 0, -1, VAL_SINGLE);
        lo = to_float(x);       /* (a call of its own: before this one's cb) */
        cb = h_ncarg;
        carg(lo, 0);
        r.lo = call_end(cb, 1, "__fp64_cvt_ftoD", &r.hi);
        return r;
    }
    if (ty == VAL_STRING && x.st == VAL_STRING) return x;
    x = dynv(x);                /* (a Type mismatch among them: bcrt_conv's) */
    cb = h_ncarg;
    carg_bv(x);
    carg(iconst(ty), 0);
    if (st_int(ty)) return mk(call_end(cb, 4, "bcrt_convi", NULL), -1, -1, ty);
    lo = call_end(cb, 4, "bcrt_conv", &hi);
    return mk(lo, hi, -1, ty);
}

/* x as a SINGLE variable's float */
static int to_float(bv_t x) {
    int w;
    if (st_int(x.st)) {
        if (x.k) {
            float f = (float)x.kv;
            int bits;
            memcpy(&bits, &f, 4);
            return hi_emit(HI_ICONST, TY_FLOAT, -1, -1, bits, NULL);
        }
        return hi_emit(HI_FCVT_ItoF, TY_FLOAT, x.lo, -1, 0, NULL);
    }
    if (st_flt(x.st)) {
        if (x.k && x.kv <= 3.4028234663852886e38 && x.kv >= -3.4028234663852886e38) {
            float f = (float)x.kv;
            int bits;
            memcpy(&bits, &f, 4);
            return hi_emit(HI_ICONST, TY_FLOAT, -1, -1, bits, NULL);
        }
        guard(in_range(x.lo, x.hi, FLT_MAX_HI, FLT_MAX_LO), ERR_OVERFLOW);
    } else
        x = coerce(x, VAL_SINGLE);      /* (bcrt_conv: the run time's checks) */
    w = call_d1("__fp64_cvt_DtoF", x.lo, x.hi, NULL);
    h_ty[w] = TY_FLOAT;
    return w;
}

/* x, of any type, stored as v holds it; a string variable lets its old
 * string go */
static void var_store(var_t *v, bv_t x) {
    if (v->type == VAL_SINGLE) {
        hi_emit(HI_STORE, TY_FLOAT, v->a, to_float(x), 0, NULL);
        return;
    }
    x = coerce(x, v->type);
    if (v->type == VAL_DOUBLE) {
        hi_emit(HI_STORE, TY_INT, v->a, x.lo, 0, NULL);
        hi_emit(HI_STORE, TY_INT, v->ahi, x.hi, 0, NULL);
        return;
    }
    if (v->type == VAL_STRING) {
        int old = hi_emit(HI_LOAD, TY_INT, v->a, -1, 0, NULL), cb;
        hi_emit(HI_STORE, TY_INT, v->a, x.lo, 0, NULL);
        cb = h_ncarg;
        carg(old, 0);
        call_end(cb, 1, "bcrt_unref", NULL);
        return;
    }
    hi_emit(HI_STORE, TY_INT, v->a, x.lo, 0, NULL);
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
static bv_t zero(void) { return kint(0, VAL_INTEGER); }

static int is(const char *a, const char *b) { return strcmp(a, b) == 0; }

/* the type of a builtin's result where builtin.c fixes it (a $ name's
 * a string; INT, FIX and ABS their argument's; val_integer of a value
 * that cannot pass 16 bits an INTEGER; a double a SINGLE unless a DOUBLE
 * went in, as builtin_call makes it...); ST_DYN where it does not */
static int builtin_type(const char *name, int n, const bv_t *a) {
    size_t l = strlen(name);
    if (l && name[l - 1] == '$') return VAL_STRING;
    if (is(name, "INT") || is(name, "FIX") || is(name, "ABS"))
        return n == 1 && st_num(a[0].st) ? a[0].st : ST_DYN;
    if (is(name, "SGN") || is(name, "LEN") || is(name, "ASC") || is(name, "INSTR") ||
        is(name, "INSTRREV") || is(name, "CINT")) return VAL_INTEGER;
    if (is(name, "CLNG")) return VAL_LONG;
    if (is(name, "RND") || is(name, "CSNG")) return VAL_SINGLE;
    if (is(name, "CDBL")) return VAL_DOUBLE;
    if (is(name, "SQR") || is(name, "SIN") || is(name, "COS") || is(name, "TAN") ||
        is(name, "ATN") || is(name, "EXP") || is(name, "LOG"))
        /* (builtin_call: a double result a SINGLE unless a DOUBLE went in) */
        return n == 1 && st_num(a[0].st) ? (a[0].st == VAL_DOUBLE ? VAL_DOUBLE : VAL_SINGLE) : ST_DYN;
    return ST_DYN;
}

/* a double of a double (floor, trunc: the interpreter's own) */
static bv_t dbl_fn(const char *fn, bv_t x, int st) {
    bv_t r = mk(0, 0, -1, st);
    r.lo = call_d1(fn, x.lo, x.hi, &r.hi);
    return r;
}

/* the builtins compiled code computes itself, as builtin.c does; the
 * argument a number of a fixed type (else NULL: the runtime's) */
static int builtin_native(const char *name, int n, bv_t *a, bv_t *r) {
    bv_t x = a[0];
    int lo, hi;
    if (is(name, "RND")) {      /* sb_rnd_of, the interpreter's generator */
        int cb;
        if (n == 1 && !st_num(x.st)) return 0;
        if (n == 1) dwords(x, &lo, &hi);
        else lo = dconst(0, &hi);
        cb = h_ncarg;
        carg(lo, 1); carg(hi, 2);
        carg(iconst(n), 0);
        *r = mk(0, 0, -1, VAL_SINGLE);
        r->lo = call_end(cb, 3, "bcrt_rnd", &r->hi);
        return 1;
    }
    if (n != 1 || !st_num(x.st)) return 0;
    if (is(name, "INT") || is(name, "FIX")) {
        *r = st_int(x.st) ? x : dbl_fn(is(name, "INT") ? "bcrt_floor" : "bcrt_trunc", x, x.st);
        return 1;
    }
    if (is(name, "ABS")) {
        if (st_flt(x.st)) {
            *r = mk(x.lo, bin(HI_AND, x.hi, iconst(0x7FFFFFFF)), -1, x.st);
            return 1;
        }
        *r = mk(bin(HI_SUB, bin(HI_XOR, x.lo, bin(HI_SRA, x.lo, iconst(31))),
                    bin(HI_SRA, x.lo, iconst(31))), -1, -1, x.st);
        if (x.st == VAL_INTEGER) check16(r->lo);      /* ABS(-32768): Overflow */
        else guard(bin(HI_SGE, r->lo, iconst(0)), ERR_OVERFLOW);
        return 1;
    }
    if (is(name, "SGN")) {
        int pos, neg;
        if (st_int(x.st)) {
            pos = bin(HI_SGT, x.lo, iconst(0));
            neg = bin(HI_SLT, x.lo, iconst(0));
        } else {
            int zlo, zhi;
            zlo = dconst(0, &zhi);
            pos = call_d2("__fp64_lt", zlo, zhi, x.lo, x.hi, NULL);
            neg = call_d2("__fp64_lt", x.lo, x.hi, zlo, zhi, NULL);
        }
        *r = mk(bin(HI_SUB, pos, neg), -1, -1, VAL_INTEGER);
        return 1;
    }
    if (is(name, "CINT") || is(name, "CLNG")) {     /* val_rint, then the range */
        int ty = is(name, "CINT") ? VAL_INTEGER : VAL_LONG;
        *r = st_int(x.st) ? coerce(x, ty) : flt_to_int(x, ty);
        return 1;
    }
    if (is(name, "CDBL")) {
        dwords(x, &lo, &hi);
        *r = mk(lo, hi, -1, VAL_DOUBLE);
        return 1;
    }
    if (is(name, "CSNG")) {     /* single_round: no range of its own */
        int w, cb;
        if (st_int(x.st)) w = hi_emit(HI_FCVT_ItoF, TY_FLOAT, x.lo, -1, 0, NULL);
        else {
            w = call_d1("__fp64_cvt_DtoF", x.lo, x.hi, NULL);
            h_ty[w] = TY_FLOAT;
        }
        cb = h_ncarg;
        carg(w, 0);
        *r = mk(0, 0, -1, VAL_SINGLE);
        r->lo = call_end(cb, 1, "__fp64_cvt_ftoD", &r->hi);
        return 1;
    }
    return 0;
}

static bv_t builtin_fn(expr_t *e, const char *name, int n, expr_t **args) {
    bv_t a[3], r;
    int cb, ty;
    if (n > 3 || strcmp(name, "TAB") == 0 || strcmp(name, "SPC") == 0) {   /* (TAB, SPC in a PRINT list: bc_print) */
        bc_not_yet(e->line, n > 3 ? "a builtin of more than three arguments" : "TAB and SPC outside PRINT");
        n = 0;
    }
    for (int i = 0; i < 3; i++) a[i] = i < n ? expr(args[i]) : zero();
    if (builtin_native(name, n, a, &r)) return r;
    ty = builtin_type(name, n, a);
    for (int i = 0; i < 3; i++) a[i] = dynv(a[i]);
    cb = h_ncarg;
    carg(saddr(name, (int)strlen(name)), 0);
    carg(iconst(n), 0);
    for (int i = 0; i < 3; i++) carg_bv(a[i]);
    r = bv_call("bcrt_fn", cb, 11);
    return ty == ST_DYN ? r : typed(r, ty);
}

static bv_t bin_call(const char *name, int op, bv_t a, bv_t b) {
    int cb;
    a = dynv(a);
    b = dynv(b);
    cb = h_ncarg;
    carg(iconst(op), 0);
    carg_bv(a);
    carg_bv(b);
    return bv_call(name, cb, 7);
}

/* name(i[, j[, k]]): the subscripts, evaluated left to right */
static void sub_args(int line, int n, expr_t **ix, bv_t *a) {
    if (n > 3) bc_not_yet(line, "arrays of more than three dimensions");
    for (int i = 0; i < 3; i++) a[i] = i < n && n <= 3 ? expr(ix[i]) : zero();
}

/* --- arrays: the interpreter's, their elements reached directly ----- */

/* each array name's cell in bcrt_acache (bc_rt.c) */
#define BC_ACELLS 256
#define BC_ACELL 24             /* gen, data, base, dims[3] */
#define BC_ELEM 12              /* a value_t: its type, then its number or string */
static char bc_anames[BC_ACELLS][64];
static int bc_nanames;

static int array_id(const char *name, int line) {
    for (int i = 0; i < bc_nanames; i++)
        if (strcmp(bc_anames[i], name) == 0) return i;
    if (bc_nanames == BC_ACELLS) { bc_not_yet(line, "more than 256 arrays"); return 0; }
    strncpy(bc_anames[bc_nanames], name, 63);
    bc_anames[bc_nanames][63] = '\0';
    return bc_nanames++;
}

static int ld(int addr, int off) {
    if (off) addr = hi_emit(HI_ADDI, TY_INT, addr, -1, off, NULL);
    return hi_emit(HI_LOAD, TY_INT, addr, -1, 0, NULL);
}

static void st(int addr, int off, int v) {
    if (off) addr = hi_emit(HI_ADDI, TY_INT, addr, -1, off, NULL);
    hi_emit(HI_STORE, TY_INT, addr, v, 0, NULL);
}

/* the address of name(sub...)'s element, as array_get finds it: the
 * subscripts made integers (val_to_integer), the array (or an implicit
 * one, eval_array_for) through its cell, each subscript in its
 * dimension (Subscript out of range) */
static int array_elem(int line, const char *name, int n, bv_t *sub) {
    int ix[3], id = array_id(name, line), cell, fast, slow, flat = 0, cb;
    if (n > 3) n = 0;           /* (refused: sub_args) */
    for (int i = 0; i < n; i++) ix[i] = coerce(sub[i], VAL_LONG).lo;
    cell = hi_emit(HI_ADDI, TY_INT, hi_emit(HI_GADDR, TY_INT, -1, -1, 0, "bcrt_acache"), -1,
                   id * BC_ACELL, NULL);
    fast = new_blk();
    slow = new_blk();
    hi_emit(HI_BRC, TY_VOID,
            bin(HI_SEQ, ld(hi_emit(HI_GADDR, TY_INT, -1, -1, 0, "array_generation"), 0), ld(cell, 0)),
            fast, slow, NULL);
    bc_live = 0;
    begin_blk(slow);
    cb = h_ncarg;
    carg(saddr(name, (int)strlen(name)), 0);
    carg(iconst(n), 0);
    carg(iconst(id), 0);
    carg(iconst(name_type(name)), 0);
    call_end(cb, 4, "bcrt_alook", NULL);
    goto_blk(fast);
    begin_blk(fast);
    {
        int base = ld(cell, 8);
        for (int i = 0; i < n; i++) {
            int dim = ld(cell, 12 + 4 * i), x = bin(HI_SUB, ix[i], base);
            guard(bin(HI_SLTU, x, dim), ERR_SUBSCRIPT_OUT_OF_RANGE);
            flat = i == 0 ? x : bin(HI_ADD, bin(HI_MUL, flat, dim), x);
        }
    }
    if (n == 0) flat = iconst(0);
    return bin(HI_ADD, ld(cell, 4), bin(HI_MUL, flat, iconst(BC_ELEM)));
}

/* name(sub...) = x: the element found, then x made its type (store_into) */
static void array_store(int line, const char *name, int n, bv_t *sub, bv_t x) {
    int ty = name_type(name), ea = array_elem(line, name, n, sub);
    x = coerce(x, ty);
    if (ty == VAL_STRING) {
        int old = ld(ea, 4), cb;
        st(ea, 4, x.lo);
        cb = h_ncarg;
        carg(old, 0);
        call_end(cb, 1, "bcrt_unref", NULL);
        return;
    }
    st(ea, 4, x.lo);
    if (st_flt(ty)) st(ea, 8, x.hi);
}

/* --- operators on values of fixed types: native, held to value.c ---- */

/* + - * of two integers (val_add...: int_result), Overflow past the
 * wider one's range */
static bv_t int_arith(int op, bv_t a, bv_t b) {
    int st = (a.st == VAL_LONG || b.st == VAL_LONG) ? VAL_LONG : VAL_INTEGER, r;
    if (a.k && b.k) {
        long long x = (long long)a.kv, y = (long long)b.kv;
        long long z = op == OP_ADD ? x + y : op == OP_SUB ? x - y : x * y;
        if (st == VAL_LONG ? (z >= -2147483648LL && z <= 2147483647LL) : INT16_OK(z))
            return kint(z, st);
    }
    r = bin(op == OP_ADD ? HI_ADD : op == OP_SUB ? HI_SUB : HI_MUL, a.lo, b.lo);
    if (st == VAL_INTEGER)              /* (16-bit operands: exact in 32) */
        check16(r);
    else if (op == OP_MUL)
        guard(bin(HI_SEQ, bin(HI_MULH, a.lo, b.lo), bin(HI_SRA, r, iconst(31))), ERR_OVERFLOW);
    else {
        int x = op == OP_ADD ? bin(HI_AND, bin(HI_XOR, a.lo, r), bin(HI_XOR, b.lo, r))
                             : bin(HI_AND, bin(HI_XOR, a.lo, b.lo), bin(HI_XOR, a.lo, r));
        guard(bin(HI_SGE, x, iconst(0)), ERR_OVERFLOW);
    }
    return mk(r, -1, -1, st);
}

/* + - * / where a float is among them (or /): the double the
 * interpreter computes (val_float: a DOUBLE if one is, else a SINGLE,
 * unrounded) */
static bv_t flt_arith(int op, bv_t a, bv_t b) {
    int st = (a.st == VAL_DOUBLE || b.st == VAL_DOUBLE) ? VAL_DOUBLE : VAL_SINGLE;
    int alo, ahi, blo, bhi;
    bv_t r;
    if (a.k && b.k && !(op == OP_DIV && b.kv == 0))
        return kdbl(op == OP_ADD ? a.kv + b.kv : op == OP_SUB ? a.kv - b.kv :
                    op == OP_MUL ? a.kv * b.kv : a.kv / b.kv, st);
    dwords(a, &alo, &ahi);
    dwords(b, &blo, &bhi);
    if (op == OP_DIV && !(b.k && b.kv != 0))
        guard(bin(HI_SNE, bin(HI_OR, blo, bin(HI_SLL, bhi, iconst(1))), iconst(0)),
              ERR_DIVISION_BY_ZERO);
    r = mk(0, 0, -1, st);
    r.lo = call_d2(op == OP_ADD ? "__fp64_add" : op == OP_SUB ? "__fp64_sub" :
                   op == OP_MUL ? "__fp64_mul" : "__fp64_div", alo, ahi, blo, bhi, &r.hi);
    return r;
}

static bv_t binary(int op, bv_t a, bv_t b) {
    if (st_int(a.st) && st_int(b.st)) {
        int st = (a.st == VAL_LONG || b.st == VAL_LONG) ? VAL_LONG : VAL_INTEGER;
        switch (op) {
        case OP_ADD: case OP_SUB: case OP_MUL:
            return int_arith(op, a, b);
        case OP_AND: return mk(bin(HI_AND, a.lo, b.lo), -1, -1, st);
        case OP_OR:  return mk(bin(HI_OR, a.lo, b.lo), -1, -1, st);
        case OP_XOR: return mk(bin(HI_XOR, a.lo, b.lo), -1, -1, st);
        case OP_IMP:
            return mk(bin(HI_OR, hi_emit(HI_BNOT, TY_INT, a.lo, -1, 0, NULL), b.lo), -1, -1, st);
        case OP_EQV:
            return mk(hi_emit(HI_BNOT, TY_INT, bin(HI_XOR, a.lo, b.lo), -1, 0, NULL), -1, -1, st);
        case OP_IDIV: case OP_MOD:      /* by a constant of neither 0 nor -1: in range */
            if (b.k && b.kv != 0 && b.kv != -1)
                return mk(bin(op == OP_IDIV ? HI_DIV : HI_REM, a.lo, b.lo), -1, -1, st);
            return typed(bin_call("bcrt_bin", op, a, b), st);
        default: break;
        }
    }
    if (st_num(a.st) && st_num(b.st)) {
        int fst = (a.st == VAL_DOUBLE || b.st == VAL_DOUBLE) ? VAL_DOUBLE : VAL_SINGLE;
        if (op == OP_ADD || op == OP_SUB || op == OP_MUL || op == OP_DIV)
            return flt_arith(op, a, b);
        if (op == OP_POW) return typed(bin_call("bcrt_bin", op, a, b), fst);
    }
    if (op == OP_STRCAT || (op == OP_ADD && a.st == VAL_STRING && b.st == VAL_STRING))
        return typed(bin_call("bcrt_bin", op, a, b), VAL_STRING);
    return bin_call("bcrt_bin", op, a, b);
}

/* a comparison's truth, 0 or 1 (val_compare) */
static int compare01(int op, bv_t a, bv_t b) {
    int alo, ahi, blo, bhi, cb;
    if (st_int(a.st) && st_int(b.st)) {
        switch (op) {
        case CMP_EQ: return bin(HI_SEQ, a.lo, b.lo);
        case CMP_NE: return bin(HI_SNE, a.lo, b.lo);
        case CMP_LT: return bin(HI_SLT, a.lo, b.lo);
        case CMP_GT: return bin(HI_SGT, a.lo, b.lo);
        case CMP_LE: return bin(HI_SLE, a.lo, b.lo);
        default:     return bin(HI_SGE, a.lo, b.lo);
        }
    }
    if (st_num(a.st) && st_num(b.st)) {
        dwords(a, &alo, &ahi);
        dwords(b, &blo, &bhi);
        switch (op) {
        case CMP_EQ: return call_d2("__fp64_eq", alo, ahi, blo, bhi, NULL);
        case CMP_NE: return bin(HI_XOR, call_d2("__fp64_eq", alo, ahi, blo, bhi, NULL), iconst(1));
        case CMP_LT: return call_d2("__fp64_lt", alo, ahi, blo, bhi, NULL);
        case CMP_GT: return call_d2("__fp64_lt", blo, bhi, alo, ahi, NULL);
        case CMP_LE: return call_d2("__fp64_le", alo, ahi, blo, bhi, NULL);
        default:     return call_d2("__fp64_le", blo, bhi, alo, ahi, NULL);
        }
    }
    a = dynv(a);
    b = dynv(b);
    cb = h_ncarg;
    carg(iconst(op), 0);
    carg_bv(a);
    carg_bv(b);
    return call_end(cb, 7, "bcrt_cmpi", NULL);
}

static bv_t unary(int op, bv_t a) {
    if (op == OP_NEG && st_int(a.st)) {
        if (a.k && (a.st == VAL_LONG ? a.kv != -2147483648.0 : a.kv != -32768)) return kint(-(long long)a.kv, a.st);
        if (a.st == VAL_INTEGER) {
            int r = hi_emit(HI_NEG, TY_INT, a.lo, -1, 0, NULL);
            check16(r);
            return mk(r, -1, -1, a.st);
        }
        guard(bin(HI_SNE, a.lo, iconst((int)0x80000000u)), ERR_OVERFLOW);
        return mk(hi_emit(HI_NEG, TY_INT, a.lo, -1, 0, NULL), -1, -1, a.st);
    }
    if (op == OP_NEG && st_flt(a.st)) {
        if (a.k) return kdbl(-a.kv, a.st);
        return mk(a.lo, bin(HI_XOR, a.hi, iconst((int)0x80000000u)), -1, a.st);
    }
    if (op == OP_NOT && st_int(a.st))
        return mk(hi_emit(HI_BNOT, TY_INT, a.lo, -1, 0, NULL), -1, -1, a.st);
    {
        int cb;
        a = dynv(a);
        cb = h_ncarg;
        carg(iconst(op), 0);
        carg_bv(a);
        return bv_call("bcrt_un", cb, 4);
    }
}

/* --- DEF FN, inlined where it is called --------------------------- */

static stmt_t **bc_fns;
static int bc_nfns, bc_capfns;
static const char *bc_fn_open[16];     /* the DEF FNs being inlined, outermost first */
static int bc_nfn_open;

static stmt_t *def_fn_find(const char *name) {
    for (int i = 0; i < bc_nfns; i++)
        if (strcmp(bc_fns[i]->def_fn.name, name) == 0) return bc_fns[i];
    return NULL;
}

/* FNname(args), as call_def_fn: each argument evaluated in the caller's
 * scope and stored into its parameter (typed by its name), the body
 * evaluated with the parameters in a scope of their own, the result made
 * the type of the function's name.  The body is lowered here, at the
 * call: every DEF FN is defined before the program runs (a use before
 * the DEF is fine -- the user's ruling, QB refusing it). */
static bv_t def_fn_call(expr_t *e, const char *name, int nargs, expr_t **args) {
    stmt_t *d = def_fn_find(name);
    var_t *params[8];
    bv_t r;
    int saved;
    if (!d) { bc_not_yet(e->line, "a DEF FN that is not defined"); return zero(); }
    for (int i = 0; i < bc_nfn_open; i++)
        if (strcmp(bc_fn_open[i], name) == 0) {
            bc_not_yet(e->line, "a DEF FN that calls itself");
            return zero();
        }
    if (nargs != d->def_fn.nparams || bc_nfn_open >= 16 || bc_nscope + nargs > 64) {
        bc_not_yet(e->line, "this DEF FN call");
        return zero();
    }
    for (int i = 0; i < nargs; i++) {
        params[i] = var_param(d->def_fn.params[i], name_type(d->def_fn.params[i]));
        var_store(params[i], expr(args[i]));
    }
    saved = bc_nscope;
    for (int i = 0; i < nargs; i++) bc_scope[bc_nscope++] = params[i];
    bc_fn_open[bc_nfn_open++] = name;
    r = expr(d->def_fn.body);
    bc_nfn_open--;
    bc_nscope = saved;
    return coerce(r, name_type(name));
}

static bv_t array_read(expr_t *e) {
    bv_t a[3];
    int ty = name_type(e->call.name), ea;
    sub_args(e->line, e->call.nargs, e->call.args, a);
    ea = array_elem(e->line, e->call.name, e->call.nargs, a);
    if (ty == VAL_STRING) {
        int cb = h_ncarg;
        carg(ld(ea, 4), 0);
        return mk(call_end(cb, 1, "bcrt_ref", NULL), -1, -1, VAL_STRING);
    }
    return mk(ld(ea, 4), st_flt(ty) ? ld(ea, 8) : -1, -1, ty);
}

static int compare_expr(expr_t *e) {
    bv_t a = expr(e->compare.left), b = expr(e->compare.right);
    return compare01(e->compare.op, a, b);
}

static bv_t expr(expr_t *e) {
    switch (e->type) {
    case EXPR_LITERAL:
        if (e->literal.type == VAL_STRING) {
            int cb = h_ncarg;
            carg(saddr(e->literal.sval->data, e->literal.sval->len), 0);
            carg(iconst(e->literal.sval->len), 0);
            return mk(call_end(cb, 2, "bcrt_str_lit", NULL), -1, -1, VAL_STRING);
        }
        if (st_int(e->literal.type)) return kint(e->literal.ival, e->literal.type);
        return kdbl(e->literal.dval, e->literal.type);
    case EXPR_VARIABLE:
        if (is_builtin_call(e->var.name))       /* RND, TIMER... with no ( ) */
            return builtin_fn(e, e->var.name, 0, NULL);
        if (strncmp(e->var.name, "FN", 2) == 0 && def_fn_find(e->var.name))
            return def_fn_call(e, e->var.name, 0, NULL);  /* DEF FNM = 123: FNM calls it */
        return var_load(var_get(e->var.name, e->var.var_type, e->line));
    case EXPR_BINARY: {
        bv_t a = expr(e->binary.left), b = expr(e->binary.right);
        return binary(e->binary.op, a, b);
    }
    case EXPR_COMPARE:          /* -1 or 0, an INTEGER */
        return mk(hi_emit(HI_NEG, TY_INT, compare_expr(e), -1, 0, NULL), -1, -1, VAL_INTEGER);
    case EXPR_UNARY:
        return unary(e->unary.op, expr(e->unary.operand));
    case EXPR_CALL:
        if (is_builtin_call(e->call.name))
            return builtin_fn(e, e->call.name, e->call.nargs, e->call.args);
        if (strncmp(e->call.name, "FN", 2) == 0) return def_fn_call(e, e->call.name, e->call.nargs, e->call.args);
        return array_read(e);
    default:
        bc_not_yet(e->line, "this expression");
        break;
    }
    return zero();
}

/* the condition's truth, an int (val_is_true) */
static int cond(expr_t *e) {
    bv_t c;
    int cb;
    if (e->type == EXPR_COMPARE) return compare_expr(e);
    c = expr(e);
    if (st_int(c.st)) return c.lo;
    if (st_flt(c.st))           /* (0 and -0 false) */
        return bin(HI_SNE, bin(HI_OR, c.lo, bin(HI_SLL, c.hi, iconst(1))), iconst(0));
    c = dynv(c);
    cb = h_ncarg;
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
typedef struct { int st, k; double kv; int a, ahi; } slot_t;
typedef struct {
    var_t *v;
    slot_t lim, stp;
    int body, exit;
} for_t;

static void bc_stmts(stmt_t *s);

/* GOSUB's return points, and the block every RETURN goes to: it pops
 * one (bcrt_return) and jumps through a table of them, made last, when
 * all are known */
static int *bc_rets;
static int bc_nrets, bc_caprets;
static int bc_ret_blk = -1;

/* a return point: its number; *back its block */
static int ret_point(int *back) {
    if (bc_nrets == bc_caprets) {
        bc_caprets = bc_caprets ? bc_caprets * 2 : 32;
        bc_rets = realloc(bc_rets, (size_t)bc_caprets * sizeof(int));
        if (!bc_rets) { fprintf(stderr, "s32-bc: out of memory\n"); exit(1); }
    }
    *back = new_blk();
    bc_rets[bc_nrets] = *back;
    return bc_nrets++;
}

/* push return point k, and go to blk */
static void gosub_jump(int k, int blk) {
    int cb = h_ncarg;
    carg(iconst(k), 0);
    call_end(cb, 1, "bcrt_gosub", NULL);
    goto_blk(blk);
}

static void gosub_to(int blk) {
    int back, k = ret_point(&back);
    gosub_jump(k, blk);
    begin_blk(back);
}

/* ON i GOTO / GOSUB l1, l2, ...: i made an integer as the interpreter
 * makes it; 1..n jumps (a GOSUB to the statement after), anything else
 * goes on */
static void on_branch(stmt_t *s, int is_gosub) {
    bv_t x = dynv(expr(s->on_branch.index));
    int n = s->on_branch.nlabels, cb, i, k, ok, cont = new_blk(), tab = new_blk(), back = -1, rk = -1;
    int *targets = malloc((size_t)(n ? n : 1) * sizeof(int));
    if (!targets) { fprintf(stderr, "s32-bc: out of memory\n"); exit(1); }
    cb = h_ncarg;
    carg_bv(x);
    i = call_end(cb, 3, "bcrt_int", NULL);
    k = hi_emit(HI_ADDI, TY_INT, i, -1, -1, NULL);
    ok = hi_emit(HI_SLTU, TY_INT, k, iconst(n), 0, NULL);
    hi_emit(HI_BRC, TY_VOID, ok, tab, cont, NULL);
    bc_live = 0;
    if (is_gosub) rk = ret_point(&back);
    for (int j = 0; j < n; j++) {
        int lb = label_blk(s->on_branch.labels[j]);
        if (!is_gosub) { targets[j] = lb; continue; }
        targets[j] = new_blk();
        begin_blk(targets[j]);
        gosub_jump(rk, lb);
    }
    begin_blk(tab);
    if (n > 0) hi_emit_jmptab(k, cont, targets, n);
    else hi_emit(HI_BR, TY_VOID, -1, -1, cont, NULL);
    bc_live = 0;
    free(targets);
    if (is_gosub) {             /* the return comes back to where going on goes */
        begin_blk(back);
        goto_blk(cont);
    }
    begin_blk(cont);
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
                bv_t a = dynv(expr(e->call.args[0]));
                cb = h_ncarg;
                carg(saddr(e->call.name, 3), 0);
                carg_bv(a);
                call_end(cb, 4, "bcrt_print_tab", NULL);
            } else {
                bv_t v = dynv(expr(e));
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

/* Each FOR's limit and step have two words of their own, 0 from the
 * start: a NEXT whose FOR has not run (a GOTO into its body) finds the
 * limit 0, as QBasic's does, and the step as written when it is a
 * literal (compiled into the NEXT), else 0 (eval.c OP_NEXT, NBS P055) */
typedef struct { stmt_t *s; int w[4]; } forslots_t;
static forslots_t *bc_forslots;
static int bc_nforslots, bc_capforslots;

static void forslots_add(stmt_t *s) {
    forslots_t *f;
    if (bc_nforslots == bc_capforslots) {
        bc_capforslots = bc_capforslots ? bc_capforslots * 2 : 16;
        bc_forslots = realloc(bc_forslots, (size_t)bc_capforslots * sizeof(forslots_t));
        if (!bc_forslots) { fprintf(stderr, "s32-bc: out of memory\n"); exit(1); }
    }
    f = &bc_forslots[bc_nforslots++];
    f->s = s;
    for (int i = 0; i < 4; i++) f->w[i] = alloca_of(TY_INT, 4);
}

static void forslots_zero(void) {
    for (int i = 0; i < bc_nforslots; i++)
        for (int j = 0; j < 4; j++)
            hi_emit(HI_STORE, TY_INT, bc_forslots[i].w[j], iconst(0), 0, NULL);
}

static int *forslots_of(stmt_t *s) {
    for (int i = 0; i < bc_nforslots; i++)
        if (bc_forslots[i].s == s) return bc_forslots[i].w;
    fprintf(stderr, "s32-bc: internal: a FOR not prescanned\n");
    exit(1);
}

/* a FOR's limit or step, kept as the interpreter keeps them (a double,
 * val_to_double: a string a Type mismatch) -- here in its own type,
 * which is the same number -- in its words; a literal step kept a
 * constant (lit) */
static slot_t slot_of(bv_t x, const int *w, int lit) {
    slot_t s;
    if (!st_num(x.st)) x = coerce(x, VAL_DOUBLE);
    s.st = x.st; s.k = lit && x.k; s.kv = x.kv; s.a = w[0]; s.ahi = -1;
    if (s.k) return s;
    if (x.k && st_int(x.st)) x.lo = iconst((int)x.kv);
    hi_emit(HI_STORE, TY_INT, s.a, x.lo, 0, NULL);
    if (st_flt(x.st)) {
        s.ahi = w[1];
        hi_emit(HI_STORE, TY_INT, s.ahi, x.hi, 0, NULL);
    }
    return s;
}

static bv_t slot_load(const slot_t *s) {
    if (s->k) return st_int(s->st) ? kint((long long)s->kv, s->st) : kdbl(s->kv, s->st);
    return mk(hi_emit(HI_LOAD, TY_INT, s->a, -1, 0, NULL),
              s->ahi < 0 ? -1 : hi_emit(HI_LOAD, TY_INT, s->ahi, -1, 0, NULL), -1, s->st);
}

/* whether the loop runs with its variable as it is (for_in_range) */
static int for_runs(for_t *f) {
    bv_t x = var_load(f->v), lim = slot_load(&f->lim), stp;
    int up, le, ge;
    if (f->stp.k) return compare01(f->stp.kv >= 0 ? CMP_LE : CMP_GE, x, lim);
    stp = slot_load(&f->stp);
    up = compare01(CMP_GE, stp, kint(0, VAL_INTEGER));
    le = compare01(CMP_LE, x, lim);
    ge = compare01(CMP_GE, x, lim);
    return bin(HI_OR, bin(HI_AND, up, le), bin(HI_AND, bin(HI_XOR, up, iconst(1)), ge));
}

/* FOR v = start TO limit [STEP step], as the interpreter's for_start:
 * the three evaluated once, in that order; limit and step kept in the
 * FOR's own slots; the variable set as an assignment sets it; the loop
 * skipped when it starts out of range */
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
    stp = s->for_stmt.step ? expr(s->for_stmt.step) : kint(1, VAL_INTEGER);
    f = &bc_fors[bc_nfors++];
    f->v = v;
    f->lim = slot_of(lim, forslots_of(s), 0);
    f->stp = slot_of(stp, forslots_of(s) + 2,
                     !s->for_stmt.step || s->for_stmt.step->type == EXPR_LITERAL);
    var_store(v, start);
    runs = for_runs(f);
    f->body = new_blk();
    f->exit = new_blk();
    hi_emit(HI_BRC, TY_VOID, runs, f->body, f->exit, NULL);
    bc_live = 0;
    begin_blk(f->body);
}

/* NEXT [v, ...]: each closes the innermost open FOR (the checker made
 * sure it is the one named), steps its variable where it lives
 * (for_next: an integer's sum stored if it is one in range, else
 * stored as an assignment stores it; a SINGLE's rounded, a DOUBLE's as
 * it is), and loops while it is in range */
static void bc_next(stmt_t *s) {
    int k = s->next_stmt.nvars ? s->next_stmt.nvars : 1;
    for (int i = 0; i < k; i++) {
        for_t *f;
        bv_t cur, stp, nv;
        int runs;
        if (bc_nfors == 0) { bc_error(s->line, "NEXT without FOR"); return; }
        f = &bc_fors[--bc_nfors];
        cur = var_load(f->v);
        stp = slot_load(&f->stp);
        if (st_int(f->v->type)) {
            if (st_int(stp.st)) {
                stp.st = VAL_LONG;      /* (the sum in 32 bits, then the variable's range) */
                nv = int_arith(OP_ADD, cur, stp);
            } else
                nv = flt_arith(OP_ADD, cur, stp);
            var_store(f->v, nv);
        } else {
            int w;
            nv = flt_arith(OP_ADD, cur, stp);
            if (f->v->type == VAL_DOUBLE) {
                hi_emit(HI_STORE, TY_INT, f->v->a, nv.lo, 0, NULL);
                hi_emit(HI_STORE, TY_INT, f->v->ahi, nv.hi, 0, NULL);
            } else {
                w = call_d1("__fp64_cvt_DtoF", nv.lo, nv.hi, NULL);
                h_ty[w] = TY_FLOAT;
                hi_emit(HI_STORE, TY_FLOAT, f->v->a, w, 0, NULL);
            }
        }
        runs = for_runs(f);
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
    for (int i = 0; i < 3; i++) a[i] = dynv(a[i]);
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

/* a value taken back from the runtime (INPUT, READ), stored in a
 * variable or an array element, the target as an assignment's */
static void store_target(int line, const char *name, int type, expr_t *target, bv_t x) {
    if (target && target->type == EXPR_CALL) {
        bv_t a[3];
        sub_args(line, target->call.nargs, target->call.args, a);
        array_store(line, target->call.name, target->call.nargs, a, x);
        return;
    }
    if (target) { bc_not_yet(line, "this INPUT or READ target"); return; }
    var_store(var_get(name, type, line), x);
}

/* --- DATA: the program's items, in the order the interpreter's scan
 * pools them (every block of the main program's text), registered as
 * it starts; a label's offset is the items before it, for RESTORE ---- */

typedef struct { char name[64]; int off; } dlabel_t;
static dlabel_t *bc_dlabels;
static int bc_ndlabels, bc_capdlabels, bc_ndata;

static bv_t literal(const value_t *v) {
    bv_t r = mk(0, 0, 0, ST_DYN);
    if (v->type == VAL_STRING) {
        int cb = h_ncarg;
        carg(saddr(v->sval->data, v->sval->len), 0);
        carg(iconst(v->sval->len), 0);
        r.lo = call_end(cb, 2, "bcrt_str_lit", NULL);
        r.hi = iconst(0);
        r.t = iconst(VAL_STRING);
    } else {
        double d = 0;
        val_to_double(v, &d);
        r.lo = dconst(d, &r.hi);
        r.t = iconst(v->type);
    }
    return r;
}

static void pool_data(stmt_t *s) {
    for (; s; s = s->next) {
        if (s->type == STMT_LABEL) {
            if (bc_ndlabels == bc_capdlabels) {
                bc_capdlabels = bc_capdlabels ? bc_capdlabels * 2 : 64;
                bc_dlabels = realloc(bc_dlabels, (size_t)bc_capdlabels * sizeof(dlabel_t));
                if (!bc_dlabels) { fprintf(stderr, "s32-bc: out of memory\n"); exit(1); }
            }
            strncpy(bc_dlabels[bc_ndlabels].name, s->label.name, 63);
            bc_dlabels[bc_ndlabels].name[63] = '\0';
            bc_dlabels[bc_ndlabels++].off = bc_ndata;
        }
        if (s->type == STMT_DATA)
            for (int j = 0; j < s->data_stmt.nvalues; j++) {
                int cb;
                bv_t x = literal(&s->data_stmt.values[j]);
                cb = h_ncarg;
                carg_bv(x);
                carg(iconst(s->data_stmt.quoted[j]), 0);
                call_end(cb, 4, "bcrt_data", NULL);
                bc_ndata++;
            }
        switch (s->type) {
        case STMT_FOR: pool_data(s->for_stmt.body); break;
        case STMT_IF: pool_data(s->if_stmt.then_body); pool_data(s->if_stmt.else_body); break;
        case STMT_WHILE: pool_data(s->while_stmt.body); break;
        case STMT_DO_LOOP: pool_data(s->do_loop.body); break;
        case STMT_SELECT:
            for (case_clause_t *c = s->select_stmt.clauses; c; c = c->next) pool_data(c->body);
            break;
        default: break;
        }
    }
}

static int data_offset(const char *label) {
    for (int i = 0; i < bc_ndlabels; i++)
        if (strcmp(bc_dlabels[i].name, label) == 0) return bc_dlabels[i].off;
    return 0;               /* (the checker refused an undefined label) */
}

static void bc_input(stmt_t *s) {
    char types[65];
    int n = s->input.nvars, cb;
    if (n > 64) { bc_not_yet(s->line, "an INPUT of more than 64"); return; }
    for (int i = 0; i < n; i++) {
        int t = s->input.vartypes[i];
        if (!s->input.targets[i])       /* a variable: a string's, or a number (exec_input) */
            t = var_get(s->input.varnames[i], s->input.vartypes[i], s->line)->type == VAL_STRING
                ? VAL_STRING : VAL_DOUBLE;
        types[i] = (char)('A' + t);
    }
    types[n] = '\0';
    cb = h_ncarg;
    carg(s->input.prompt ? saddr(s->input.prompt, (int)strlen(s->input.prompt)) : iconst(0), 0);
    carg(iconst(s->input.no_question), 0);
    carg(iconst(s->input.same_line), 0);
    carg(iconst(n), 0);
    carg(saddr(types, n), 0);
    call_end(cb, 5, "bcrt_input", NULL);
    for (int i = 0; i < n; i++) {
        bv_t x;
        cb = h_ncarg;
        carg(iconst(i), 0);
        x = bv_call("bcrt_input_take", cb, 1);
        store_target(s->line, s->input.varnames[i], s->input.vartypes[i], s->input.targets[i], x);
    }
}

static void bc_read(stmt_t *s) {
    for (int i = 0; i < s->read_stmt.nvars; i++) {
        bv_t x;
        int cb = h_ncarg;
        carg(iconst(s->read_stmt.vartypes[i]), 0);
        x = bv_call("bcrt_read", cb, 1);
        store_target(s->line, s->read_stmt.varnames[i], s->read_stmt.vartypes[i],
                     s->read_stmt.targets[i], x);
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
    case STMT_DEF_FN:
        break;                  /* (inlined where it is called) */
    case STMT_ON_GOTO:
        on_branch(s, 0);
        break;
    case STMT_ON_GOSUB:
        on_branch(s, 1);
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
        sub_args(s->line, s->array_assign.nindices, s->array_assign.indices, a);
        x = expr(s->array_assign.value);
        array_store(s->line, s->array_assign.name, s->array_assign.nindices, a, x);
        break;
    }
    case STMT_OPTION_BASE: {
        int cb = h_ncarg;
        carg(iconst(s->option_base.base), 0);
        call_end(cb, 1, "bcrt_option_base", NULL);
        break;
    }
    case STMT_RANDOMIZE: {      /* the seed a double (val_to_double), or asked for */
        int lo, hi, cb;
        if (s->randomize.seed) {
            bv_t x = expr(s->randomize.seed);
            if (!st_num(x.st)) x = coerce(x, VAL_DOUBLE);
            dwords(x, &lo, &hi);
        } else
            lo = dconst(0, &hi);
        cb = h_ncarg;
        carg(iconst(s->randomize.seed != NULL), 0);
        carg(lo, 1); carg(hi, 2);
        call_end(cb, 3, "bcrt_randomize", NULL);
        break;
    }
    case STMT_INPUT:
        bc_input(s);
        break;
    case STMT_READ:
        bc_read(s);
        break;
    case STMT_DATA:
        break;                  /* (pooled as the program starts) */
    case STMT_RESTORE: {
        int cb = h_ncarg;
        carg(iconst(s->restore_stmt.label[0] ? data_offset(s->restore_stmt.label) : 0), 0);
        call_end(cb, 1, "bcrt_restore", NULL);
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
    if (e->type == EXPR_VARIABLE && !is_builtin_call(e->var.name) &&
        !(strncmp(e->var.name, "FN", 2) == 0 && def_fn_find(e->var.name)))
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

/* the DEF FNs, before anything else: a name that is one is no variable
 * (DEF FNM = 123: FNM calls it), wherever its DEF is */
static void find_fns(stmt_t *s) {
    for (; s; s = s->next) {
        if (s->type == STMT_DEF_FN && !def_fn_find(s->def_fn.name)) {
            if (bc_nfns == bc_capfns) {
                bc_capfns = bc_capfns ? bc_capfns * 2 : 16;
                bc_fns = realloc(bc_fns, (size_t)bc_capfns * sizeof(stmt_t *));
                if (!bc_fns) { fprintf(stderr, "s32-bc: out of memory\n"); exit(1); }
            }
            bc_fns[bc_nfns++] = s;
        }
        switch (s->type) {
        case STMT_FOR: find_fns(s->for_stmt.body); break;
        case STMT_IF: find_fns(s->if_stmt.then_body); find_fns(s->if_stmt.else_body); break;
        case STMT_WHILE: find_fns(s->while_stmt.body); break;
        case STMT_DO_LOOP: find_fns(s->do_loop.body); break;
        case STMT_SELECT:
            for (case_clause_t *c = s->select_stmt.clauses; c; c = c->next) find_fns(c->body);
            break;
        default: break;
        }
    }
}

static void prescan(stmt_t *s) {
    for (; s; s = s->next) {
        if (s->type == STMT_DEF_FN) continue;   /* (its parameters are no program variables) */
        stmt_exprs(s, prescan_top, NULL);
        switch (s->type) {
        case STMT_ASSIGN: var_get(s->assign.name, s->assign.var_type, s->line); break;
        case STMT_FOR:
            var_get(s->for_stmt.var_name, s->for_stmt.var_type, s->line);
            forslots_add(s);
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
    bc_nfail = 0;
    bc_nforslots = 0;
    bc_nanames = 0;
    entry = new_blk();
    begin_blk(entry);
    bc_nshadow = 0;
    bc_nfns = 0;
    bc_nscope = 0;
    bc_nfn_open = 0;
    preshadow(bc_program);
    find_fns(bc_program);
    prescan(bc_program);
    for (int i = 0; i < bc_nvars; i++) var_zero(&bc_vars[i]);
    forslots_zero();
    call_void("bcrt_start");
    /* the static arrays, then the OPTION BASE the text ends with in
     * force until an OPTION BASE runs (the interpreter's scan leaves it) */
    bc_nstatics = 0;
    bc_base = 0;
    hoist_dims(bc_program);
    bc_ndlabels = 0;
    bc_ndata = 0;
    pool_data(bc_program);
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
    fail_blocks();
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
