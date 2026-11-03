/* bc_rt.c -- the runtime of the programs s32-bc compiles.
 *
 * Compiled for SLOW-32 and linked with the interpreter's own objects
 * (all but its main): every operator, conversion, builtin and PRINT item
 * goes through the interpreter's code (eval_binary, eval_store,
 * builtin_call, print_value...), so a compiled program computes and
 * prints what the interpreter does, to the byte, by construction.
 *
 * A value crosses from compiled code as three words: lo, hi and its
 * val_type_t.  A number's lo and hi are the words of its double (an
 * INTEGER or LONG is exact in one; a SINGLE intermediate is an
 * unrounded double, as the interpreter keeps it); a string's lo is its
 * sb_string_t.  The type travels at run time because some results'
 * types depend on their values (5.5 \ 2 is an INTEGER, 100000.5 \ 2 a
 * LONG).  A result comes back as a 64-bit pair (lo in r1, hi in r2 --
 * a double's convention), its type from bcrt_type().
 *
 * Strings: every string value compiled code holds is a reference of its
 * own, and every function here releases the strings it is given -- a
 * variable's load takes a reference (bcrt_ref), a store releases the
 * string the variable held.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "value.h"
#include "error.h"
#include "ast.h"
#include "eval.h"
#include "builtin.h"
#include "fileio.h"
#include "terminal.h"

extern int console_width;

typedef long long pair_t;       /* lo in the low word, hi in the high */

static int bcrt_last;           /* the type of the last result */

static void bcrt_fail(error_t e) {
    error_print(e, 0);
    fflush(stdout);
    exit(0);
}

/* a value from its three words, the string's reference passing to it */
static value_t take(int lo, int hi, int t) {
    value_t v;
    union { double d; int w[2]; } u;
    v.type = (val_type_t)t;
    if (t == VAL_STRING) {
        v.sval = (sb_string_t *)(intptr_t)lo;
        return v;
    }
    u.w[0] = lo;
    u.w[1] = hi;
    if (t == VAL_INTEGER || t == VAL_LONG) v.ival = (int)u.d;
    else v.dval = u.d;
    return v;
}

/* a value as its words, its string's reference passing to the caller */
static pair_t give(const value_t *v) {
    union { double d; int w[2]; } u;
    bcrt_last = v->type;
    if (v->type == VAL_STRING)
        return (pair_t)(unsigned)(intptr_t)v->sval;
    u.d = (v->type == VAL_INTEGER || v->type == VAL_LONG) ? (double)v->ival : v->dval;
    return (pair_t)(unsigned)u.w[0] | ((pair_t)(unsigned)u.w[1] << 32);
}

/* as eval_program_with starts a program */
void bcrt_start(void) {
    builtin_rnd_reset();
    fileio_init();
    sb_term_init();
    print_col = 0;
    print_row = 1;
}

void bcrt_end(void) {
    fflush(stdout);
    exit(0);
}

/* the type of the last result, for a type that depends on the value */
int bcrt_type(void) { return bcrt_last; }

/* a string variable's value, as a reference of the caller's own */
int bcrt_ref(int p) {
    if (p) string_ref((sb_string_t *)(intptr_t)p);
    return p;
}

void bcrt_unref(int p) {
    if (p) string_unref((sb_string_t *)(intptr_t)p);
}

/* a string variable's first value */
int bcrt_str_empty(void) {
    value_t v = val_default(VAL_STRING);
    return (int)(intptr_t)v.sval;
}

/* a string literal's value */
int bcrt_str_lit(const char *s, int len) {
    value_t v = val_string(s, len);
    return (int)(intptr_t)v.sval;
}

pair_t bcrt_bin(int op, int alo, int ahi, int at, int blo, int bhi, int bt) {
    value_t x = take(alo, ahi, at), y = take(blo, bhi, bt), r;
    error_t e = eval_binary((binop_t)op, &x, &y, &r);
    val_clear(&x);
    val_clear(&y);
    if (e != ERR_NONE) bcrt_fail(e);
    return give(&r);
}

pair_t bcrt_cmp(int op, int alo, int ahi, int at, int blo, int bhi, int bt) {
    value_t x = take(alo, ahi, at), y = take(blo, bhi, bt), r;
    error_t e = eval_compare((cmpop_t)op, &x, &y, &r);
    val_clear(&x);
    val_clear(&y);
    if (e != ERR_NONE) bcrt_fail(e);
    return give(&r);
}

pair_t bcrt_un(int op, int alo, int ahi, int at) {
    value_t x = take(alo, ahi, at), r;
    error_t e = eval_unary((unaryop_t)op, &x, &r);
    val_clear(&x);
    if (e != ERR_NONE) bcrt_fail(e);
    return give(&r);
}

/* a value, as a variable of type to holds it (rounding, Overflow, Type
 * mismatch) */
pair_t bcrt_conv(int lo, int hi, int t, int to) {
    value_t slot = val_default((val_type_t)to), x = take(lo, hi, t);
    error_t e = eval_store(&slot, &x);
    val_clear(&x);
    if (e != ERR_NONE) bcrt_fail(e);
    return give(&slot);
}

int bcrt_true(int lo, int hi, int t) {
    value_t x = take(lo, hi, t);
    int r = val_is_true(&x);
    val_clear(&x);
    return r;
}

/* FOR: whether the loop runs with the variable at v (for_in_range) */
int bcrt_for_in(double v, double limit, double step) {
    return step >= 0 ? v <= limit : v >= limit;
}

/* NEXT: the variable of type vt, at cur, stepped (for_next) */
double bcrt_for_step(double cur, int vt, double step) {
    double v;
    if (vt == VAL_INTEGER || vt == VAL_LONG) {
        v = cur + step;
        if (!(v <= 2147483647.0 && v >= -2147483648.0) || v != (double)(int)v ||
            (vt == VAL_INTEGER && !INT16_OK(v))) {
            value_t slot = val_default((val_type_t)vt), x = val_double(v);
            error_t e = eval_store(&slot, &x);
            if (e != ERR_NONE) bcrt_fail(e);
            return (double)slot.ival;
        }
        return v;
    }
    if (vt == VAL_SINGLE) return single_round(cur + step);
    return cur + step;
}

void bcrt_print_val(int lo, int hi, int t) {
    value_t x = take(lo, hi, t);
    error_t e = print_value(&x, 0, console_width);
    val_clear(&x);
    if (e != ERR_NONE) bcrt_fail(e);
}

void bcrt_print_comma(void) { print_comma(console_width); }

void bcrt_print_nl(void) { print_text("\n"); }

/* TAB(n) or SPC(n) as a PRINT item: the builtin's value, printed as one
 * (print_value's tab_spc) */
void bcrt_print_tab(const char *name, int lo, int hi, int t) {
    value_t arg = take(lo, hi, t), out;
    error_t e = builtin_call(name, &arg, 1, &out);
    val_clear(&arg);
    if (e != ERR_NONE) bcrt_fail(e);
    e = print_value(&out, 1, console_width);
    val_clear(&out);
    if (e != ERR_NONE) bcrt_fail(e);
}

/* a builtin function, as the interpreter's EXPR_CALL reaches it
 * (builtin_call): n of three arguments */
pair_t bcrt_fn(const char *name, int n, int alo, int ahi, int at, int blo, int bhi, int bt,
               int clo, int chi, int ct) {
    value_t args[3], out;
    error_t e;
    args[0] = take(alo, ahi, at);
    args[1] = take(blo, bhi, bt);
    args[2] = take(clo, chi, ct);
    e = builtin_call(name, args, n, &out);
    for (int i = 0; i < 3; i++) val_clear(&args[i]);
    if (e != ERR_NONE) bcrt_fail(e);
    return give(&out);
}

/* GOSUB and RETURN: the return points are the compiled program's own,
 * numbered; the stack is the interpreter's depth (MAX_GOSUB_DEPTH) and
 * errors */
#define BCRT_GOSUB_DEPTH 256
static int bcrt_gosubs[BCRT_GOSUB_DEPTH];
static int bcrt_ngosub;

void bcrt_gosub(int k) {
    if (bcrt_ngosub >= BCRT_GOSUB_DEPTH) bcrt_fail(ERR_STACK_OVERFLOW);
    bcrt_gosubs[bcrt_ngosub++] = k;
}

int bcrt_return(void) {
    if (bcrt_ngosub <= 0) bcrt_fail(ERR_RETURN_WITHOUT_GOSUB);
    return bcrt_gosubs[--bcrt_ngosub];
}
