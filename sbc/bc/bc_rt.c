/* bc_rt.c -- the runtime of the programs s32-bc compiles.
 *
 * Compiled for SLOW-32 and linked with libsbasic.s32a, the interpreter
 * less its main(): every operator, conversion and PRINT item goes
 * through the interpreter's own code (eval_binary, eval_store,
 * print_value...), so a compiled program computes and prints what the
 * interpreter does, to the byte, by construction.  Values cross from
 * compiled code as a double and a type (val_type_t): an INTEGER or LONG
 * is exact in a double, a SINGLE intermediate is an unrounded double as
 * the interpreter keeps it.  The type travels at run time because some
 * results' types depend on their values (5.5 \ 2 is an INTEGER,
 * 100000.5 \ 2 a LONG).  Native code for the common cases comes later;
 * this is the reference it will be held to.
 */
#include <stdio.h>
#include <stdlib.h>
#include "value.h"
#include "error.h"
#include "ast.h"
#include "eval.h"
#include "builtin.h"
#include "fileio.h"
#include "terminal.h"

extern int console_width;

static int bcrt_last;           /* the type of the last result */

static void bcrt_fail(error_t e) {
    error_print(e, 0);
    fflush(stdout);
    exit(0);
}

static value_t num(double d, int t) {
    value_t v;
    v.type = (val_type_t)t;
    if (t == VAL_INTEGER || t == VAL_LONG) v.ival = (int)d;
    else v.dval = d;
    return v;
}

static double as_double(const value_t *v) {
    bcrt_last = v->type;
    if (v->type == VAL_INTEGER || v->type == VAL_LONG) return (double)v->ival;
    return v->dval;
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

double bcrt_bin(int op, double a, int at, double b, int bt) {
    value_t x = num(a, at), y = num(b, bt), r;
    error_t e = eval_binary((binop_t)op, &x, &y, &r);
    if (e != ERR_NONE) bcrt_fail(e);
    return as_double(&r);
}

double bcrt_cmp(int op, double a, int at, double b, int bt) {
    value_t x = num(a, at), y = num(b, bt), r;
    error_t e = eval_compare((cmpop_t)op, &x, &y, &r);
    if (e != ERR_NONE) bcrt_fail(e);
    return as_double(&r);
}

double bcrt_un(int op, double a, int at) {
    value_t x = num(a, at), r;
    error_t e = eval_unary((unaryop_t)op, &x, &r);
    if (e != ERR_NONE) bcrt_fail(e);
    return as_double(&r);
}

/* a value of type from, as a variable of type to holds it (rounding,
 * Overflow) */
double bcrt_conv(double v, int from, int to) {
    value_t slot = val_default((val_type_t)to), x = num(v, from);
    error_t e = eval_store(&slot, &x);
    if (e != ERR_NONE) bcrt_fail(e);
    return as_double(&slot);
}

int bcrt_true(double v, int t) {
    value_t x = num(v, t);
    return val_is_true(&x);
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
            (vt == VAL_INTEGER && !INT16_OK(v)))
            return bcrt_conv(v, VAL_DOUBLE, vt);
        bcrt_last = vt;
        return v;
    }
    bcrt_last = vt;
    if (vt == VAL_SINGLE) return single_round(cur + step);
    return cur + step;
}

void bcrt_print_num(double v, int t) {
    value_t x = num(v, t);
    error_t e = print_value(&x, 0, console_width);
    if (e != ERR_NONE) bcrt_fail(e);
}

void bcrt_print_lit(const char *s, int len) {
    value_t x = val_string(s, len);
    error_t e = print_value(&x, 0, console_width);
    val_clear(&x);
    if (e != ERR_NONE) bcrt_fail(e);
}

void bcrt_print_comma(void) { print_comma(console_width); }

void bcrt_print_nl(void) { print_text("\n"); }

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

/* a builtin function, as the interpreter's EXPR_CALL reaches it
 * (builtin_call): numeric arguments, n of them, a number back */
double bcrt_fn(const char *name, int n, double a, int at, double b, int bt, double c, int ct) {
    value_t args[3], out;
    error_t e;
    args[0] = num(a, at);
    args[1] = num(b, bt);
    args[2] = num(c, ct);
    e = builtin_call(name, args, n, &out);
    if (e != ERR_NONE) bcrt_fail(e);
    if (out.type == VAL_STRING) {       /* (s32-bc does not call these yet) */
        val_clear(&out);
        bcrt_fail(ERR_TYPE_MISMATCH);
    }
    return as_double(&out);
}

/* TAB(n) or SPC(n) as a PRINT item: the builtin's value, printed as one
 * (print_value's tab_spc) */
void bcrt_print_tab(const char *name, double a, int at) {
    value_t arg = num(a, at), out;
    error_t e = builtin_call(name, &arg, 1, &out);
    if (e != ERR_NONE) bcrt_fail(e);
    e = print_value(&out, 1, console_width);
    val_clear(&out);
    if (e != ERR_NONE) bcrt_fail(e);
}
