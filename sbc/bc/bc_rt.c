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
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stddef.h>
#include <setjmp.h>
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

/* --- ON ERROR GOTO: the handler, entered from wherever the error is ---
 *
 * As act_transfer traps them: only while a handler is set and none is
 * running, only an error that may be trapped (eval_trappable).  The
 * program's statements are a function of their own (bcm_body), run by
 * bcrt_run; an error longjmps back there, out of whatever runtime code
 * or procedure it was in, and the body is entered again at the
 * handler's op.  Ops are numbered as lower.c numbers the main program's;
 * the body stores the one it is at (bcrt_op) and its line (bcrt_line,
 * the ERL) as each begins.  RESUME goes to an op through bcrt_target. */
int bcrt_op, bcrt_line, bcrt_target;
static jmp_buf bcrt_jb;
static int bcrt_running;        /* inside bcrt_run's body */
static int trap_handler = -1, trap_active, trap_op, trap_code, trap_line;
static int bcrt_nactive;        /* procedures entered (bcrt_enter) and not left */

static void bcrt_trap(int code) {
    trap_code = code;
    trap_line = bcrt_line;
    trap_active = 1;
    trap_op = bcrt_op;
    bcrt_target = trap_handler;
    longjmp(bcrt_jb, 1);
}

static void bcrt_fail(error_t e) {
    if (bcrt_running && trap_handler >= 0 && !trap_active && eval_trappable(e))
        bcrt_trap(error_qb_code(e));
    error_print(e, 0);
    fflush(stdout);
    exit(0);
}

/* the program's statements, body(0) from the start and body(1) at
 * bcrt_target, again after each error trapped */
void bcrt_run(void (*body)(int)) {
    int at = 0;
    for (;;) {
        if (setjmp(bcrt_jb) == 0) {
            bcrt_running = 1;
            body(at);
            bcrt_running = 0;
            return;
        }
        /* (the procedures it was in are left: their array scopes and
         * their depth) */
        while (bcrt_nactive > 0) {
            array_scope_pop();
            eval_leave();
            bcrt_nactive--;
        }
        at = 1;
    }
}

/* ON ERROR GOTO label (op: the label's), or ON ERROR GOTO 0 (op -1) */
void bcrt_on_error(int op) {
    trap_handler = op;
    trap_active = 0;
}

/* RESUME (next 0) or RESUME NEXT (1): to the op the error was in, or the
 * one after it, through bcrt_target */
void bcrt_resume(int next) {
    if (!trap_active) bcrt_fail(ERR_RESUME_WITHOUT_ERROR);
    trap_active = 0;
    bcrt_target = trap_op + (next ? 1 : 0);
}

/* RESUME label: the jump is compiled code's */
void bcrt_resume_label(void) {
    if (!trap_active) bcrt_fail(ERR_RESUME_WITHOUT_ERROR);
    trap_active = 0;
}

/* ERROR n: n is ERR, the handler's if there is one (else "ON ERROR GOTO",
 * as the interpreter reports it) */
void bcrt_error_raise(int n) {
    if (bcrt_running && trap_handler >= 0 && !trap_active)
        bcrt_trap(n);
    trap_code = n;
    trap_line = bcrt_line;
    bcrt_fail(ERR_ON_ERROR_GOTO);
}

/* ERR (which 0) and ERL (1) */


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

/* The arrays compiled code reaches itself: a cell for each array name
 * of the program, what an element's address takes, good while
 * array_generation is the cell's (an array made, erased or
 * re-dimensioned bumps it).  bc.c knows the cell's layout and
 * value_t's: an element is 12 bytes, its number or string 4 in. */
typedef struct { unsigned gen; value_t *data; int base, dims[3]; } bcrt_acell_t;
_Static_assert(sizeof(bcrt_acell_t) == 24, "bc.c: the cell's layout");
_Static_assert(sizeof(value_t) == 12 && offsetof(value_t, ival) == 4 &&
               offsetof(value_t, dval) == 4 && offsetof(value_t, sval) == 4, "bc.c: value_t's layout");
#define BCRT_ACELLS 256
bcrt_acell_t bcrt_acache[BCRT_ACELLS];

/* as eval_program_with starts a program */
void bcrt_start(void) {
    for (int i = 0; i < BCRT_ACELLS; i++) bcrt_acache[i].gen = ~0u;
    eval_reset();
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

/* the same for a value of a run-time type: only a string's counts */
int bcrt_vref(int lo, int t) { return t == VAL_STRING ? bcrt_ref(lo) : lo; }
void bcrt_vunref(int lo, int t) { if (t == VAL_STRING) bcrt_unref(lo); }

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

/* the same, as an INTEGER or LONG variable holds it: its int */
int bcrt_convi(int lo, int hi, int t, int to) {
    value_t slot = val_default((val_type_t)to), x = take(lo, hi, t);
    error_t e = eval_store(&slot, &x);
    val_clear(&x);
    if (e != ERR_NONE) bcrt_fail(e);
    return slot.ival;
}

/* a comparison's truth, 0 or 1, where compiled code does not make it
 * itself (a string among them) */
int bcrt_cmpi(int op, int alo, int ahi, int at, int blo, int bhi, int bt) {
    value_t x = take(alo, ahi, at), y = take(blo, bhi, bt), r;
    error_t e = eval_compare((cmpop_t)op, &x, &y, &r);
    val_clear(&x);
    val_clear(&y);
    if (e != ERR_NONE) bcrt_fail(e);
    return r.ival != 0;
}

/* a SUB's or FUNCTION's activation, counted as the interpreter counts
 * them (eval_enter: Out of stack space past its depth) */
void bcrt_enter(int proc) {
    error_t e = eval_enter();
    if (e != ERR_NONE) bcrt_fail(e);
    e = array_scope_push(proc, NULL, NULL);
    if (e != ERR_NONE) { eval_leave(); bcrt_fail(e); }
    bcrt_nactive++;
}

void bcrt_leave(void) {
    array_scope_pop();          /* (its own arrays erased) */
    eval_leave();
    bcrt_nactive--;
}

pair_t bcrt_errvar(int which) {
    value_t v = val_integer(which ? trap_line : trap_code);
    return give(&v);
}

/* an array's name as compiled code gives it, with how it is scoped
 * there (ARRAY_MODULE, ARRAY_LOCAL, ARRAY_STATIC: bc.c decides from the
 * procedure's SHARED and STATIC and DIM SHARED), made its key */
static const char *akey(const char *name, int mode, char *kb) {
    return array_key_mode(name, mode, kb, ARRAY_KEY_MAX);
}

/* an array element passed by reference, its value back after the call
 * where the array still has it (call_proc_values) */
void bcrt_elem_back(const char *name, int n, int i0, int i1, int i2, int lo, int hi, int t, int mode) {
    value_t x = take(lo, hi, t), *slot;
    int ix[3] = { i0, i1, i2 };
    char kb[ARRAY_KEY_MAX];
    sb_array_t *arr = array_find(name = akey(name, mode, kb));
    error_t e = ERR_NONE;
    if (arr && arr->ndims == n && array_get(name, ix, n, &slot) == ERR_NONE)
        e = eval_store(slot, &x);
    val_clear(&x);
    if (e != ERR_NONE) bcrt_fail(e);
}

/* LBOUND or UBOUND (upper) of array name's dimension dim */
pair_t bcrt_bound(const char *name, int dim, int upper, int mode) {
    value_t r;
    char kb[ARRAY_KEY_MAX];
    error_t e = eval_bound(akey(name, mode, kb), dim, upper, &r);
    if (e != ERR_NONE) bcrt_fail(e);
    return give(&r);
}

/* a letter's DEFtype, set as the program starts (after bcrt_start) */
void bcrt_deftype(int letter, int t) { eval_deftype((char)letter, (val_type_t)t); }

void bcrt_randomize(int has_seed, double seed) {
    error_t e = eval_randomize(has_seed, seed);
    if (e != ERR_NONE) bcrt_fail(e);
}

/* RND(n) (has_n) or RND: the interpreter's generator (fn_rnd) */
double bcrt_rnd(double n, int has_n) { return sb_rnd_of(n, has_n); }

/* INT and FIX of a float (fn_int, fn_fix) */
double bcrt_floor(double d) { return floor(d); }
double bcrt_trunc(double d) { return trunc(d); }

/* a float not a whole number in range (or not a number) made an int,
 * as val_to_integer makes it: Overflow, or rounded half to even */
int bcrt_d2i(double d) {
    if (!(d < 2147483647.5 && d >= -2147483648.5)) bcrt_fail(ERR_OVERFLOW);
    return (int)val_rint(d);
}

/* an error compiled code finds itself (Overflow, Division by zero) */
void bcrt_err(int e) {
    bcrt_fail((error_t)e);
}

int bcrt_true(int lo, int hi, int t) {
    value_t x = take(lo, hi, t);
    int r = val_is_true(&x);
    val_clear(&x);
    return r;
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

/* --- arrays: the interpreter's own (array.c), by name ----------------- */

#include "array.h"

/* DIM / REDIM name(u0[, u1[, u2]]) AS type; base -1 is the OPTION BASE in
 * force (eval_option_base), a static DIM made before the run passes the
 * one of its place in the text.  A static DIM met when it runs is a
 * declaration, unless ERASE took the array away (exec_dim). */
void bcrt_dim(const char *name, int type, int n, int is_static, int base, int redim, int preserve,
              int alo, int ahi, int at, int blo, int bhi, int bt, int clo, int chi, int ct, int mode) {
    value_t v[3];
    int u[3];
    error_t e;
    char kb[ARRAY_KEY_MAX];
    name = akey(name, mode, kb);
    v[0] = take(alo, ahi, at);
    v[1] = take(blo, bhi, bt);
    v[2] = take(clo, chi, ct);
    if (is_static && base < 0 && array_find(name)) {
        for (int i = 0; i < 3; i++) val_clear(&v[i]);
        return;
    }
    if (base < 0) base = eval_option_base();
    for (int i = 0; i < n; i++) {
        e = val_to_integer(&v[i], &u[i]);
        if (e == ERR_NONE && u[i] - base + 1 <= 0) e = ERR_ILLEGAL_FUNCTION_CALL;
        if (e != ERR_NONE) {
            for (int j = 0; j < 3; j++) val_clear(&v[j]);
            bcrt_fail(e);
        }
    }
    for (int i = 0; i < 3; i++) val_clear(&v[i]);
    e = eval_dim(name, (val_type_t)type, n, u, base, redim, preserve);
    if (e != ERR_NONE) bcrt_fail(e);
}

/* cell id's array, name(n subscripts) (eval_array_for: an implicit one
 * made), of elements of type ty */
void bcrt_alook(const char *name, int n, int id, int ty, int mode) {
    sb_array_t *arr;
    bcrt_acell_t *c = &bcrt_acache[id];
    char kb[ARRAY_KEY_MAX];
    error_t e = eval_array_for(name = akey(name, mode, kb), n, &arr);
    if (e != ERR_NONE) bcrt_fail(e);
    if ((int)arr->elem_type != ty) {
        fprintf(stderr, "s32-bc: internal: %s has elements of type %d, not %d\n", name, arr->elem_type, ty);
        exit(1);
    }
    c->gen = array_generation;
    c->data = arr->data;
    c->base = arr->base;
    for (int i = 0; i < n && i < 3; i++) c->dims[i] = arr->dims[i];
}

void bcrt_option_base(int base) { eval_set_option_base(base); }

void bcrt_erase(const char *name, int mode) {
    char kb[ARRAY_KEY_MAX];
    array_erase(akey(name, mode, kb));
}

/* --- INPUT, DATA, READ, RESTORE: the interpreter's cores ------------- */

static value_t bcrt_in[64];

/* INPUT's line (eval_input_line): types[i] is 'A' + the i-th field's
 * type; the values are taken one by one (bcrt_input_take) */
void bcrt_input(const char *prompt, int no_question, int same_line, int n, const char *types) {
    val_type_t t[64];
    error_t e;
    for (int i = 0; i < n; i++) t[i] = (val_type_t)(types[i] - 'A');
    e = eval_input_line(prompt, no_question, same_line, n, t, bcrt_in);
    if (e != ERR_NONE) bcrt_fail(e);
}

pair_t bcrt_input_take(int i) {
    pair_t r = give(&bcrt_in[i]);       /* (its string's reference goes with it) */
    bcrt_in[i].type = VAL_INTEGER;
    return r;
}

void bcrt_data(int lo, int hi, int t, int quoted) {
    value_t v = take(lo, hi, t);
    error_t e = eval_data_add(&v, quoted);
    val_clear(&v);
    if (e != ERR_NONE) bcrt_fail(e);
}

pair_t bcrt_read(int t) {
    value_t v;
    error_t e = eval_read_value((val_type_t)t, &v);
    if (e != ERR_NONE) bcrt_fail(e);
    return give(&v);
}

void bcrt_restore(int offset) { eval_restore(offset); }

/* a value as an integer, as ON ... GOTO makes its index (val_to_integer:
 * rounding, Overflow, Type mismatch) */
int bcrt_int(int lo, int hi, int t) {
    value_t x = take(lo, hi, t);
    int i;
    error_t e = val_to_integer(&x, &i);
    val_clear(&x);
    if (e != ERR_NONE) bcrt_fail(e);
    return i;
}
