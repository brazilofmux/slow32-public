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

/* PRINT #'s output, the file's, until its end puts back what was (a
 * stack: an item's evaluation may PRINT # too); PRINT's width is then 0
 * (print_items: no wrapping in a file) */
static print_redirect_t bcrt_redirect[16];
static int bcrt_redirect_w[16];         /* its width: a file's 0, the printer's 80 */
static int bcrt_nredirect;

static int bcrt_width_now(void) { return bcrt_nredirect ? bcrt_redirect_w[bcrt_nredirect - 1] : console_width; }

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

static int bcrt_nusing;
static void bcrt_trap(int code) {
    while (bcrt_nredirect > 0) eval_print_file_end(&bcrt_redirect[--bcrt_nredirect]);
    bcrt_nusing = 0;
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
typedef struct { unsigned gen; value_t *data; int lbound[3], dims[3]; } bcrt_acell_t;
_Static_assert(sizeof(bcrt_acell_t) == 32, "bc.c: the cell's layout");
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
        e = eval_store_fixed(slot, &x, arr->fixlen);
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

/* --- console statements: the interpreter's actions (eval.c) ---------- */

void bcrt_cls(void) { eval_cls(); }

int bcrt_cursor_row(void) { return eval_cursor_row(); }
int bcrt_cursor_col(void) { return eval_cursor_col(); }

void bcrt_locate(int row, int col) {
    error_t e = eval_locate(row, col);
    if (e != ERR_NONE) bcrt_fail(e);
}

void bcrt_color_arg(int c) {
    error_t e = eval_color_arg(c);
    if (e != ERR_NONE) bcrt_fail(e);
}

void bcrt_color(int fg, int bg, int has_fg, int has_bg) { eval_color(fg, bg, has_fg, has_bg); }

/* LINE INPUT's line, a string of the caller's */
int bcrt_line_input(const char *prompt) {
    value_t v;
    error_t e = eval_line_input(prompt, &v);
    if (e != ERR_NONE) bcrt_fail(e);
    return (int)(intptr_t)v.sval;
}

void bcrt_write_comma(void) { print_text(","); }
void bcrt_write_end(void) { print_text("\n"); }

void bcrt_write_val(int lo, int hi, int t) {
    value_t x = take(lo, hi, t);
    eval_write_value(&x);
    val_clear(&x);
}

/* SLEEP v (has 0: SLEEP alone) */
void bcrt_sleep(int lo, int hi, int t, int has) {
    value_t x = take(lo, hi, t);
    error_t e = eval_sleep(has ? &x : NULL);
    val_clear(&x);
    if (e != ERR_NONE) bcrt_fail(e);
}

void bcrt_width(int w) { eval_width(w); }

/* CLEAR's arrays (compiled code clears its own variables) */
void bcrt_clear(void) { array_clear_all(); }

/* PRINT USING: the format's value and which items have one ("1"/"0"
 * each); then using_next (the item to evaluate, -1 when done) and
 * using_put, as exec_print_using_items drives them.  A stack: an item's
 * evaluation may PRINT USING too (a FUNCTION). */
static using_t bcrt_using[16];
static unsigned char bcrt_using_has[16][256];

void bcrt_using_begin(int lo, int hi, int t, int n, const char *has) {
    value_t f = take(lo, hi, t);
    using_t *u;
    if (bcrt_nusing == 16 || n > 256) bcrt_fail(ERR_STACK_OVERFLOW);
    u = &bcrt_using[bcrt_nusing];
    for (int i = 0; i < n; i++) bcrt_using_has[bcrt_nusing][i] = has[i] == '1';
    using_begin(u, f.type == VAL_STRING && f.sval ? f.sval->data : "", n, bcrt_using_has[bcrt_nusing]);
    bcrt_nusing++;
    val_clear(&f);
}

int bcrt_using_next(void) {
    int k = using_next(&bcrt_using[bcrt_nusing - 1]);
    if (k < 0) bcrt_nusing--;
    return k;
}

void bcrt_using_put(int lo, int hi, int t) {
    value_t x = take(lo, hi, t);
    using_put(&bcrt_using[bcrt_nusing - 1], &x);
    val_clear(&x);
}

/* MID$(v$, start[, n]) = r$ (exec_mid_assign): whether start is in v$
 * (else nothing is evaluated, nothing changes); n's check; the new
 * string of v$'s length -- old, the variable's own, not taken */
int bcrt_mid_in(int old, int start) {
    sb_string_t *o = (sb_string_t *)(intptr_t)old;
    return start - 1 >= 0 && start - 1 < o->len;
}

void bcrt_mid_len(int n) { if (n < 0) bcrt_fail(ERR_ILLEGAL_FUNCTION_CALL); }

int bcrt_mid_set(int old, int start, int n, int lo, int hi, int t) {
    sb_string_t *o = (sb_string_t *)(intptr_t)old;
    value_t r = take(lo, hi, t), res;
    int maxlen = o->len - (start - 1);
    error_t e;
    if (n >= 0 && n < maxlen) maxlen = n;
    e = eval_mid_set(o, start - 1, o->len, maxlen, &r, &res);
    val_clear(&r);
    if (e != ERR_NONE) bcrt_fail(e);
    return (int)(intptr_t)res.sval;
}

/* --- files: the interpreter's (fileio.c, eval.c) ----------------------- */

/* OPEN name FOR mode AS #handle [LEN = n]: the name a string (held until
 * the handle is evaluated), then the open, then a RANDOM file's length */
static value_t bcrt_open_held;

void bcrt_open_name(int lo, int hi, int t) {
    value_t f = take(lo, hi, t);
    if (f.type != VAL_STRING) { val_clear(&f); bcrt_fail(ERR_TYPE_MISMATCH); }
    val_clear(&bcrt_open_held);
    bcrt_open_held = f;
}

void bcrt_open(int handle, int mode) {
    error_t e = fileio_open(bcrt_open_held.sval->data, (file_mode_t)mode, handle);
    val_clear(&bcrt_open_held);
    bcrt_open_held = val_default(VAL_INTEGER);
    if (e != ERR_NONE) bcrt_fail(e);
}

void bcrt_reclen(int handle, int n) { fileio_set_reclen(handle, n); }

void bcrt_close(int handle) {
    error_t e = fileio_close(handle);
    if (e != ERR_NONE) bcrt_fail(e);
}

void bcrt_pfile_begin(int handle) {
    error_t e;
    if (bcrt_nredirect == 16) bcrt_fail(ERR_STACK_OVERFLOW);
    e = eval_print_file_begin(handle, &bcrt_redirect[bcrt_nredirect]);
    if (e != ERR_NONE) bcrt_fail(e);
    bcrt_redirect_w[bcrt_nredirect++] = 0;
}

/* LPRINT's output: the printer's (eval_lprint_begin), ended as PRINT #'s */
void bcrt_lprint_begin(void) {
    if (bcrt_nredirect == 16) bcrt_fail(ERR_STACK_OVERFLOW);
    eval_lprint_begin(&bcrt_redirect[bcrt_nredirect]);
    bcrt_redirect_w[bcrt_nredirect++] = 80;
}

void bcrt_pfile_end(void) { eval_print_file_end(&bcrt_redirect[--bcrt_nredirect]); }

/* the open file #handle (INPUT #, LINE INPUT #, WRITE #) */
int bcrt_file_fp(int handle) {
    FILE *fp = fileio_get(handle);
    if (!fp) bcrt_fail(ERR_FILE_NOT_OPEN);
    return (int)(intptr_t)fp;
}

void bcrt_write_file_comma(int fp) { fprintf((FILE *)(intptr_t)fp, ","); }
void bcrt_write_file_end(int fp) { fprintf((FILE *)(intptr_t)fp, "\n"); }

void bcrt_write_file_val(int fp, int lo, int hi, int t) {
    value_t x = take(lo, hi, t);
    eval_write_file_value((FILE *)(intptr_t)fp, &x);
    val_clear(&x);
}

/* INPUT #'s next field, for a target of type t */
pair_t bcrt_input_field(int fp, int t) {
    value_t v;
    error_t e = eval_input_field((FILE *)(intptr_t)fp, (val_type_t)t, &v);
    if (e != ERR_NONE) bcrt_fail(e);
    return give(&v);
}

int bcrt_line_input_file(int fp) {
    value_t v;
    error_t e = eval_line_input_file((FILE *)(intptr_t)fp, &v);
    if (e != ERR_NONE) bcrt_fail(e);
    return (int)(intptr_t)v.sval;
}

/* GET and PUT's file (BINARY or RANDOM), and a position (from 1) */
int bcrt_getput_fp(int handle) {
    FILE *fp;
    error_t e = eval_getput_file(handle, &fp);
    if (e != ERR_NONE) bcrt_fail(e);
    return (int)(intptr_t)fp;
}

void bcrt_file_pos(int handle, int fp, int pos) {
    error_t e = eval_file_pos(handle, (FILE *)(intptr_t)fp, pos);
    if (e != ERR_NONE) bcrt_fail(e);
}

/* GET's value of type t (a string's as long as cur, the variable's own
 * string, not taken) */
pair_t bcrt_get_val(int fp, int t, int cur) {
    value_t v;
    sb_string_t *c = (sb_string_t *)(intptr_t)cur;
    error_t e = eval_get_value((FILE *)(intptr_t)fp, (val_type_t)t, c ? c->len : 0, &v);
    if (e != ERR_NONE) bcrt_fail(e);
    return give(&v);
}

void bcrt_put_val(int fp, int lo, int hi, int t) {
    value_t x = take(lo, hi, t);
    error_t e = eval_put_value((FILE *)(intptr_t)fp, &x);
    val_clear(&x);
    if (e != ERR_NONE) bcrt_fail(e);
}

/* LSET (left) / RSET: the variable's string's length (cur, not taken),
 * and the new string */
int bcrt_strlen(int cur) { return cur ? ((sb_string_t *)(intptr_t)cur)->len : 0; }

int bcrt_lrset(int cur, int lo, int hi, int t, int left) {
    value_t nv = take(lo, hi, t), r;
    error_t e = eval_lrset(bcrt_strlen(cur), &nv, left, &r);
    val_clear(&nv);
    if (e != ERR_NONE) bcrt_fail(e);
    return (int)(intptr_t)r.sval;
}

void bcrt_kill(int lo, int hi, int t) {
    value_t f = take(lo, hi, t);
    error_t e = f.type == VAL_STRING ? fileio_kill(f.sval->data) : ERR_TYPE_MISMATCH;
    val_clear(&f);
    if (e != ERR_NONE) bcrt_fail(e);
}

void bcrt_name(int alo, int ahi, int at, int blo, int bhi, int bt) {
    value_t a = take(alo, ahi, at), b = take(blo, bhi, bt);
    error_t e = a.type == VAL_STRING && b.type == VAL_STRING
                ? fileio_rename(a.sval->data, b.sval->data) : ERR_TYPE_MISMATCH;
    val_clear(&a);
    val_clear(&b);
    if (e != ERR_NONE) bcrt_fail(e);
}

/* CHDIR, MKDIR, RMDIR (kind: the statement's number) */
void bcrt_dir(int kind, int lo, int hi, int t) {
    value_t p = take(lo, hi, t);
    error_t e = eval_dir(kind, &p);
    val_clear(&p);
    if (e != ERR_NONE) bcrt_fail(e);
}

void bcrt_poke(int offset, int v) {
    error_t e = eval_poke(offset, v);
    if (e != ERR_NONE) bcrt_fail(e);
}

void bcrt_def_seg(int seg) {
    extern int def_seg;
    def_seg = seg;
}

/* --- graphics: the interpreter's (eval_gfx, eval_screen) --------------- */

void bcrt_screen(int mode) {
    error_t e = eval_screen(mode);
    if (e != ERR_NONE) bcrt_fail(e);
}

/* GET (x1, y1)-(x2, y2), a or PUT (x, y), a, action (eval_gfx_getput):
 * steps bit 0 the first point's STEP, bit 1 the second's; a, array
 * name's element of n subscripts (none: its first) */
void bcrt_gfx_getput(int put, int steps, int action, int x1, int y1, int x2, int y2,
                     const char *name, int mode, int n, int i0, int i1, int i2) {
    int ix[3] = { i0, i1, i2 };
    char kb[ARRAY_KEY_MAX];
    gfx_op_t g = { put ? STMT_GFX_PUT : STMT_GFX_GET, x1, y1, x2, y2, -1, -1, 1, 0, 0,
                   steps & 1, (steps >> 1) & 1, 1, action, 0 };
    error_t e = eval_gfx_getput(&g, akey(name, mode, kb), n, ix);
    if (e != ERR_NONE) bcrt_fail(e);
}

/* flags: has_x1, has_color, has_aux, step1, step2, has_from, preset, from bit 0 */
void bcrt_gfx(int type, int flags, int box, int x1, int y1, int x2, int y2, int color, int aux) {
    gfx_op_t g = { type, x1, y1, x2, y2, color, aux, flags & 1, (flags >> 1) & 1, (flags >> 2) & 1,
                   (flags >> 3) & 1, (flags >> 4) & 1, (flags >> 5) & 1, box, (flags >> 6) & 1 };
    error_t e = eval_gfx(&g);
    if (e != ERR_NONE) bcrt_fail(e);
}

/* --- records (TYPE): the interpreter's sb_record_t ----------------------- */

_Static_assert(offsetof(sb_record_t, fields) == 12, "bc.c: a record's fields 12 in");
static unsigned char bcrt_types_ran[256];

/* TYPE t has run: DIM AS t may make one */
void bcrt_type_ran(int t) { if (t >= 0 && t < 256) bcrt_types_ran[t] = 1; }

/* TYPE t's n fields read from bc.c's type_sig: a letter each ('A' +
 * val_type_t), a STRING * n's followed by n -- t one that has run */
static void sig_fields(int t, const char *sig, int n, val_type_t *types, int *lens) {
    if (t < 0 || t >= 256 || !bcrt_types_ran[t]) bcrt_fail(ERR_UNDEFINED_TYPE);
    for (int i = 0; i < n && i < 32; i++) {
        types[i] = (val_type_t)(*sig++ - 'A');
        lens[i] = 0;
        while (*sig >= '0' && *sig <= '9') lens[i] = lens[i] * 10 + (*sig++ - '0');
    }
}

/* DIM v AS t: a record of n fields, each 0 or empty, a STRING * n n
 * CHR$(0)s (exec_dim_as_type) */
int bcrt_record_new(int t, const char *sig, int n) {
    val_type_t types[32];
    int lens[32];
    sb_record_t *r;
    if (n > 32) n = 32;
    sig_fields(t, sig, n, types, lens);
    r = eval_record_make(t, n, types, lens);
    if (!r) bcrt_fail(ERR_OUT_OF_MEMORY);
    return (int)(intptr_t)r;
}

/* DIM a(...) AS t: its elements records (eval_array_records) */
void bcrt_dim_records(const char *name, int mode, int t, const char *sig, int n) {
    val_type_t types[32];
    int lens[32];
    char kb[ARRAY_KEY_MAX];
    error_t e;
    if (n > 32) n = 32;
    sig_fields(t, sig, n, types, lens);
    e = eval_array_records(akey(name, mode, kb), t, n, types, lens);
    if (e != ERR_NONE) bcrt_fail(e);
}

/* DIM a(...) AS STRING * n (eval_array_fixed) */
void bcrt_dim_fixed(const char *name, int mode, int n) {
    char kb[ARRAY_KEY_MAX];
    error_t e = eval_array_fixed(akey(name, mode, kb), n);
    if (e != ERR_NONE) bcrt_fail(e);
}

/* b = a, two records: a's fields into b's (record_store) */
void bcrt_record_copy(int dst, int src) {
    value_t d, r;
    error_t e;
    if (!dst || !src) bcrt_fail(ERR_TYPE_MISMATCH);
    d.type = r.type = VAL_RECORD;
    d.rval = (sb_record_t *)(intptr_t)dst;
    r.rval = (sb_record_t *)(intptr_t)src;
    e = eval_record_copy(&d, &r);
    if (e != ERR_NONE) bcrt_fail(e);
}

/* string p (its reference taken) made a STRING * n's: cut, or padded
 * with spaces */
int bcrt_fix(int p, int n) {
    value_t v;
    error_t e;
    v.type = VAL_STRING;
    v.sval = (sb_string_t *)(intptr_t)p;
    e = eval_fix_string(&v, n);
    if (e != ERR_NONE) bcrt_fail(e);
    return (int)(intptr_t)v.sval;
}

/* DIM x AS STRING * n: x n CHR$(0)s, unless it is n characters already
 * (a DIM met again: a declaration) */
int bcrt_fixinit(int p, int n) {
    value_t v;
    if (p && ((sb_string_t *)(intptr_t)p)->len == n) return p;
    bcrt_unref(p);
    v = eval_fixed_value(n);
    return (int)(intptr_t)v.sval;
}

void bcrt_record_unref(int p) {
    if (p) record_unref((sb_record_t *)(intptr_t)p);
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
    error_t e = print_value(&x, 0, bcrt_width_now());
    val_clear(&x);
    if (e != ERR_NONE) bcrt_fail(e);
}

void bcrt_print_comma(void) { print_comma(bcrt_width_now()); }

void bcrt_print_nl(void) { print_text("\n"); }

/* TAB(n) or SPC(n) as a PRINT item: the builtin's value, printed as one
 * (print_value's tab_spc) */
void bcrt_print_tab(const char *name, int lo, int hi, int t) {
    value_t arg = take(lo, hi, t), out;
    error_t e = builtin_call(name, &arg, 1, &out);
    val_clear(&arg);
    if (e != ERR_NONE) bcrt_fail(e);
    e = print_value(&out, 1, bcrt_width_now());
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

/* DIM / REDIM name(lo TO up, ...) AS type, its bounds integers checked
 * (bcrt_dim_range).  in_place: a static DIM met when it runs, a
 * declaration unless ERASE took the array away (exec_dim). */
void bcrt_dim(const char *name, int type, int n, int in_place, int redim, int preserve,
              int l0, int l1, int l2, int u0, int u1, int u2, int mode) {
    int lo[3] = { l0, l1, l2 }, up[3] = { u0, u1, u2 };
    char kb[ARRAY_KEY_MAX];
    error_t e;
    name = akey(name, mode, kb);
    if (in_place && array_find(name)) return;
    e = eval_dim(name, (val_type_t)type, n, lo, up, redim, preserve);
    if (e != ERR_NONE) bcrt_fail(e);
}

void bcrt_dim_range(int lower, int upper) {
    error_t e = eval_dim_range(lower, upper);
    if (e != ERR_NONE) bcrt_fail(e);
}

int bcrt_option_base_now(void) { return eval_option_base(); }

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
    for (int i = 0; i < n && i < 3; i++) {
        c->lbound[i] = arr->lbound[i];
        c->dims[i] = arr->dims[i];
    }
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
