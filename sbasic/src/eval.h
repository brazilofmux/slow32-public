#ifndef SBASIC_EVAL_H
#define SBASIC_EVAL_H

#include "ast.h"
#include "env.h"
#include "error.h"
#include "lower.h"

/* Evaluate an expression, storing result in *out */
error_t eval_expr(env_t *env, expr_t *e, value_t *out);

/* Execute a single statement */
error_t eval_stmt(env_t *env, stmt_t *s);

/* Execute a statement chain (linked list traversal) */
error_t eval_stmts(env_t *env, stmt_t *stmts);

/* Run a program: its labels, DATA and procedures collected, its units
 * lowered (lower.h) and run. */
error_t eval_program(env_t *env, stmt_t *program);

/* The same, with the main unit run by main_fn instead of the executor:
 * a compiled program's (s32-sbc), given the lowered ops it was written
 * from */
typedef error_t (*main_fn_t)(env_t *env, code_t *main_code);
error_t eval_program_with(env_t *env, stmt_t *program, main_fn_t main_fn);

/* a SUB's or FUNCTION's body as C, run in place of the executor when it
 * is called (its lowered body must have nops ops) */
void eval_compiled_proc(const char *name, main_fn_t fn, int nops);

/* --- The executor, op by op, for compiled programs --- */

/* a running loop: its FOR, limit, step, and the op its body starts at */
typedef struct {
    stmt_t *s;
    double limit, step;
    int body_pc;
    value_t *var;       /* the variable's value, while gen is env_generation */
    unsigned gen;
} for_slot_t;

/* an activation of a unit: its FOR stack and GOSUB base */
typedef struct {
    env_t *env;
    code_t *c;
    int is_main;
    code_t *saved_code;
    for_slot_t *fors;
    int nfors, cap_fors;
    int gosub_base;
} act_t;

error_t act_begin(act_t *a, env_t *env, code_t *c, int is_main);
void act_end(act_t *a);
error_t eval_enter(void);
void eval_leave(void);
int eval_names_plain(const char *const *names, int n);
/* name is a DEF FN whose DEF has run (s32-sbc's inlined calls) */
int eval_def_fn_defined(const char *name);
/* run op *pc: ERR_NONE with *pc the next op, or the op's error (*pc
 * unchanged) */
error_t act_step(act_t *a, int *pc);
/* an op's error: a transfer (GOTO, GOSUB, RETURN, RESUME, an error ON
 * ERROR traps) sets *pc and is 1; anything else ends the unit with
 * *result, 0 */
int act_transfer(act_t *a, error_t err, int *pc, error_t *result);
/* a FOR reached, its counter set (var: its value, or NULL): its own
 * running loop dropped, a frame pushed when runs */
error_t act_for_enter(act_t *a, stmt_t *s, int runs, double limit, double step,
                      int body_pc, value_t *var);
/* a RETURN without a label: 1 and *pc its target, or 0 (no GOSUB open:
 * the executor's) */
int act_return(act_t *a, int *pc);
/* GOSUB, its return op pushed (the jump is the caller's) */
error_t act_gosub(act_t *a, int return_pc);

/* --- The interpreter's operations, for compiled expressions and
 * statements: each does what the executor does at that point --- */

#include "array.h"
extern int trace_enabled;
extern int cur_basic_line;      /* the running statement's line number */
void eval_trace(const stmt_t *s);
error_t eval_variable(env_t *env, const char *name, value_t *out);
value_t *eval_var_slot(env_t *env, const char *name);
value_t *eval_assign_slot(env_t *env, const char *name);
error_t eval_store(value_t *slot, const value_t *v);
error_t eval_unary(unaryop_t op, value_t *operand, value_t *out);
error_t eval_binary(binop_t op, value_t *left, value_t *right, value_t *out);
error_t eval_compare(cmpop_t op, value_t *left, value_t *right, value_t *out);
error_t eval_assign_check(env_t *env, const char *name);
error_t eval_assign_store(env_t *env, const char *name, const value_t *v);
error_t eval_array_store(const char *name, int nidx, int *indices, const value_t *v);
sb_array_t *eval_call_array(const char *name);
/* a FUNCTION's call (out its value) or a SUB's (out NULL) with its
 * arguments evaluated, which it takes -- refs[i], when not NULL, the
 * variable passed by reference instead; *cache keeps the procedure */
error_t eval_call_proc(const char *name, void **cache, value_t *args, value_t **refs,
                       int nargs, value_t *out);
/* a variable argument's value when it is passed by reference, else NULL */
value_t *eval_arg_ref(env_t *env, const char *name);
/* the check of argument i's type, passed by reference, as it is reached */
error_t eval_arg_check(const char *name, void **cache, int i, const value_t *ref);
error_t eval_array_read(sb_array_t *arr, int nidx, int *indices, value_t *out);
error_t print_value(const value_t *v, int tab_spc, int width);
void print_comma(int width);
void print_tab_spc(int is_spc, int n);
void print_text(const char *s);
extern int console_width;

/* Current print position (column 0-based, row 1-based) */
extern int print_col;
extern int print_row;

/* Points to the active column counter (stdout or file) for TAB() */
extern int *active_print_col;

#endif
