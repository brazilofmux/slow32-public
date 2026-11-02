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

/* --- The executor, op by op, for compiled programs --- */

/* a running loop: its FOR, limit, step, and the op its body starts at */
typedef struct {
    stmt_t *s;
    double limit, step;
    int body_pc;
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
/* run op *pc: ERR_NONE with *pc the next op, or the op's error (*pc
 * unchanged) */
error_t act_step(act_t *a, int *pc);
/* an op's error: a transfer (GOTO, GOSUB, RETURN, RESUME, an error ON
 * ERROR traps) sets *pc and is 1; anything else ends the unit with
 * *result, 0 */
int act_transfer(act_t *a, error_t err, int *pc, error_t *result);

/* Current print position (column 0-based, row 1-based) */
extern int print_col;
extern int print_row;

/* Points to the active column counter (stdout or file) for TAB() */
extern int *active_print_col;

#endif
