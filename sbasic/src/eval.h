#ifndef SBASIC_EVAL_H
#define SBASIC_EVAL_H

#include "ast.h"
#include "env.h"
#include "error.h"

/* Evaluate an expression, storing result in *out */
error_t eval_expr(env_t *env, expr_t *e, value_t *out);

/* Execute a statement chain */
error_t eval_stmts(env_t *env, stmt_t *stmts);

/* Execute a single statement */
error_t eval_stmt(env_t *env, stmt_t *s);

#endif
