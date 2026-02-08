#include "eval.h"
#include "builtin.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* --- Expression evaluation --- */

error_t eval_expr(env_t *env, expr_t *e, value_t *out) {
    if (!e) return ERR_INTERNAL;

    switch (e->type) {
        case EXPR_LITERAL:
            *out = val_copy(&e->literal);
            return ERR_NONE;

        case EXPR_VARIABLE: {
            value_t *v = env_get(env, e->var.name);
            if (!v) return ERR_OUT_OF_MEMORY;
            *out = val_copy(v);
            return ERR_NONE;
        }

        case EXPR_UNARY: {
            value_t operand;
            EVAL_CHECK(eval_expr(env, e->unary.operand, &operand));
            error_t err;
            if (e->unary.op == OP_NEG) {
                err = val_neg(&operand, out);
            } else { /* OP_NOT */
                *out = val_integer(val_is_true(&operand) ? 0 : -1);
                err = ERR_NONE;
            }
            val_clear(&operand);
            return err;
        }

        case EXPR_BINARY: {
            /* Short-circuit for AND/OR */
            if (e->binary.op == OP_AND) {
                value_t left;
                EVAL_CHECK(eval_expr(env, e->binary.left, &left));
                if (!val_is_true(&left)) {
                    val_clear(&left);
                    *out = val_integer(0);
                    return ERR_NONE;
                }
                val_clear(&left);
                value_t right;
                EVAL_CHECK(eval_expr(env, e->binary.right, &right));
                *out = val_integer(val_is_true(&right) ? -1 : 0);
                val_clear(&right);
                return ERR_NONE;
            }
            if (e->binary.op == OP_OR) {
                value_t left;
                EVAL_CHECK(eval_expr(env, e->binary.left, &left));
                if (val_is_true(&left)) {
                    val_clear(&left);
                    *out = val_integer(-1);
                    return ERR_NONE;
                }
                val_clear(&left);
                value_t right;
                EVAL_CHECK(eval_expr(env, e->binary.right, &right));
                *out = val_integer(val_is_true(&right) ? -1 : 0);
                val_clear(&right);
                return ERR_NONE;
            }

            value_t left, right;
            EVAL_CHECK(eval_expr(env, e->binary.left, &left));
            error_t err = eval_expr(env, e->binary.right, &right);
            if (err != ERR_NONE) {
                val_clear(&left);
                return err;
            }

            switch (e->binary.op) {
                case OP_ADD:  err = val_add(&left, &right, out); break;
                case OP_SUB:  err = val_sub(&left, &right, out); break;
                case OP_MUL:  err = val_mul(&left, &right, out); break;
                case OP_DIV:  err = val_div(&left, &right, out); break;
                case OP_IDIV: err = val_idiv(&left, &right, out); break;
                case OP_MOD:  err = val_mod(&left, &right, out); break;
                case OP_POW:  err = val_pow(&left, &right, out); break;
                default:      err = ERR_INTERNAL; break;
            }
            val_clear(&left);
            val_clear(&right);
            return err;
        }

        case EXPR_COMPARE: {
            value_t left, right;
            EVAL_CHECK(eval_expr(env, e->compare.left, &left));
            error_t err = eval_expr(env, e->compare.right, &right);
            if (err != ERR_NONE) {
                val_clear(&left);
                return err;
            }
            int cmp;
            err = val_compare(&left, &right, &cmp);
            val_clear(&left);
            val_clear(&right);
            if (err != ERR_NONE) return err;

            int result;
            switch (e->compare.op) {
                case CMP_EQ: result = (cmp == 0); break;
                case CMP_NE: result = (cmp != 0); break;
                case CMP_LT: result = (cmp < 0); break;
                case CMP_GT: result = (cmp > 0); break;
                case CMP_LE: result = (cmp <= 0); break;
                case CMP_GE: result = (cmp >= 0); break;
                default:     result = 0; break;
            }
            *out = val_integer(result ? -1 : 0);
            return ERR_NONE;
        }

        case EXPR_CALL: {
            /* Evaluate arguments */
            value_t args[8];
            int nargs = e->call.nargs;
            for (int i = 0; i < nargs; i++) {
                error_t err = eval_expr(env, e->call.args[i], &args[i]);
                if (err != ERR_NONE) {
                    for (int j = 0; j < i; j++) val_clear(&args[j]);
                    return err;
                }
            }
            error_t err = builtin_call(e->call.name, args, nargs, out);
            for (int i = 0; i < nargs; i++) val_clear(&args[i]);
            return err;
        }
    }

    return ERR_INTERNAL;
}

/* --- Statement execution --- */

static error_t exec_print(env_t *env, stmt_t *s) {
    int needs_newline = 1;

    for (int i = 0; i < s->print.nitems; i++) {
        print_item_t *item = &s->print.items[i];

        if (item->expr) {
            value_t v;
            EVAL_CHECK(eval_expr(env, item->expr, &v));

            switch (v.type) {
                case VAL_INTEGER:
                    if (v.ival >= 0) printf(" %d", v.ival);
                    else printf("%d", v.ival);
                    break;
                case VAL_DOUBLE:
                    if (v.dval >= 0) printf(" %g", v.dval);
                    else printf("%g", v.dval);
                    break;
                case VAL_STRING:
                    printf("%s", v.sval->data);
                    break;
            }
            val_clear(&v);
        }

        if (item->sep == ';') {
            needs_newline = 0;
        } else if (item->sep == ',') {
            /* Tab to next 14-column zone */
            printf("\t");
            needs_newline = 0;
        } else {
            needs_newline = 1;
        }
    }

    if (needs_newline)
        printf("\n");

    return ERR_NONE;
}

static error_t exec_input(env_t *env, stmt_t *s) {
    /* Print prompt */
    if (s->input.prompt)
        printf("%s", s->input.prompt);
    else
        printf("? ");

    /* Read line */
    char line[1024];
    int pos = 0;
    int ch;
    while ((ch = getchar()) != EOF && ch != '\n') {
        if (pos < 1023)
            line[pos++] = (char)ch;
    }
    line[pos] = '\0';

    if (ch == EOF && pos == 0)
        return ERR_INPUT_PAST_END;

    /* Parse comma-separated values into variables */
    const char *p = line;
    for (int i = 0; i < s->input.nvars; i++) {
        /* Skip leading whitespace */
        while (*p == ' ') p++;

        if (s->input.vartypes[i] == VAL_STRING) {
            /* Read until comma or end */
            const char *start = p;
            while (*p && *p != ',') p++;
            int len = (int)(p - start);
            /* Trim trailing whitespace */
            while (len > 0 && start[len - 1] == ' ') len--;
            value_t v = val_string(start, len);
            env_set(env, s->input.varnames[i], &v);
            val_clear(&v);
        } else {
            /* Numeric */
            char *endp;
            double d = strtod(p, &endp);
            if (endp == p) d = 0.0;
            p = endp;
            if (s->input.vartypes[i] == VAL_INTEGER) {
                value_t v = val_integer((int)d);
                env_set(env, s->input.varnames[i], &v);
            } else {
                value_t v = val_double(d);
                env_set(env, s->input.varnames[i], &v);
            }
        }

        /* Skip comma separator */
        if (*p == ',') p++;
    }

    return ERR_NONE;
}

static error_t exec_assign(env_t *env, stmt_t *s) {
    value_t v;
    EVAL_CHECK(eval_expr(env, s->assign.value, &v));

    /* Coerce to variable type if needed */
    if (s->assign.var_type == VAL_INTEGER && v.type != VAL_INTEGER) {
        int iv;
        error_t err = val_to_integer(&v, &iv);
        val_clear(&v);
        if (err != ERR_NONE) return err;
        v = val_integer(iv);
    } else if (s->assign.var_type == VAL_STRING && v.type != VAL_STRING) {
        char buf[256];
        error_t err = val_to_string(&v, buf, sizeof(buf));
        val_clear(&v);
        if (err != ERR_NONE) return err;
        v = val_string_cstr(buf);
    }

    env_set(env, s->assign.name, &v);
    val_clear(&v);
    return ERR_NONE;
}

static error_t exec_if(env_t *env, stmt_t *s) {
    value_t cond;
    EVAL_CHECK(eval_expr(env, s->if_stmt.condition, &cond));
    int is_true = val_is_true(&cond);
    val_clear(&cond);

    if (is_true) {
        if (s->if_stmt.then_body)
            return eval_stmts(env, s->if_stmt.then_body);
    } else {
        if (s->if_stmt.else_body)
            return eval_stmts(env, s->if_stmt.else_body);
    }
    return ERR_NONE;
}

static error_t exec_for(env_t *env, stmt_t *s) {
    /* Evaluate start, end, step */
    value_t start_val, end_val, step_val;
    EVAL_CHECK(eval_expr(env, s->for_stmt.start, &start_val));

    error_t err = eval_expr(env, s->for_stmt.end, &end_val);
    if (err != ERR_NONE) { val_clear(&start_val); return err; }

    if (s->for_stmt.step) {
        err = eval_expr(env, s->for_stmt.step, &step_val);
        if (err != ERR_NONE) {
            val_clear(&start_val);
            val_clear(&end_val);
            return err;
        }
    } else {
        step_val = val_integer(1);
    }

    /* Convert to doubles for uniform handling */
    double counter, limit, step;
    val_to_double(&start_val, &counter);
    val_to_double(&end_val, &limit);
    val_to_double(&step_val, &step);
    val_clear(&start_val);
    val_clear(&end_val);
    val_clear(&step_val);

    if (step == 0.0) return ERR_ILLEGAL_FUNCTION_CALL;

    /* Loop */
    while (1) {
        /* Check termination */
        if (step > 0 && counter > limit) break;
        if (step < 0 && counter < limit) break;

        /* Set loop variable */
        if (s->for_stmt.var_type == VAL_INTEGER) {
            value_t v = val_integer((int)counter);
            env_set(env, s->for_stmt.var_name, &v);
        } else {
            value_t v = val_double(counter);
            env_set(env, s->for_stmt.var_name, &v);
        }

        /* Execute body */
        if (s->for_stmt.body) {
            err = eval_stmts(env, s->for_stmt.body);
            if (err == ERR_BREAK) break;
            if (err != ERR_NONE) return err;
        }

        /* Increment */
        counter += step;
    }

    return ERR_NONE;
}

static error_t exec_while(env_t *env, stmt_t *s) {
    while (1) {
        value_t cond;
        EVAL_CHECK(eval_expr(env, s->while_stmt.condition, &cond));
        int is_true = val_is_true(&cond);
        val_clear(&cond);
        if (!is_true) break;

        if (s->while_stmt.body) {
            error_t err = eval_stmts(env, s->while_stmt.body);
            if (err == ERR_BREAK) break;
            if (err != ERR_NONE) return err;
        }
    }
    return ERR_NONE;
}

error_t eval_stmt(env_t *env, stmt_t *s) {
    switch (s->type) {
        case STMT_PRINT:  return exec_print(env, s);
        case STMT_INPUT:  return exec_input(env, s);
        case STMT_ASSIGN: return exec_assign(env, s);
        case STMT_IF:     return exec_if(env, s);
        case STMT_FOR:    return exec_for(env, s);
        case STMT_WHILE:  return exec_while(env, s);
        case STMT_END:    return ERR_EXIT;
        case STMT_REM:    return ERR_NONE;
    }
    return ERR_INTERNAL;
}

error_t eval_stmts(env_t *env, stmt_t *stmts) {
    stmt_t *s = stmts;
    while (s) {
        error_t err = eval_stmt(env, s);
        if (err != ERR_NONE) return err;
        s = s->next;
    }
    return ERR_NONE;
}
