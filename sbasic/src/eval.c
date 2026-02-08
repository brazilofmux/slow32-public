#include "eval.h"
#include "builtin.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* --- Procedure and label tables --- */

#define MAX_PROCS 128
#define MAX_LABELS 256
#define MAX_GOSUB_DEPTH 64

typedef struct {
    char name[64];
    stmt_t *def;
} proc_entry_t;

typedef struct {
    char name[64];
    int index;
} label_entry_t;

static proc_entry_t proc_table[MAX_PROCS];
static int proc_count = 0;

static label_entry_t label_table[MAX_LABELS];
static int label_count = 0;

static int gosub_stack[MAX_GOSUB_DEPTH];
static int gosub_top = 0;

static int goto_target = -1;

/* Pointer to global env for SHARED variable access */
static env_t *program_global_env = NULL;

/* --- Lookup helpers --- */

static proc_entry_t *find_proc(const char *name) {
    for (int i = 0; i < proc_count; i++) {
        if (strcmp(proc_table[i].name, name) == 0)
            return &proc_table[i];
    }
    return NULL;
}

static int find_label(const char *name) {
    for (int i = 0; i < label_count; i++) {
        if (strcmp(label_table[i].name, name) == 0)
            return label_table[i].index;
    }
    return -1;
}

/* --- User-defined procedure call --- */

static error_t call_proc(env_t *caller_env, proc_entry_t *proc,
                          expr_t **arg_exprs, int nargs, value_t *out) {
    stmt_t *def = proc->def;
    int is_function = (def->type == STMT_FUNC_DEF);

    /* Check argument count */
    if (nargs != def->proc_def.nparams)
        return ERR_ILLEGAL_FUNCTION_CALL;

    /* Evaluate arguments in caller's scope */
    value_t arg_vals[16];
    for (int i = 0; i < nargs; i++) {
        error_t err = eval_expr(caller_env, arg_exprs[i], &arg_vals[i]);
        if (err != ERR_NONE) {
            for (int j = 0; j < i; j++) val_clear(&arg_vals[j]);
            return err;
        }
    }

    /* Create isolated local env (no parent chain) */
    env_t *local = env_create(NULL);

    /* Bind parameters */
    for (int i = 0; i < nargs; i++) {
        env_set(local, def->proc_def.params[i], &arg_vals[i]);
        val_clear(&arg_vals[i]);
    }

    /* Collect SHARED variable names from body (top-level scan) */
    char shared_names[32][64];
    int nshared = 0;
    for (stmt_t *s = def->proc_def.body; s && nshared < 32; s = s->next) {
        if (s->type == STMT_SHARED) {
            for (int i = 0; i < s->shared.nvars && nshared < 32; i++) {
                strncpy(shared_names[nshared], s->shared.varnames[i], 63);
                shared_names[nshared][63] = '\0';
                nshared++;
            }
        }
    }

    /* Copy-in SHARED variables from global */
    for (int i = 0; i < nshared; i++) {
        value_t *gv = env_get(program_global_env, shared_names[i]);
        if (gv) env_set(local, shared_names[i], gv);
    }

    /* Execute body */
    error_t err = eval_stmts(local, def->proc_def.body);

    /* Handle EXIT SUB/FUNCTION */
    if (err == ERR_EXIT_SUB && !is_function) err = ERR_NONE;
    if (err == ERR_EXIT_FUNCTION && is_function) err = ERR_NONE;

    /* For FUNCTION: retrieve return value from function-name variable */
    if (is_function && out) {
        if (err == ERR_NONE) {
            value_t *retval = env_get(local, def->proc_def.name);
            if (retval)
                *out = val_copy(retval);
            else
                *out = val_double(0.0);
        }
    }

    /* Copy-out SHARED variables back to global */
    for (int i = 0; i < nshared; i++) {
        value_t *lv = env_get(local, shared_names[i]);
        if (lv) env_set(program_global_env, shared_names[i], lv);
    }

    env_destroy(local);
    return err;
}

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
            /* Check for user-defined function first */
            proc_entry_t *proc = find_proc(e->call.name);
            if (proc && proc->def->type == STMT_FUNC_DEF) {
                return call_proc(env, proc, e->call.args, e->call.nargs, out);
            }

            /* Fall back to built-in functions */
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
    if (s->input.prompt)
        printf("%s", s->input.prompt);
    else
        printf("? ");

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

    const char *p = line;
    for (int i = 0; i < s->input.nvars; i++) {
        while (*p == ' ') p++;

        if (s->input.vartypes[i] == VAL_STRING) {
            const char *start = p;
            while (*p && *p != ',') p++;
            int len = (int)(p - start);
            while (len > 0 && start[len - 1] == ' ') len--;
            value_t v = val_string(start, len);
            env_set(env, s->input.varnames[i], &v);
            val_clear(&v);
        } else {
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

        if (*p == ',') p++;
    }

    return ERR_NONE;
}

static error_t exec_assign(env_t *env, stmt_t *s) {
    /* Check for CONST reassignment */
    if (env_is_const(env, s->assign.name))
        return ERR_CONST_REASSIGN;

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

    double counter, limit, step;
    val_to_double(&start_val, &counter);
    val_to_double(&end_val, &limit);
    val_to_double(&step_val, &step);
    val_clear(&start_val);
    val_clear(&end_val);
    val_clear(&step_val);

    if (step == 0.0) return ERR_ILLEGAL_FUNCTION_CALL;

    while (1) {
        if (step > 0 && counter > limit) break;
        if (step < 0 && counter < limit) break;

        if (s->for_stmt.var_type == VAL_INTEGER) {
            value_t v = val_integer((int)counter);
            env_set(env, s->for_stmt.var_name, &v);
        } else {
            value_t v = val_double(counter);
            env_set(env, s->for_stmt.var_name, &v);
        }

        if (s->for_stmt.body) {
            err = eval_stmts(env, s->for_stmt.body);
            if (err == ERR_BREAK || err == ERR_EXIT_FOR) break;
            if (err != ERR_NONE) return err;
        }

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
            if (err == ERR_BREAK || err == ERR_EXIT_WHILE) break;
            if (err != ERR_NONE) return err;
        }
    }
    return ERR_NONE;
}

static error_t exec_do_loop(env_t *env, stmt_t *s) {
    while (1) {
        /* Pre-condition: DO WHILE/UNTIL expr */
        if (s->do_loop.pre_cond) {
            value_t cond;
            EVAL_CHECK(eval_expr(env, s->do_loop.pre_cond, &cond));
            int is_true = val_is_true(&cond);
            val_clear(&cond);
            if (s->do_loop.pre_until) is_true = !is_true;
            if (!is_true) break;
        }

        /* Body */
        if (s->do_loop.body) {
            error_t err = eval_stmts(env, s->do_loop.body);
            if (err == ERR_EXIT_DO) break;
            if (err != ERR_NONE) return err;
        }

        /* Post-condition: LOOP WHILE/UNTIL expr */
        if (s->do_loop.post_cond) {
            value_t cond;
            EVAL_CHECK(eval_expr(env, s->do_loop.post_cond, &cond));
            int is_true = val_is_true(&cond);
            val_clear(&cond);
            if (s->do_loop.post_until) is_true = !is_true;
            if (!is_true) break;
        }

        /* Infinite loop if no conditions (DO ... LOOP) */
    }
    return ERR_NONE;
}

static error_t exec_select(env_t *env, stmt_t *s) {
    value_t test;
    EVAL_CHECK(eval_expr(env, s->select_stmt.test_expr, &test));

    case_clause_t *matched = NULL;
    case_clause_t *else_clause = NULL;

    for (case_clause_t *c = s->select_stmt.clauses; c; c = c->next) {
        if (c->is_else) {
            else_clause = c;
            continue;
        }

        int found = 0;
        for (int i = 0; i < c->nmatches && !found; i++) {
            case_match_t *m = &c->matches[i];
            if (m->match_type == 0) {
                /* Value match */
                value_t v;
                error_t err = eval_expr(env, m->expr1, &v);
                if (err != ERR_NONE) { val_clear(&test); return err; }
                int cmp;
                err = val_compare(&test, &v, &cmp);
                val_clear(&v);
                if (err != ERR_NONE) { val_clear(&test); return err; }
                if (cmp == 0) found = 1;
            } else if (m->match_type == 1) {
                /* Range match: val1 TO val2 */
                value_t lo, hi;
                error_t err = eval_expr(env, m->expr1, &lo);
                if (err != ERR_NONE) { val_clear(&test); return err; }
                err = eval_expr(env, m->expr2, &hi);
                if (err != ERR_NONE) { val_clear(&lo); val_clear(&test); return err; }
                int cmp_lo, cmp_hi;
                val_compare(&test, &lo, &cmp_lo);
                val_compare(&test, &hi, &cmp_hi);
                val_clear(&lo);
                val_clear(&hi);
                if (cmp_lo >= 0 && cmp_hi <= 0) found = 1;
            } else if (m->match_type == 2) {
                /* IS comparison */
                value_t v;
                error_t err = eval_expr(env, m->expr1, &v);
                if (err != ERR_NONE) { val_clear(&test); return err; }
                int cmp;
                err = val_compare(&test, &v, &cmp);
                val_clear(&v);
                if (err != ERR_NONE) { val_clear(&test); return err; }
                switch (m->is_op) {
                    case CMP_EQ: found = (cmp == 0); break;
                    case CMP_NE: found = (cmp != 0); break;
                    case CMP_LT: found = (cmp < 0); break;
                    case CMP_GT: found = (cmp > 0); break;
                    case CMP_LE: found = (cmp <= 0); break;
                    case CMP_GE: found = (cmp >= 0); break;
                }
            }
        }

        if (found) {
            matched = c;
            break;
        }
    }

    val_clear(&test);

    if (!matched) matched = else_clause;

    if (matched && matched->body)
        return eval_stmts(env, matched->body);

    return ERR_NONE;
}

static error_t exec_const(env_t *env, stmt_t *s) {
    value_t v;
    EVAL_CHECK(eval_expr(env, s->const_stmt.value, &v));

    /* Coerce to declared type */
    if (s->const_stmt.var_type == VAL_INTEGER && v.type != VAL_INTEGER) {
        int iv;
        error_t err = val_to_integer(&v, &iv);
        val_clear(&v);
        if (err != ERR_NONE) return err;
        v = val_integer(iv);
    } else if (s->const_stmt.var_type == VAL_STRING && v.type != VAL_STRING) {
        char buf[256];
        error_t err = val_to_string(&v, buf, sizeof(buf));
        val_clear(&v);
        if (err != ERR_NONE) return err;
        v = val_string_cstr(buf);
    }

    env_set_const(env, s->const_stmt.name, &v);
    val_clear(&v);
    return ERR_NONE;
}

static error_t exec_call_stmt(env_t *env, stmt_t *s) {
    proc_entry_t *proc = find_proc(s->call_stmt.name);
    if (!proc)
        return ERR_UNDEFINED_PROC;

    if (proc->def->type == STMT_FUNC_DEF) {
        /* Calling a FUNCTION as a statement — discard return value */
        value_t result;
        error_t err = call_proc(env, proc, s->call_stmt.args,
                                s->call_stmt.nargs, &result);
        if (err == ERR_NONE) val_clear(&result);
        return err;
    }

    return call_proc(env, proc, s->call_stmt.args,
                     s->call_stmt.nargs, NULL);
}

/* --- Statement dispatch --- */

error_t eval_stmt(env_t *env, stmt_t *s) {
    switch (s->type) {
        case STMT_PRINT:    return exec_print(env, s);
        case STMT_INPUT:    return exec_input(env, s);
        case STMT_ASSIGN:   return exec_assign(env, s);
        case STMT_IF:       return exec_if(env, s);
        case STMT_FOR:      return exec_for(env, s);
        case STMT_WHILE:    return exec_while(env, s);
        case STMT_DO_LOOP:  return exec_do_loop(env, s);
        case STMT_SELECT:   return exec_select(env, s);
        case STMT_CALL:     return exec_call_stmt(env, s);
        case STMT_CONST:    return exec_const(env, s);
        case STMT_END:      return ERR_EXIT;
        case STMT_REM:      return ERR_NONE;
        case STMT_LABEL:    return ERR_NONE;
        case STMT_DECLARE:  return ERR_NONE;
        case STMT_SHARED:   return ERR_NONE; /* handled by call_proc */

        case STMT_SUB_DEF:
        case STMT_FUNC_DEF:
            return ERR_NONE; /* definitions collected in first pass */

        case STMT_EXIT:
            switch (s->exit_stmt.what) {
                case EXIT_FOR:      return ERR_EXIT_FOR;
                case EXIT_WHILE:    return ERR_EXIT_WHILE;
                case EXIT_DO:       return ERR_EXIT_DO;
                case EXIT_SUB:      return ERR_EXIT_SUB;
                case EXIT_FUNCTION: return ERR_EXIT_FUNCTION;
            }
            return ERR_INTERNAL;

        case STMT_GOTO: {
            int idx = find_label(s->goto_stmt.label);
            if (idx < 0) return ERR_UNDEFINED_LABEL;
            goto_target = idx;
            return ERR_GOTO;
        }

        case STMT_GOSUB: {
            int idx = find_label(s->goto_stmt.label);
            if (idx < 0) return ERR_UNDEFINED_LABEL;
            goto_target = idx;
            return ERR_GOSUB;
        }

        case STMT_RETURN:
            return ERR_RETURN;
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

/* --- Program execution with GOTO/GOSUB/SUB/FUNCTION support --- */

error_t eval_program(env_t *env, stmt_t *program) {
    /* Flatten linked list to array for indexed access */
    int count = 0;
    for (stmt_t *s = program; s; s = s->next)
        count++;

    if (count == 0) return ERR_NONE;

    stmt_t **stmts = malloc(count * sizeof(stmt_t *));
    if (!stmts) return ERR_OUT_OF_MEMORY;

    int i = 0;
    for (stmt_t *s = program; s; s = s->next)
        stmts[i++] = s;

    /* Save global env reference for SHARED access */
    program_global_env = env;

    /* First pass: collect labels and procedure definitions */
    proc_count = 0;
    label_count = 0;

    for (i = 0; i < count; i++) {
        if (stmts[i]->type == STMT_LABEL) {
            if (label_count < MAX_LABELS) {
                strncpy(label_table[label_count].name,
                        stmts[i]->label.name, 63);
                label_table[label_count].name[63] = '\0';
                label_table[label_count].index = i;
                label_count++;
            }
        }
        if (stmts[i]->type == STMT_SUB_DEF || stmts[i]->type == STMT_FUNC_DEF) {
            if (proc_count < MAX_PROCS) {
                strncpy(proc_table[proc_count].name,
                        stmts[i]->proc_def.name, 63);
                proc_table[proc_count].name[63] = '\0';
                proc_table[proc_count].def = stmts[i];
                proc_count++;
            }
        }
    }

    /* Execute with program counter */
    int pc = 0;
    gosub_top = 0;
    error_t result = ERR_NONE;

    while (pc < count) {
        /* Skip SUB/FUNCTION definitions during main flow */
        if (stmts[pc]->type == STMT_SUB_DEF ||
            stmts[pc]->type == STMT_FUNC_DEF) {
            pc++;
            continue;
        }

        error_t err = eval_stmt(env, stmts[pc]);

        if (err == ERR_NONE) {
            pc++;
            continue;
        }

        if (err == ERR_EXIT) {
            break;
        }

        if (err == ERR_GOTO) {
            pc = goto_target;
            continue;
        }

        if (err == ERR_GOSUB) {
            if (gosub_top >= MAX_GOSUB_DEPTH) {
                result = ERR_OUT_OF_MEMORY;
                break;
            }
            gosub_stack[gosub_top++] = pc + 1;
            pc = goto_target;
            continue;
        }

        if (err == ERR_RETURN) {
            if (gosub_top <= 0) {
                result = ERR_RETURN_WITHOUT_GOSUB;
                break;
            }
            pc = gosub_stack[--gosub_top];
            continue;
        }

        /* Any other error is a real error */
        result = err;
        break;
    }

    free(stmts);
    return result;
}
