#include "ast.h"
#include <stdlib.h>
#include <string.h>

/* --- Expression constructors --- */

expr_t *expr_literal(value_t val, int line) {
    expr_t *e = calloc(1, sizeof(expr_t));
    if (!e) return NULL;
    e->type = EXPR_LITERAL;
    e->line = line;
    e->literal = val_copy(&val);
    return e;
}

expr_t *expr_variable(const char *name, val_type_t type, int line) {
    expr_t *e = calloc(1, sizeof(expr_t));
    if (!e) return NULL;
    e->type = EXPR_VARIABLE;
    e->line = line;
    strncpy(e->var.name, name, 63);
    e->var.var_type = type;
    return e;
}

expr_t *expr_unary(unaryop_t op, expr_t *operand, int line) {
    expr_t *e = calloc(1, sizeof(expr_t));
    if (!e) return NULL;
    e->type = EXPR_UNARY;
    e->line = line;
    e->unary.op = op;
    e->unary.operand = operand;
    return e;
}

expr_t *expr_binary(binop_t op, expr_t *left, expr_t *right, int line) {
    expr_t *e = calloc(1, sizeof(expr_t));
    if (!e) return NULL;
    e->type = EXPR_BINARY;
    e->line = line;
    e->binary.op = op;
    e->binary.left = left;
    e->binary.right = right;
    return e;
}

expr_t *expr_compare(cmpop_t op, expr_t *left, expr_t *right, int line) {
    expr_t *e = calloc(1, sizeof(expr_t));
    if (!e) return NULL;
    e->type = EXPR_COMPARE;
    e->line = line;
    e->compare.op = op;
    e->compare.left = left;
    e->compare.right = right;
    return e;
}

expr_t *expr_call(const char *name, int line) {
    expr_t *e = calloc(1, sizeof(expr_t));
    if (!e) return NULL;
    e->type = EXPR_CALL;
    e->line = line;
    strncpy(e->call.name, name, 63);
    e->call.nargs = 0;
    return e;
}

void expr_call_add_arg(expr_t *call, expr_t *arg) {
    if (call->call.nargs < 8)
        call->call.args[call->call.nargs++] = arg;
}

/* --- Expression destructor --- */

void expr_free(expr_t *e) {
    if (!e) return;
    switch (e->type) {
        case EXPR_LITERAL:
            val_clear(&e->literal);
            break;
        case EXPR_VARIABLE:
            break;
        case EXPR_UNARY:
            expr_free(e->unary.operand);
            break;
        case EXPR_BINARY:
            expr_free(e->binary.left);
            expr_free(e->binary.right);
            break;
        case EXPR_COMPARE:
            expr_free(e->compare.left);
            expr_free(e->compare.right);
            break;
        case EXPR_CALL:
            for (int i = 0; i < e->call.nargs; i++)
                expr_free(e->call.args[i]);
            break;
    }
    free(e);
}

/* --- Statement constructors --- */

stmt_t *stmt_alloc(stmt_type_t type, int line) {
    stmt_t *s = calloc(1, sizeof(stmt_t));
    if (!s) return NULL;
    s->type = type;
    s->line = line;
    s->next = NULL;
    return s;
}

stmt_t *stmt_print(int line) {
    stmt_t *s = stmt_alloc(STMT_PRINT, line);
    if (!s) return NULL;
    s->print.items = NULL;
    s->print.nitems = 0;
    return s;
}

void stmt_print_add(stmt_t *s, expr_t *expr, char sep) {
    int n = s->print.nitems;
    s->print.items = realloc(s->print.items, (n + 1) * sizeof(print_item_t));
    s->print.items[n].expr = expr;
    s->print.items[n].sep = sep;
    s->print.nitems = n + 1;
}

stmt_t *stmt_input(const char *prompt, int line) {
    stmt_t *s = stmt_alloc(STMT_INPUT, line);
    if (!s) return NULL;
    s->input.prompt = prompt ? strdup(prompt) : NULL;
    s->input.nvars = 0;
    return s;
}

void stmt_input_add_var(stmt_t *s, const char *name, val_type_t type) {
    int n = s->input.nvars;
    if (n >= 8) return;
    strncpy(s->input.varnames[n], name, 63);
    s->input.vartypes[n] = type;
    s->input.nvars = n + 1;
}

stmt_t *stmt_assign(const char *name, val_type_t type, expr_t *value, int line) {
    stmt_t *s = stmt_alloc(STMT_ASSIGN, line);
    if (!s) return NULL;
    strncpy(s->assign.name, name, 63);
    s->assign.var_type = type;
    s->assign.value = value;
    return s;
}

stmt_t *stmt_if(expr_t *cond, stmt_t *then_body, stmt_t *else_body, int line) {
    stmt_t *s = stmt_alloc(STMT_IF, line);
    if (!s) return NULL;
    s->if_stmt.condition = cond;
    s->if_stmt.then_body = then_body;
    s->if_stmt.else_body = else_body;
    return s;
}

stmt_t *stmt_for(const char *var, val_type_t type,
                 expr_t *start, expr_t *end, expr_t *step,
                 stmt_t *body, int line) {
    stmt_t *s = stmt_alloc(STMT_FOR, line);
    if (!s) return NULL;
    strncpy(s->for_stmt.var_name, var, 63);
    s->for_stmt.var_type = type;
    s->for_stmt.start = start;
    s->for_stmt.end = end;
    s->for_stmt.step = step;
    s->for_stmt.body = body;
    return s;
}

stmt_t *stmt_while(expr_t *cond, stmt_t *body, int line) {
    stmt_t *s = stmt_alloc(STMT_WHILE, line);
    if (!s) return NULL;
    s->while_stmt.condition = cond;
    s->while_stmt.body = body;
    return s;
}

stmt_t *stmt_end(int line) {
    return stmt_alloc(STMT_END, line);
}

/* --- Statement destructor --- */

void stmt_free(stmt_t *s) {
    while (s) {
        stmt_t *next = s->next;
        switch (s->type) {
            case STMT_PRINT:
                for (int i = 0; i < s->print.nitems; i++)
                    expr_free(s->print.items[i].expr);
                free(s->print.items);
                break;
            case STMT_INPUT:
                free(s->input.prompt);
                break;
            case STMT_ASSIGN:
                expr_free(s->assign.value);
                break;
            case STMT_IF:
                expr_free(s->if_stmt.condition);
                stmt_free(s->if_stmt.then_body);
                stmt_free(s->if_stmt.else_body);
                break;
            case STMT_FOR:
                expr_free(s->for_stmt.start);
                expr_free(s->for_stmt.end);
                expr_free(s->for_stmt.step);
                stmt_free(s->for_stmt.body);
                break;
            case STMT_WHILE:
                expr_free(s->while_stmt.condition);
                stmt_free(s->while_stmt.body);
                break;
            case STMT_END:
            case STMT_REM:
                break;
        }
        free(s);
        s = next;
    }
}

void stmt_append(stmt_t **head, stmt_t *s) {
    if (!*head) {
        *head = s;
    } else {
        stmt_t *tail = *head;
        while (tail->next)
            tail = tail->next;
        tail->next = s;
    }
}
