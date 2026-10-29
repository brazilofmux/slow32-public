/* ast.c — Compile expressions to AST, evaluate AST trees.
 *
 * The compile phase mirrors expr.c's recursive descent parser exactly,
 * but builds tree nodes instead of computing values.  The eval phase
 * walks the tree, using the same type-checking and operator semantics
 * as the original parser.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>
#include "ast.h"
#include "command.h"
#include "memvar.h"
#include "set.h"
#include "date.h"
#include "util.h"
#include "lex.h"

/* Forward declaration for func.c */
int func_call(expr_ctx_t *ctx, const char *name, value_t *args, int nargs, value_t *result);

/* ================================================================
 *  Node constructors
 * ================================================================ */

static ast_node_t *node_alloc(ast_type_t type) {
    ast_node_t *n = calloc(1, sizeof(ast_node_t));
    if (n) n->type = type;
    return n;
}

static ast_node_t *node_literal(value_t val) {
    ast_node_t *n = node_alloc(AST_LITERAL);
    if (n) n->literal = val;
    return n;
}

static ast_node_t *node_memvar(const char *name) {
    ast_node_t *n = node_alloc(AST_MEMVAR);
    if (n) str_copy(n->name, name, sizeof(n->name));
    return n;
}

static ast_node_t *node_alias_field(const char *alias, const char *field) {
    ast_node_t *n = node_alloc(AST_ALIAS_FIELD);
    if (n) {
        str_copy(n->alias_field.alias, alias, sizeof(n->alias_field.alias));
        str_copy(n->alias_field.field, field, sizeof(n->alias_field.field));
    }
    return n;
}

static ast_node_t *node_array(const char *name, ast_node_t *idx1, ast_node_t *idx2) {
    ast_node_t *n = node_alloc(AST_ARRAY_ACCESS);
    if (n) {
        str_copy(n->array.name, name, sizeof(n->array.name));
        n->array.index1 = idx1;
        n->array.index2 = idx2;
    }
    return n;
}

static ast_node_t *node_literal(value_t v);

static int int_literal(const ast_node_t *n) {
    return n && n->type == AST_LITERAL && n->literal.type == VAL_NUM && n->literal.ndec == 0;
}

static ast_node_t *node_binop(ast_op_t op, ast_node_t *left, ast_node_t *right) {
    ast_node_t *n;
    /* Two integer literals divided with a whole quotient fold to an integer
     * at compile time (10/5 shows 2); at run time / and % take SET
     * DECIMALS (24/4 from variables shows 6.00) */
    if ((op == OP_DIV || op == OP_MOD) && int_literal(left) && int_literal(right) &&
        right->literal.num != 0.0) {
        double q = op == OP_DIV ? left->literal.num / right->literal.num
                                : fmod(left->literal.num, right->literal.num);
        if (q == floor(q)) {
            ast_free(left);
            ast_free(right);
            return node_literal(val_num_dec(q, 0, 0));
        }
    }
    n = node_alloc(AST_BINOP);
    if (n) {
        n->binop.op = op;
        n->binop.left = left;
        n->binop.right = right;
    }
    return n;
}

static ast_node_t *node_unop(ast_op_t op, ast_node_t *child) {
    ast_node_t *n = node_alloc(AST_UNOP);
    if (n) {
        n->unop.op = op;
        n->unop.child = child;
    }
    return n;
}

static ast_node_t *node_call(const char *name, ast_node_t **args, int nargs) {
    ast_node_t *n = node_alloc(AST_FUNC_CALL);
    if (n) {
        str_copy(n->call.name, name, sizeof(n->call.name));
        n->call.args = args;
        n->call.nargs = nargs;
    }
    return n;
}

/* ================================================================
 *  ast_free — recursive
 * ================================================================ */

void ast_free(ast_node_t *node) {
    int i;
    if (!node) return;

    switch (node->type) {
    case AST_LITERAL:
    case AST_FIELD:
    case AST_MEMVAR:
        break;
    case AST_ALIAS_FIELD:
        break;
    case AST_ARRAY_ACCESS:
        ast_free(node->array.index1);
        ast_free(node->array.index2);
        break;
    case AST_BINOP:
        ast_free(node->binop.left);
        ast_free(node->binop.right);
        break;
    case AST_UNOP:
        ast_free(node->unop.child);
        break;
    case AST_FUNC_CALL:
        for (i = 0; i < node->call.nargs; i++)
            ast_free(node->call.args[i]);
        free(node->call.args);
        break;
    case AST_ASSIGN:
        ast_free(node->assign.target);
        ast_free(node->assign.value);
        break;
    case AST_INCDEC:
        ast_free(node->incdec.target);
        break;
    }
    free(node);
}

/* ================================================================
 *  Compile phase — recursive descent → tree
 * ================================================================ */

/* Forward declarations */
static ast_node_t *ast_parse_expr(lexer_t *l, memvar_store_t *store, const char **error);
static ast_node_t *ast_parse_or(lexer_t *l, memvar_store_t *store, const char **error);
static ast_node_t *ast_parse_and(lexer_t *l, memvar_store_t *store, const char **error);
static ast_node_t *ast_parse_not(lexer_t *l, memvar_store_t *store, const char **error);
static ast_node_t *ast_parse_compare(lexer_t *l, memvar_store_t *store, const char **error);
static ast_node_t *ast_parse_add(lexer_t *l, memvar_store_t *store, const char **error);
static ast_node_t *ast_parse_mul(lexer_t *l, memvar_store_t *store, const char **error);
static ast_node_t *ast_parse_power(lexer_t *l, memvar_store_t *store, const char **error);
static ast_node_t *ast_parse_unary(lexer_t *l, memvar_store_t *store, const char **error);
static ast_node_t *ast_parse_primary(lexer_t *l, memvar_store_t *store, const char **error);

static int is_lvalue(const ast_node_t *n) {
    return n && (n->type == AST_MEMVAR || n->type == AST_ARRAY_ACCESS ||
                 n->type == AST_ALIAS_FIELD || n->type == AST_FIELD);
}

/* expr → or_expr [ (:= | += | -= | *= | /= | %= | ^=) expr ]  (right associative) */
static ast_node_t *ast_parse_expr(lexer_t *l, memvar_store_t *store, const char **error) {
    ast_node_t *left = ast_parse_or(l, store, error);
    token_type_t t;
    ast_op_t op;
    if (!left) return NULL;
    t = lex_peek(l);
    switch (t) {
    case TOK_ASSIGN: op = OP_NONE; break;
    case TOK_ADDEQ:  op = OP_ADD; break;
    case TOK_SUBEQ:  op = OP_SUB; break;
    case TOK_MULEQ:  op = OP_MUL; break;
    case TOK_DIVEQ:  op = OP_DIV; break;
    case TOK_MODEQ:  op = OP_MOD; break;
    case TOK_POWEQ:  op = OP_POWER; break;
    default: return left;
    }
    if (!is_lvalue(left)) {
        *error = "Invalid assignment target";
        ast_free(left);
        return NULL;
    }
    lex_next(l);
    {
        ast_node_t *value = ast_parse_expr(l, store, error);
        ast_node_t *n;
        if (!value) { ast_free(left); return NULL; }
        n = node_alloc(AST_ASSIGN);
        if (!n) { ast_free(left); ast_free(value); *error = "Out of memory"; return NULL; }
        n->assign.op = op;
        n->assign.target = left;
        n->assign.value = value;
        return n;
    }
}

static ast_node_t *node_incdec(int delta, int prefix, ast_node_t *target, const char **error) {
    ast_node_t *n;
    if (!is_lvalue(target)) {
        *error = "Invalid ++ or -- operand";
        ast_free(target);
        return NULL;
    }
    n = node_alloc(AST_INCDEC);
    if (!n) { ast_free(target); *error = "Out of memory"; return NULL; }
    n->incdec.delta = delta;
    n->incdec.prefix = prefix;
    n->incdec.target = target;
    return n;
}

/* or_expr → and_expr [ .OR. and_expr ]* */
static ast_node_t *ast_parse_or(lexer_t *l, memvar_store_t *store, const char **error) {
    ast_node_t *left = ast_parse_and(l, store, error);
    if (!left) return NULL;

    while (lex_peek(l) == TOK_OR) {
        ast_node_t *right;
        lex_next(l);
        right = ast_parse_and(l, store, error);
        if (!right) { ast_free(left); return NULL; }
        left = node_binop(OP_OR, left, right);
        if (!left) { *error = "Out of memory"; return NULL; }
    }
    return left;
}

/* and_expr → not_expr [ .AND. not_expr ]* */
static ast_node_t *ast_parse_and(lexer_t *l, memvar_store_t *store, const char **error) {
    ast_node_t *left = ast_parse_not(l, store, error);
    if (!left) return NULL;

    while (lex_peek(l) == TOK_AND) {
        ast_node_t *right;
        lex_next(l);
        right = ast_parse_not(l, store, error);
        if (!right) { ast_free(left); return NULL; }
        left = node_binop(OP_AND, left, right);
        if (!left) { *error = "Out of memory"; return NULL; }
    }
    return left;
}

/* not_expr → [.NOT.] compare */
static ast_node_t *ast_parse_not(lexer_t *l, memvar_store_t *store, const char **error) {
    if (lex_peek(l) == TOK_NOT) {
        ast_node_t *child;
        lex_next(l);
        child = ast_parse_compare(l, store, error);
        if (!child) return NULL;
        return node_unop(OP_NOT, child);
    }
    return ast_parse_compare(l, store, error);
}

/* compare → add_expr [ (= | == | <> | # | < | > | <= | >= | $) add_expr ] */
static ast_node_t *ast_parse_compare(lexer_t *l, memvar_store_t *store, const char **error) {
    ast_node_t *left = ast_parse_add(l, store, error);
    if (!left) return NULL;

    {
        token_type_t op = lex_peek(l);
        ast_op_t ast_op;
        ast_node_t *right;

        switch (op) {
        case TOK_EQ:       ast_op = OP_EQ;       break;
        case TOK_EXACT_EQ: ast_op = OP_EXACT_EQ; break;
        case TOK_NE:       ast_op = OP_NE;       break;
        case TOK_LT:       ast_op = OP_LT;       break;
        case TOK_GT:       ast_op = OP_GT;       break;
        case TOK_LE:       ast_op = OP_LE;       break;
        case TOK_GE:       ast_op = OP_GE;       break;
        case TOK_SUBSTR:   ast_op = OP_SUBSTR;   break;
        default:           return left;  /* no comparison operator */
        }

        lex_next(l);
        right = ast_parse_add(l, store, error);
        if (!right) { ast_free(left); return NULL; }
        left = node_binop(ast_op, left, right);
        if (!left) { *error = "Out of memory"; return NULL; }
    }
    return left;
}

/* add_expr → mul_expr [ (+ | -) mul_expr ]* */
static ast_node_t *ast_parse_add(lexer_t *l, memvar_store_t *store, const char **error) {
    ast_node_t *left = ast_parse_mul(l, store, error);
    if (!left) return NULL;

    for (;;) {
        token_type_t op = lex_peek(l);
        ast_op_t ast_op;
        ast_node_t *right;

        if (op == TOK_PLUS)       ast_op = OP_ADD;
        else if (op == TOK_MINUS) ast_op = OP_SUB;
        else break;

        lex_next(l);
        right = ast_parse_mul(l, store, error);
        if (!right) { ast_free(left); return NULL; }
        left = node_binop(ast_op, left, right);
        if (!left) { *error = "Out of memory"; return NULL; }
    }
    return left;
}

/* mul_expr → power_expr [ (* | /) power_expr ]* */
static ast_node_t *ast_parse_mul(lexer_t *l, memvar_store_t *store, const char **error) {
    ast_node_t *left = ast_parse_power(l, store, error);
    if (!left) return NULL;

    for (;;) {
        token_type_t op = lex_peek(l);
        ast_op_t ast_op;
        ast_node_t *right;

        if (op == TOK_MUL)       ast_op = OP_MUL;
        else if (op == TOK_DIV)  ast_op = OP_DIV;
        else if (op == TOK_MOD)  ast_op = OP_MOD;
        else break;

        lex_next(l);
        right = ast_parse_power(l, store, error);
        if (!right) { ast_free(left); return NULL; }
        left = node_binop(ast_op, left, right);
        if (!left) { *error = "Out of memory"; return NULL; }
    }
    return left;
}

/* power_expr → unary [ (** | ^) unary ] */
static ast_node_t *ast_parse_power(lexer_t *l, memvar_store_t *store, const char **error) {
    ast_node_t *left = ast_parse_unary(l, store, error);
    if (!left) return NULL;

    if (lex_peek(l) == TOK_POWER) {
        ast_node_t *right;
        lex_next(l);
        right = ast_parse_unary(l, store, error);
        if (!right) { ast_free(left); return NULL; }
        left = node_binop(OP_POWER, left, right);
        if (!left) { *error = "Out of memory"; return NULL; }
    }
    return left;
}

/* unary → [-|+] primary */
static ast_node_t *ast_parse_unary(lexer_t *l, memvar_store_t *store, const char **error) {
    token_type_t op = lex_peek(l);

    if (op == TOK_MINUS) {
        ast_node_t *child;
        lex_next(l);
        child = ast_parse_primary(l, store, error);
        if (!child) return NULL;
        return node_unop(OP_NEGATE, child);
    }

    if (op == TOK_PLUS) {
        lex_next(l);
    }

    if (op == TOK_INC || op == TOK_DEC) {   /* prefix ++ / -- */
        ast_node_t *target;
        lex_next(l);
        target = ast_parse_primary(l, store, error);
        if (!target) return NULL;
        return node_incdec(op == TOK_INC ? 1 : -1, 1, target, error);
    }

    {
        ast_node_t *n = ast_parse_primary(l, store, error);
        if (n && (lex_peek(l) == TOK_INC || lex_peek(l) == TOK_DEC) && is_lvalue(n)) {
            int delta = lex_peek(l) == TOK_INC ? 1 : -1;   /* postfix */
            lex_next(l);
            return node_incdec(delta, 0, n, error);
        }
        return n;
    }
}

/* primary → number | string | .T./.F. | {date} | (expr) | func(args) |
 *           alias->field | array[i] | name */
static ast_node_t *ast_parse_primary(lexer_t *l, memvar_store_t *store, const char **error) {
    token_t t = l->current;

    switch (t.type) {
    case TOK_NUMBER:
        lex_next(l);
        return node_literal(val_num_literal(t.num_val, t.text));

    case TOK_STRING:
        lex_next(l);
        return node_literal(val_str(t.text));

    case TOK_LOGIC:
        lex_next(l);
        return node_literal(val_logic(t.logic_val));

    case TOK_DATE:
        lex_next(l);
        return node_literal(val_date(t.date_val));

    case TOK_MACRO:
        *error = "Expected identifier after &";
        return NULL;

    case TOK_LPAREN: {
        ast_node_t *inner;
        lex_next(l);
        inner = ast_parse_expr(l, store, error);
        if (!inner) return NULL;
        if (lex_peek(l) != TOK_RPAREN) {
            *error = "Missing closing parenthesis";
            ast_free(inner);
            return NULL;
        }
        lex_next(l);
        return inner;
    }

    case TOK_IDENT: {
        char name[64];
        str_copy(name, t.text, sizeof(name));
        lex_next(l);

        /* Check for array indexed access: name[i] or name(i) */
        {
            int is_array = 0;
            if (store) {
                value_t mv;
                if (memvar_find(store, name, &mv) == 0 && mv.type == VAL_ARRAY)
                    is_array = 1;
            }

            if ((is_array && (l->current.type == TOK_LPAREN || l->current.type == TOK_LBRACKET)) ||
                (l->current.type == TOK_LBRACKET)) {
                token_type_t end_type = (l->current.type == TOK_LPAREN) ? TOK_RPAREN : TOK_RBRACKET;
                ast_node_t *idx1, *idx2 = NULL;

                lex_next(l);  /* skip [ or ( */
                idx1 = ast_parse_expr(l, store, error);
                if (!idx1) return NULL;

                if (lex_peek(l) == TOK_COMMA) {
                    lex_next(l);
                    idx2 = ast_parse_expr(l, store, error);
                    if (!idx2) { ast_free(idx1); return NULL; }
                }

                if (lex_peek(l) == end_type) {
                    lex_next(l);
                }

                return node_array(name, idx1, idx2);
            }
        }

        /* Check for alias->field reference */
        if (lex_peek(l) == TOK_ARROW) {
            char field_name[64];
            lex_next(l);
            if (lex_peek(l) != TOK_IDENT) {
                *error = "Expected field name after ->";
                return NULL;
            }
            str_copy(field_name, l->current.text, sizeof(field_name));
            lex_next(l);
            return node_alias_field(name, field_name);
        }

        /* Function call: name( */
        if (lex_peek(l) == TOK_LPAREN) {
            ast_node_t **args = NULL;
            int nargs = 0;
            int capacity = 0;

            lex_next(l); /* skip ( */

            if (lex_peek(l) != TOK_RPAREN) {
                for (;;) {
                    ast_node_t *arg = ast_parse_expr(l, store, error);
                    if (!arg) {
                        int j;
                        for (j = 0; j < nargs; j++) ast_free(args[j]);
                        free(args);
                        return NULL;
                    }
                    if (nargs >= MAX_FUNC_ARGS) {
                        int j;
                        *error = "Too many function arguments";
                        for (j = 0; j < nargs; j++) ast_free(args[j]);
                        free(args);
                        ast_free(arg);
                        return NULL;
                    }
                    if (nargs >= capacity) {
                        ast_node_t **new_args;
                        capacity = capacity ? capacity * 2 : 4;
                        new_args = realloc(args, capacity * sizeof(ast_node_t *));
                        if (!new_args) {
                            int j;
                            *error = "Out of memory";
                            for (j = 0; j < nargs; j++) ast_free(args[j]);
                            free(args);
                            ast_free(arg);
                            return NULL;
                        }
                        args = new_args;
                    }
                    args[nargs++] = arg;
                    if (lex_peek(l) == TOK_COMMA) { lex_next(l); continue; }
                    break;
                }
            }

            if (lex_peek(l) != TOK_RPAREN) {
                int j;
                *error = "Missing ) in function call";
                for (j = 0; j < nargs; j++) ast_free(args[j]);
                free(args);
                return NULL;
            }
            lex_next(l);

            return node_call(name, args, nargs);
        }

        /* Bare identifier → resolve at eval time (field first, then memvar) */
        return node_memvar(name);
    }

    default:
        *error = "Unexpected token in expression";
        return NULL;
    }
}

/* ================================================================
 *  Public compile API
 * ================================================================ */

ast_node_t *ast_compile(const char *expr, memvar_store_t *store, const char **error) {
    lexer_t l;
    lexer_init_ext(&l, expr, store);
    *error = NULL;
    return ast_parse_expr(&l, store, error);
}

ast_node_t *ast_compile_adv(const char **pp, memvar_store_t *store, const char **error) {
    lexer_t l;
    ast_node_t *node;
    lexer_init_ext(&l, *pp, store);
    *error = NULL;
    node = ast_parse_expr(&l, store, error);
    *pp = l.token_start;
    return node;
}

/* ================================================================
 *  Eval phase — tree walk
 * ================================================================ */

static int eval_field_from_db(dbf_t *db, const char *name, value_t *result) {
    int idx;
    if (!db || !dbf_is_open(db) || db->current_record == 0) return -1;
    idx = dbf_find_field(db, name);
    if (idx < 0) return -1;
    {
        char raw[256];
        dbf_get_field_raw(db, idx, raw, sizeof(raw));
        switch (db->fields[idx].type) {
        case 'C': *result = val_str(raw); return 0;
        case 'F':
        case 'N': {
            /* a field shows at its own width and decimals */
            int fdec = db->fields[idx].decimals;
            int fw = db->fields[idx].length - (fdec > 0 ? fdec + 1 : 0);
            *result = val_num_dec(atof(raw), fdec, fw);
            return 0;
        }
        case 'D': *result = val_date(date_from_dbf(raw)); return 0;
        case 'L': *result = val_logic(raw[0] == 'T' || raw[0] == 't'); return 0;
        case 'M': {
            int blk = atoi(raw);
            char memo[256];
            if (blk > 0 && dbf_memo_read(db, blk, memo, sizeof(memo)) == 0)
                *result = val_str(memo);
            else
                *result = val_str("");
            return 0;
        }
        }
    }
    return -1;
}

/* ---- The operations, one per node kind: ast_eval walks the tree with
 * these, and compiled code (clip/) calls them directly, so both compute
 * the same thing.  0, or -1 with ctx->error set. ---- */

int ast_eval_field(expr_ctx_t *ctx, const char *name, value_t *result) {
    if (eval_field_from_db(ctx->db, name, result) == 0) return 0;
    snprintf(ctx->err_msg, sizeof(ctx->err_msg), "Field not found: %s", name);
    ctx->error = ctx->err_msg;
    return -1;
}

int ast_eval_name(expr_ctx_t *ctx, const char *name, value_t *result) {
    /* Field-first lookup, then memvar — matches parse_primary in expr.c */
    if (eval_field_from_db(ctx->db, name, result) == 0) return 0;
    /* memvar_find writes *result only when it finds the name */
    if (ctx->vars && memvar_find(ctx->vars, name, result) == 0)
        return 0;
    snprintf(ctx->err_msg, sizeof(ctx->err_msg), "Variable not found: %s", name);
    ctx->error = ctx->err_msg;
    return -1;
}

int ast_eval_alias_field(expr_ctx_t *ctx, const char *alias, const char *field, value_t *result) {
    if (str_icmp(alias, "M") == 0 || str_icmp(alias, "MEMVAR") == 0) {
        if (ctx->vars && memvar_find(ctx->vars, field, result) == 0) return 0;
        snprintf(ctx->err_msg, sizeof(ctx->err_msg), "Variable not found: %s", field);
        ctx->error = ctx->err_msg;
        return -1;
    }
    if (str_icmp(alias, "FIELD") == 0)
        return ast_eval_field(ctx, field, result);
    if (ctx->area_lookup) {
        dbf_t *adb = ctx->area_lookup(alias);
        if (eval_field_from_db(adb, field, result) == 0) return 0;
    }
    snprintf(ctx->err_msg, sizeof(ctx->err_msg), "Cannot resolve %s->%s", alias, field);
    ctx->error = ctx->err_msg;
    return -1;
}

int ast_eval_elem(expr_ctx_t *ctx, const char *name, const value_t *row_v,
                  const value_t *col_v, value_t *result) {
    int row, col = 1, res;
    if (row_v->type != VAL_NUM) { ctx->error = "Array index must be numeric"; return -1; }
    row = (int)row_v->num;
    if (col_v) {
        if (col_v->type != VAL_NUM) { ctx->error = "Array index must be numeric"; return -1; }
        col = (int)col_v->num;
    }
    res = memvar_get_elem(ctx->vars, name, row, col, result);
    if (res == 0) return 0;
    if (res == -2) { ctx->error = "Array index out of bounds"; return -1; }
    ctx->error = "Variable is not an array";
    return -1;
}

int ast_eval_unop(expr_ctx_t *ctx, ast_op_t op, const value_t *child, value_t *result) {
    if (op == OP_NEGATE) {
        if (child->type != VAL_NUM) { ctx->error = "Unary minus requires number"; return -1; }
        *result = val_num_dec(-child->num, child->ndec, child->nwidth);
        return 0;
    }
    if (op == OP_NOT) {
        if (child->type != VAL_LOGIC) { ctx->error = "Type mismatch in .NOT."; return -1; }
        *result = val_logic(!child->logic);
        return 0;
    }
    ctx->error = "Unknown unary operator";
    return -1;
}

/* The decimals Clipper gives an arithmetic result: + and - the larger of
 * the two, * their sum, / and ** SET DECIMALS unless the quotient is whole */
static value_t num_result(ast_op_t op, double r, const value_t *left, const value_t *right) {
    int dl = left->ndec, dr = right->ndec;
    switch (op) {
    case OP_ADD: case OP_SUB: return val_num_dec(r, dl > dr ? dl : dr, 0);
    case OP_MUL:              return val_num_dec(r, dl + dr, 0);
    default:                  return val_num_dec(r, val_set_decimals, 0);
    }
}

int ast_eval_binop(expr_ctx_t *ctx, ast_op_t op, const value_t *left, const value_t *right,
                   value_t *result) {
    switch (op) {

    /* ---- Arithmetic ---- */
    case OP_ADD:
        if (left->type == VAL_NUM && right->type == VAL_NUM) {
            *result = num_result(op, left->num + right->num, left, right);
        } else if (left->type == VAL_CHAR && right->type == VAL_CHAR) {
            /* String concatenation (truncate if too long) */
            char buf[256];
            str_copy(buf, left->str, sizeof(buf));
            {
                int len = strlen(buf);
                int rlen = strlen(right->str);
                int avail = (int)sizeof(buf) - 1 - len;
                if (avail > 0) {
                    if (rlen > avail) rlen = avail;
                    memcpy(buf + len, right->str, rlen);
                    buf[len + rlen] = '\0';
                }
            }
            *result = val_str(buf);
        } else if (left->type == VAL_DATE && right->type == VAL_NUM) {
            *result = val_date(left->date + (int32_t)right->num);
        } else if (left->type == VAL_NUM && right->type == VAL_DATE) {
            *result = val_date(right->date + (int32_t)left->num);
        } else {
            ctx->error = "Type mismatch in +"; return -1;
        }
        return 0;

    case OP_SUB:
        if (left->type == VAL_NUM && right->type == VAL_NUM) {
            *result = num_result(op, left->num - right->num, left, right);
        } else if (left->type == VAL_DATE && right->type == VAL_DATE) {
            *result = val_num((double)(left->date - right->date));
        } else if (left->type == VAL_DATE && right->type == VAL_NUM) {
            *result = val_date(left->date - (int32_t)right->num);
        } else if (left->type == VAL_CHAR && right->type == VAL_CHAR) {
            /* Clipper/FoxPro-style trim-concat:
               trim trailing spaces from left, append right,
               then append trimmed spaces to preserve total length. */
            char buf[256];
            int len;
            int left_len;
            int trimmed_spaces;
            str_copy(buf, left->str, sizeof(buf));
            left_len = strlen(buf);
            len = left_len;
            while (len > 0 && buf[len - 1] == ' ') len--;
            trimmed_spaces = left_len - len;
            buf[len] = '\0';
            {
                int rlen = strlen(right->str);
                int avail = (int)sizeof(buf) - 1 - len;
                if (avail > 0) {
                    if (rlen > avail) rlen = avail;
                    memcpy(buf + len, right->str, rlen);
                    len += rlen;
                }
            }
            {
                int avail = (int)sizeof(buf) - 1 - len;
                if (avail > 0 && trimmed_spaces > 0) {
                    int add_spaces = trimmed_spaces;
                    if (add_spaces > avail) add_spaces = avail;
                    memset(buf + len, ' ', add_spaces);
                    len += add_spaces;
                }
            }
            buf[len] = '\0';
            *result = val_str(buf);
        } else {
            ctx->error = "Type mismatch in -"; return -1;
        }
        return 0;

    case OP_MUL:
        if (left->type != VAL_NUM || right->type != VAL_NUM) {
            ctx->error = "Type mismatch in * or /"; return -1;
        }
        *result = num_result(op, left->num * right->num, left, right);
        return 0;

    case OP_DIV:
        if (left->type != VAL_NUM || right->type != VAL_NUM) {
            ctx->error = "Type mismatch in * or /"; return -1;
        }
        if (right->num == 0.0) { ctx->error = "Division by zero"; return -1; }
        *result = num_result(op, left->num / right->num, left, right);
        return 0;

    case OP_MOD:
        if (left->type != VAL_NUM || right->type != VAL_NUM) {
            ctx->error = "Type mismatch in %"; return -1;
        }
        if (right->num == 0.0) { ctx->error = "Division by zero"; return -1; }
        *result = num_result(OP_MOD, fmod(left->num, right->num), left, right);
        return 0;

    case OP_POWER:
        if (left->type != VAL_NUM || right->type != VAL_NUM) {
            ctx->error = "Type mismatch in **"; return -1;
        }
        {
            double r;
            errno = 0;
            r = pow(left->num, right->num);
            if (!isfinite(r) || errno != 0) {
                ctx->error = "Invalid exponentiation"; return -1;
            }
            *result = num_result(op, r, left, right);
        }
        return 0;

    /* ---- Substring ---- */
    case OP_SUBSTR:
        if (left->type != VAL_CHAR || right->type != VAL_CHAR) {
            ctx->error = "$ requires strings"; return -1;
        }
        *result = val_logic(strstr(right->str, left->str) != NULL);
        return 0;

    /* ---- Comparison ---- */
    case OP_EQ: case OP_EXACT_EQ: case OP_NE:
    case OP_LT: case OP_GT: case OP_LE: case OP_GE: {
        int cmp = 0;

        if (left->type == VAL_NUM && right->type == VAL_NUM) {
            double d = left->num - right->num;
            cmp = (d < 0) ? -1 : (d > 0) ? 1 : 0;
        } else if (left->type == VAL_CHAR && right->type == VAL_CHAR) {
            if (op == OP_EXACT_EQ)
                cmp = str_icmp(left->str, right->str);
            else if (ctx->opts && !((set_options_t *)ctx->opts)->exact)
                cmp = str_nicmp(left->str, right->str, strlen(right->str));
            else
                cmp = str_icmp(left->str, right->str);
        } else if (left->type == VAL_DATE && right->type == VAL_DATE) {
            int32_t d = left->date - right->date;
            cmp = (d < 0) ? -1 : (d > 0) ? 1 : 0;
        } else if (left->type == VAL_LOGIC && right->type == VAL_LOGIC) {
            cmp = left->logic - right->logic;
        } else {
            ctx->error = "Type mismatch in comparison"; return -1;
        }

        switch (op) {
        case OP_EQ:       case OP_EXACT_EQ: *result = val_logic(cmp == 0); break;
        case OP_NE:       *result = val_logic(cmp != 0); break;
        case OP_LT:       *result = val_logic(cmp < 0); break;
        case OP_GT:       *result = val_logic(cmp > 0); break;
        case OP_LE:       *result = val_logic(cmp <= 0); break;
        case OP_GE:       *result = val_logic(cmp >= 0); break;
        default: break;
        }
        return 0;
    }

    /* ---- Logical ---- */
    case OP_AND:
        if (left->type != VAL_LOGIC || right->type != VAL_LOGIC) {
            ctx->error = "Type mismatch in .AND."; return -1;
        }
        *result = val_logic(left->logic && right->logic);
        return 0;

    case OP_OR:
        if (left->type != VAL_LOGIC || right->type != VAL_LOGIC) {
            ctx->error = "Type mismatch in .OR."; return -1;
        }
        *result = val_logic(left->logic || right->logic);
        return 0;

    default:
        ctx->error = "Unknown binary operator";
        return -1;
    }
    return -1;
}

int ast_store_name(expr_ctx_t *ctx, const char *name, const value_t *val) {
    if (!ctx->vars || memvar_set(ctx->vars, name, val) != 0) {
        ctx->error = "Too many memory variables";
        return -1;
    }
    return 0;
}

int ast_store_elem(expr_ctx_t *ctx, const char *name, const value_t *row_v,
                   const value_t *col_v, const value_t *val) {
    int row, col = 1, res;
    if (row_v->type != VAL_NUM || (col_v && col_v->type != VAL_NUM)) {
        ctx->error = "Array index must be numeric";
        return -1;
    }
    row = (int)row_v->num;
    if (col_v) col = (int)col_v->num;
    res = memvar_set_elem(ctx->vars, name, row, col, val);
    if (res == 0) return 0;
    ctx->error = res == -2 ? "Array index out of bounds" : "Variable is not an array";
    return -1;
}

int ast_store_alias_field(expr_ctx_t *ctx, const char *alias, const char *field,
                          const value_t *val) {
    if (str_icmp(alias, "M") == 0 || str_icmp(alias, "MEMVAR") == 0)
        return ast_store_name(ctx, field, val);
    if (cmd_api_field_assign(str_icmp(alias, "FIELD") == 0 ? NULL : alias, field, val) != 0) {
        snprintf(ctx->err_msg, sizeof(ctx->err_msg), "Cannot assign %s->%s", alias, field);
        ctx->error = ctx->err_msg;
        return -1;
    }
    return 0;
}

static int store_to(ast_node_t *t, expr_ctx_t *ctx, const value_t *row, const value_t *col,
                    const value_t *val) {
    switch (t->type) {
    case AST_MEMVAR:       return ast_store_name(ctx, t->name, val);
    case AST_FIELD:        return ast_store_alias_field(ctx, "FIELD", t->name, val);
    case AST_ALIAS_FIELD:  return ast_store_alias_field(ctx, t->alias_field.alias, t->alias_field.field, val);
    case AST_ARRAY_ACCESS: return ast_store_elem(ctx, t->array.name, row, col, val);
    default: ctx->error = "Invalid assignment target"; return -1;
    }
}

/* := op= ++ --: an element's indexes are evaluated once */
static int eval_store(ast_node_t *node, expr_ctx_t *ctx, value_t *result) {
    ast_node_t *t = node->type == AST_ASSIGN ? node->assign.target : node->incdec.target;
    value_t row, col, cur, rhs, nv;
    int has_col = 0;
    if (t->type == AST_ARRAY_ACCESS) {
        if (ast_eval(t->array.index1, ctx, &row) != 0) return -1;
        if (t->array.index2) {
            if (ast_eval(t->array.index2, ctx, &col) != 0) return -1;
            has_col = 1;
        }
    }
    if (node->type == AST_ASSIGN && node->assign.op == OP_NONE) {
        if (ast_eval(node->assign.value, ctx, &nv) != 0) return -1;
    } else {
        if (t->type == AST_ARRAY_ACCESS) {
            if (ast_eval_elem(ctx, t->array.name, &row, has_col ? &col : NULL, &cur) != 0) return -1;
        } else if (ast_eval(t, ctx, &cur) != 0) return -1;
        if (node->type == AST_ASSIGN) {
            if (ast_eval(node->assign.value, ctx, &rhs) != 0) return -1;
            if (ast_eval_binop(ctx, node->assign.op, &cur, &rhs, &nv) != 0) return -1;
        } else {
            value_t one = val_num_dec(1, 0, 0);
            if (ast_eval_binop(ctx, node->incdec.delta > 0 ? OP_ADD : OP_SUB, &cur, &one, &nv) != 0)
                return -1;
        }
    }
    if (store_to(t, ctx, &row, has_col ? &col : NULL, &nv) != 0) return -1;
    *result = (node->type == AST_INCDEC && !node->incdec.prefix) ? cur : nv;
    return 0;
}

int ast_eval(ast_node_t *node, expr_ctx_t *ctx, value_t *result) {
    if (!node) { ctx->error = "NULL AST node"; return -1; }

    switch (node->type) {

    case AST_LITERAL:
        *result = node->literal;
        return 0;

    case AST_FIELD:
        return ast_eval_field(ctx, node->name, result);

    case AST_MEMVAR:
        return ast_eval_name(ctx, node->name, result);

    case AST_ALIAS_FIELD:
        return ast_eval_alias_field(ctx, node->alias_field.alias, node->alias_field.field, result);

    case AST_ARRAY_ACCESS: {
        value_t row_v, col_v;
        if (ast_eval(node->array.index1, ctx, &row_v) != 0) return -1;
        if (row_v.type != VAL_NUM) { ctx->error = "Array index must be numeric"; return -1; }
        if (node->array.index2) {
            if (ast_eval(node->array.index2, ctx, &col_v) != 0) return -1;
            return ast_eval_elem(ctx, node->array.name, &row_v, &col_v, result);
        }
        return ast_eval_elem(ctx, node->array.name, &row_v, NULL, result);
    }

    case AST_FUNC_CALL: {
        value_t *args = NULL;
        int i;
        int rc;

        if (node->call.nargs > MAX_FUNC_ARGS) {
            ctx->error = "Too many function arguments";
            return -1;
        }
        if (node->call.nargs > 0) {
            args = (value_t *)malloc(node->call.nargs * sizeof(value_t));
            if (!args) {
                ctx->error = "Out of memory";
                return -1;
            }
        }
        for (i = 0; i < node->call.nargs; i++) {
            if (ast_eval(node->call.args[i], ctx, &args[i]) != 0) {
                if (args) free(args);
                return -1;
            }
        }
        rc = func_call(ctx, node->call.name, args, node->call.nargs, result);
        if (args) free(args);
        return rc;
    }

    case AST_UNOP: {
        value_t child;
        if (ast_eval(node->unop.child, ctx, &child) != 0) return -1;
        return ast_eval_unop(ctx, node->unop.op, &child, result);
    }

    case AST_BINOP: {
        value_t left, right;
        if (ast_eval(node->binop.left, ctx, &left) != 0) return -1;
        if (ast_eval(node->binop.right, ctx, &right) != 0) return -1;
        return ast_eval_binop(ctx, node->binop.op, &left, &right, result);
    }

    case AST_ASSIGN:
    case AST_INCDEC:
        return eval_store(node, ctx, result);

    } /* end switch(node->type) */

    ctx->error = "Unknown AST node type";
    return -1;
}

/* ----------------------------------------------------------------
 *  Compiled-expression cache, keyed by the expression text.
 *
 *  Every statement re-parsed its expressions on every execution (the
 *  evaluating parser in expr.c, or compile-eval-free here).  The tree for a
 *  given text is the same each time except in two cases, both handled: a
 *  macro (&) expands at compile time, so such text is never cached (2.1);
 *  name(...) is an array or a call according to what exists at compile time
 *  (2.2), so the cache is dropped whenever memvar_array_gen moves.  Trees are
 *  not written during evaluation, so one tree serves every context.
 * ---------------------------------------------------------------- */
#define AST_CACHE_SIZE 1024
typedef struct { char *text; ast_node_t *ast; int consumed; } ast_cache_entry_t;
static ast_cache_entry_t ast_cache[AST_CACHE_SIZE];
static unsigned ast_cache_gen;

static unsigned ast_text_hash(const char *s) {
    unsigned h = 2166136261u;
    while (*s) { h ^= (unsigned char)*s++; h *= 16777619u; }
    return h;
}

static void ast_cache_flush(void) {
    int i;
    for (i = 0; i < AST_CACHE_SIZE; i++) {
        if (ast_cache[i].text) { free(ast_cache[i].text); ast_free(ast_cache[i].ast); ast_cache[i].text = NULL; ast_cache[i].ast = NULL; }
    }
}

/* The tree for `text` (the whole string, or a leading expression of it when
 * adv), compiling on a miss.  NULL: not cacheable or does not compile; the
 * caller falls back to the parser that reports the error. */
static ast_node_t *ast_cached(const char *text, memvar_store_t *store, int adv, int *consumed) {
    ast_cache_entry_t *e;
    if (strchr(text, '&')) return NULL;
    if (ast_cache_gen != memvar_array_gen) { ast_cache_flush(); ast_cache_gen = memvar_array_gen; }
    e = &ast_cache[ast_text_hash(text) & (AST_CACHE_SIZE - 1)];
    if (e->text && strcmp(e->text, text) == 0 && (adv ? e->consumed >= 0 : e->consumed < 0)) {
        if (consumed) *consumed = e->consumed;
        return e->ast;
    }
    {
        const char *error;
        ast_node_t *ast;
        int used = -1;
        if (adv) {
            const char *pp = text;
            ast = ast_compile_adv(&pp, store, &error);
            used = (int)(pp - text);
        } else {
            ast = ast_compile(text, store, &error);
        }
        if (!ast) return NULL;
        if (e->text) { free(e->text); ast_free(e->ast); }
        e->text = strdup(text);
        if (!e->text) { ast_free(ast); e->ast = NULL; return NULL; }
        e->ast = ast;
        e->consumed = used;
        if (consumed) *consumed = used;
        return ast;
    }
}

int ast_eval_dynamic(const char *expr, expr_ctx_t *ctx, value_t *result) {
    ast_node_t *ast = ast_cached(expr, ctx->vars, 0, NULL);
    if (!ast) return expr_eval_str(ctx, expr, result);
    return ast_eval(ast, ctx, result);
}

/* Evaluate the expression at *pp and advance *pp past it, like expr_eval(). */
int ast_eval_adv(expr_ctx_t *ctx, const char **pp, value_t *result) {
    int used = 0;
    ast_node_t *ast = ast_cached(*pp, ctx->vars, 1, &used);
    int rc;
    if (!ast) return expr_eval(ctx, pp, result);
    ctx->error = NULL;
    *result = val_nil();
    rc = ast_eval(ast, ctx, result);
    *pp += used;
    return rc;
}
