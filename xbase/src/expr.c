#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>
#include "expr.h"
#include "gc.h"
#include "memvar.h"
#include "set.h"
#include "date.h"
#include "command.h"
#include "ast.h"
#include "util.h"
#include "lex.h"

/* Forward declarations for func.c */
int func_call(expr_ctx_t *ctx, const char *name, value_t *args, int nargs, value_t *result);

/* Forward declarations for recursive descent */
static int parse_or(expr_ctx_t *ctx, lexer_t *l, value_t *result);
static int parse_and(expr_ctx_t *ctx, lexer_t *l, value_t *result);
static int parse_not(expr_ctx_t *ctx, lexer_t *l, value_t *result);
static int parse_compare(expr_ctx_t *ctx, lexer_t *l, value_t *result);
static int parse_add(expr_ctx_t *ctx, lexer_t *l, value_t *result);
static int parse_mul(expr_ctx_t *ctx, lexer_t *l, value_t *result);
static int parse_power(expr_ctx_t *ctx, lexer_t *l, value_t *result);
static int parse_unary(expr_ctx_t *ctx, lexer_t *l, value_t *result);
static int parse_primary(expr_ctx_t *ctx, lexer_t *l, value_t *result);

/* ---- Value constructors ---- */

int val_set_decimals = 2;
int val_set_fixed;

/* A computed number: an integer when it is whole, else SET DECIMALS
 * places, as Clipper gives a function's result */
value_t val_num(double n) {
    value_t v;
    v.ext = NULL;
    v.len = 0;
    v.type = VAL_NUM;
    v.num = n;
    v.ndec = (n == floor(n) && fabs(n) < 1e15) ? 0 : (short)val_set_decimals;
    v.nwidth = 0;
    return v;
}

value_t val_num_dec(double n, int ndec, int nwidth) {
    value_t v;
    v.ext = NULL;
    v.len = 0;
    v.type = VAL_NUM;
    v.num = n;
    v.ndec = (short)(ndec < 0 ? 0 : ndec > 18 ? 18 : ndec);
    v.nwidth = (short)(nwidth < 0 ? 0 : nwidth);
    return v;
}

value_t val_num_literal(double n, const char *text) {
    const char *dot = text ? strchr(text, '.') : NULL;
    int dec = 0;
    if (dot) {
        while (dot[1 + dec] >= '0' && dot[1 + dec] <= '9') dec++;
    }
    return val_num_dec(n, dec, 0);
}

void val_to_display(const value_t *v, char *buf, int size) {
    if (v->type == VAL_ARRAY || v->type == VAL_BLOCK || v->type == VAL_OBJECT) {
        buf[0] = '\0';   /* ? shows nothing for these (Clipper) */
        return;
    }
    if (v->type == VAL_NUM) {
        int dec = val_set_fixed ? val_set_decimals : v->ndec;
        int width = v->nwidth;
        if (width <= 0) {
            /* ten columns, or the integer digits and one more */
            double a = fabs(v->num);
            int digits = 1;
            while (a >= 10.0 && digits < 40) { a /= 10.0; digits++; }
            width = digits + 1 > 10 ? digits + 1 : 10;
        }
        snprintf(buf, size, "%*.*f", width + (dec > 0 ? dec + 1 : 0), dec, v->num);
        return;
    }
    val_to_string(v, buf, size);
}

value_t val_strn(const char *s, int n) {
    value_t v;
    v.type = VAL_CHAR;
    v.ndec = v.nwidth = 0;
    v.ext = NULL;
    v.len = 0;
    if (n < 0) n = 0;
    if (n <= 255 && memchr(s, 0, (size_t)n) == NULL) {
        memcpy(v.str, s, (size_t)n);
        v.str[n] = '\0';
        return v;
    }
    v.ext = malloc((size_t)n + 1);
    if (!v.ext) { v.str[0] = '\0'; return v; }
    gc_register(v.ext, GC_TEXT, (size_t)n + 1);
    memcpy(v.ext, s, (size_t)n);
    v.ext[n] = '\0';
    v.len = n;
    {
        int k = n < 255 ? n : 255;
        memcpy(v.str, s, (size_t)k);   /* the prefix; a NUL inside ends it for inline readers */
        v.str[k] = '\0';
    }
    return v;
}

value_t val_str(const char *s) {
    return val_strn(s, (int)strlen(s));
}

const char *val_text(const value_t *v) {
    return (v->type == VAL_CHAR && v->ext) ? v->ext : v->str;
}

int val_len(const value_t *v) {
    if (v->type != VAL_CHAR) return 0;
    return v->ext ? v->len : (int)strlen(v->str);
}

value_t val_logic(int b) {
    value_t v;
    v.ext = NULL;
    v.len = 0;
    v.type = VAL_LOGIC;
    v.logic = b ? 1 : 0;
    return v;
}

value_t val_date(int32_t jdn) {
    value_t v;
    v.ext = NULL;
    v.len = 0;
    v.type = VAL_DATE;
    v.date = jdn;
    return v;
}

value_t val_nil(void) {
    value_t v;
    v.ext = NULL;
    v.len = 0;
    v.type = VAL_NIL;
    v.num = 0;
    return v;
}

value_t val_object(object_t *o) {
    value_t v;
    v.ext = NULL;
    v.len = 0;
    v.type = VAL_OBJECT;
    v.ndec = 0;
    v.nwidth = 0;
    v.obj = o;
    return v;
}

object_t *object_new(const char *cls) {
    object_t *o = calloc(1, sizeof(*o));
    gc_register(o, GC_OBJECT, sizeof(*o));
    if (o) snprintf(o->cls, sizeof(o->cls), "%s", cls);
    return o;
}

value_t *object_ivar(object_t *o, const char *name, int create) {
    int i;
    for (i = 0; i < o->n; i++) {
        if (str_icmp(o->names[i], name) == 0) return &o->vals[i];
    }
    if (!create || o->n >= OBJECT_MAX_IVARS) return NULL;
    snprintf(o->names[o->n], sizeof(o->names[0]), "%s", name);
    o->vals[o->n] = val_nil();
    return &o->vals[o->n++];
}

/* ---- Display formatting ---- */

void val_to_string(const value_t *v, char *buf, int size) {
    switch (v->type) {
    case VAL_ARRAY:
        snprintf(buf, size, "{...}");   /* as Clipper shows an array */
        return;
    case VAL_BLOCK:
        snprintf(buf, size, "{||...}");
        return;
    case VAL_OBJECT:
        snprintf(buf, size, "%s", v->obj ? v->obj->cls : "NIL");
        return;
    case VAL_NUM: {
        /* Print integer if no fractional part, else strip trailing zeros */
        double n = v->num;
        /* Check if it's an integer value */
        if (n >= -2147483647.0 && n <= 2147483647.0) {
            int i = (int)n;
            if ((double)i == n) {
                snprintf(buf, size, "%d", i);
                return;
            }
        }
        snprintf(buf, size, "%.10f", n);
        /* Strip trailing zeros */
        {
            int len = strlen(buf);
            int has_dot = 0;
            int k;
            for (k = 0; k < len; k++)
                if (buf[k] == '.') { has_dot = 1; break; }
            if (has_dot) {
                while (len > 1 && buf[len-1] == '0') len--;
                if (len > 0 && buf[len-1] == '.') len--;
                buf[len] = '\0';
            }
        }
        break;
    }
    case VAL_CHAR:
        str_copy(buf, val_text(v), size);   /* as far as buf reaches */
        break;
    case VAL_DATE: {
        char mdy[16];
        date_to_display(v->date, mdy, cmd_get_date_format(), cmd_get_century(), cmd_get_mark());
        str_copy(buf, mdy, size);
        break;
    }
    case VAL_LOGIC:
        str_copy(buf, v->logic ? ".T." : ".F.", size);
        break;
    default:
        str_copy(buf, "NIL", size);
        break;
    }
}

/* ---- Parser ---- */

/* expr → or_expr */
static int parse_expr(expr_ctx_t *ctx, lexer_t *l, value_t *result) {
    return parse_or(ctx, l, result);
}

/* or_expr → and_expr [ .OR. and_expr ]* */
static int parse_or(expr_ctx_t *ctx, lexer_t *l, value_t *result) {
    if (parse_and(ctx, l, result) != 0) return -1;

    while (lex_peek(l) == TOK_OR) {
        value_t right;
        lex_next(l);
        if (parse_and(ctx, l, &right) != 0) return -1;
        if (result->type != VAL_LOGIC || right.type != VAL_LOGIC) {
            ctx->error = "Type mismatch in .OR.";
            return -1;
        }
        result->logic = result->logic || right.logic;
    }
    return 0;
}

/* and_expr → not_expr [ .AND. not_expr ]* */
static int parse_and(expr_ctx_t *ctx, lexer_t *l, value_t *result) {
    if (parse_not(ctx, l, result) != 0) return -1;

    while (lex_peek(l) == TOK_AND) {
        value_t right;
        lex_next(l);
        if (parse_not(ctx, l, &right) != 0) return -1;
        if (result->type != VAL_LOGIC || right.type != VAL_LOGIC) {
            ctx->error = "Type mismatch in .AND.";
            return -1;
        }
        result->logic = result->logic && right.logic;
    }
    return 0;
}

/* not_expr → [.NOT.] compare */
static int parse_not(expr_ctx_t *ctx, lexer_t *l, value_t *result) {
    if (lex_peek(l) == TOK_NOT) {
        lex_next(l);
        if (parse_compare(ctx, l, result) != 0) return -1;
        if (result->type != VAL_LOGIC) {
            ctx->error = "Type mismatch in .NOT.";
            return -1;
        }
        result->logic = !result->logic;
        return 0;
    }
    return parse_compare(ctx, l, result);
}

/* compare → add_expr [ (= | <> | # | < | > | <= | >= | $) add_expr ] */
static int parse_compare(expr_ctx_t *ctx, lexer_t *l, value_t *result) {
    token_type_t op;
    value_t right;

    if (parse_add(ctx, l, result) != 0) return -1;

    op = lex_peek(l);
    if (op == TOK_EQ || op == TOK_EXACT_EQ || op == TOK_NE || op == TOK_LT || op == TOK_GT ||
        op == TOK_LE || op == TOK_GE || op == TOK_SUBSTR) {
        
        lex_next(l);
        if (parse_add(ctx, l, &right) != 0) return -1;

        /* $ = substring test */
        if (op == TOK_SUBSTR) {
            if (result->type != VAL_CHAR || right.type != VAL_CHAR) {
                ctx->error = "$ requires strings";
                return -1;
            }
            *result = val_logic(strstr(right.str, result->str) != NULL);
            return 0;
        }

        /* Comparison by type */
        {
            int cmp = 0;
            if (result->type == VAL_NUM && right.type == VAL_NUM) {
                double d = result->num - right.num;
                cmp = (d < 0) ? -1 : (d > 0) ? 1 : 0;
            } else if (result->type == VAL_CHAR && right.type == VAL_CHAR) {
                if (op == TOK_EXACT_EQ)
                    cmp = str_icmp(result->str, right.str);  /* always exact */
                else if (ctx->opts && !((set_options_t *)ctx->opts)->exact)
                    cmp = str_nicmp(result->str, right.str, strlen(right.str));
                else
                    cmp = str_icmp(result->str, right.str);
            } else if (result->type == VAL_DATE && right.type == VAL_DATE) {
                int32_t d = result->date - right.date;
                cmp = (d < 0) ? -1 : (d > 0) ? 1 : 0;
            } else if (result->type == VAL_LOGIC && right.type == VAL_LOGIC) {
                cmp = result->logic - right.logic;
            } else {
                ctx->error = "Type mismatch in comparison";
                return -1;
            }

            if (op == TOK_EQ || op == TOK_EXACT_EQ) *result = val_logic(cmp == 0);
            else if (op == TOK_NE) *result = val_logic(cmp != 0);
            else if (op == TOK_LE) *result = val_logic(cmp <= 0);
            else if (op == TOK_GE) *result = val_logic(cmp >= 0);
            else if (op == TOK_LT) *result = val_logic(cmp < 0);
            else if (op == TOK_GT) *result = val_logic(cmp > 0);
        }
        return 0;
    }

    return 0;
}

/* add_expr → mul_expr [ (+ | -) mul_expr ]* */
/* The arithmetic is ast.c's (ast_eval_binop), so the two evaluators agree,
 * numeric decimals and widths included. */
static int apply_binop(expr_ctx_t *ctx, ast_op_t op, value_t *result, const value_t *right) {
    value_t r;
    if (ast_eval_binop(ctx, op, result, right, &r) != 0) return -1;
    *result = r;
    return 0;
}

static int parse_add(expr_ctx_t *ctx, lexer_t *l, value_t *result) {
    if (parse_mul(ctx, l, result) != 0) return -1;

    for (;;) {
        token_type_t op = lex_peek(l);
        value_t right;
        if (op != TOK_PLUS && op != TOK_MINUS) break;
        lex_next(l);
        if (parse_mul(ctx, l, &right) != 0) return -1;
        if (apply_binop(ctx, op == TOK_PLUS ? OP_ADD : OP_SUB, result, &right) != 0) return -1;
    }
    return 0;
}

/* mul_expr → power_expr [ (* | /) power_expr ]* */
static int parse_mul(expr_ctx_t *ctx, lexer_t *l, value_t *result) {
    if (parse_power(ctx, l, result) != 0) return -1;

    for (;;) {
        token_type_t op = lex_peek(l);
        value_t right;
        if (op != TOK_MUL && op != TOK_DIV && op != TOK_MOD) break;
        lex_next(l);
        if (parse_power(ctx, l, &right) != 0) return -1;
        if (apply_binop(ctx, op == TOK_MUL ? OP_MUL : op == TOK_DIV ? OP_DIV : OP_MOD,
                        result, &right) != 0) return -1;
    }
    return 0;
}

/* power_expr → unary [ (** | ^) unary ] */
static int parse_power(expr_ctx_t *ctx, lexer_t *l, value_t *result) {
    if (parse_unary(ctx, l, result) != 0) return -1;

    if (lex_peek(l) == TOK_POWER) {
        value_t right;
        lex_next(l);
        if (parse_unary(ctx, l, &right) != 0) return -1;
        return apply_binop(ctx, OP_POWER, result, &right);
    }
    return 0;
}

/* unary → [-] primary */
static int parse_unary(expr_ctx_t *ctx, lexer_t *l, value_t *result) {
    token_type_t op = lex_peek(l);

    if (op == TOK_MINUS) {
        lex_next(l);
        if (parse_primary(ctx, l, result) != 0) return -1;
        if (result->type != VAL_NUM) {
            ctx->error = "Unary minus requires number";
            return -1;
        }
        result->num = -result->num;   /* decimals and width kept */
        return 0;
    }

    if (op == TOK_PLUS) {
        lex_next(l);
    }

    return parse_primary(ctx, l, result);
}

/* primary → number | string | .T./.F. | {date} | (expr) | func(args) | field_ref */
static int parse_primary(expr_ctx_t *ctx, lexer_t *l, value_t *result) {
    token_t t = l->current;

    switch (t.type) {
    case TOK_NUMBER:
        *result = val_num_literal(t.num_val, t.text);
        lex_next(l);
        return 0;

    case TOK_STRING:
        *result = val_str(t.text);
        lex_next(l);
        return 0;

    case TOK_LOGIC:
        *result = val_logic(t.logic_val);
        lex_next(l);
        return 0;

    case TOK_DATE:
        *result = val_date(t.date_val);
        lex_next(l);
        return 0;

    case TOK_MACRO:
        ctx->error = "Expected identifier after &";
        return -1;

    case TOK_LPAREN:
        lex_next(l);
        if (parse_expr(ctx, l, result) != 0) return -1;
        if (lex_peek(l) != TOK_RPAREN) {
            ctx->error = "Missing closing parenthesis";
            return -1;
        }
        lex_next(l);
        return 0;

    case TOK_IDENT: {
        char name[64];
        str_copy(name, t.text, sizeof(name));
        lex_next(l);

        /* Check for array indexed access: name[i] or name(i) or name[i,j] */
        int is_array = 0;
        if (ctx->vars) {
            value_t mv;
            if (memvar_find(ctx->vars, name, &mv) == 0 && mv.type == VAL_ARRAY)
                is_array = 1;
        }

        if ((is_array && (l->current.type == TOK_LPAREN || l->current.type == TOK_LBRACKET)) ||
            (l->current.type == TOK_LBRACKET)) {
            token_type_t end_type = (l->current.type == TOK_LPAREN) ? TOK_RPAREN : TOK_RBRACKET;
            const char *next_p = l->token_start + 1; /* skip [ or ( */
            value_t row_v, col_v;
            int row = 0, col = -1;   /* col -1: no second index */

            if (expr_eval(ctx, &next_p, &row_v) != 0) return -1;
            if (row_v.type != VAL_NUM) { ctx->error = "Array index must be numeric"; return -1; }
            row = (int)row_v.num;
            lexer_init_ext(l, next_p, ctx->vars);

            if (l->current.type == TOK_COMMA) {
                next_p = l->token_start + 1; /* skip , */
                if (expr_eval(ctx, &next_p, &col_v) != 0) return -1;
                if (col_v.type != VAL_NUM) { ctx->error = "Array index must be numeric"; return -1; }
                col = (int)col_v.num;
                lexer_init_ext(l, next_p, ctx->vars);
            }

            if (l->current.type == end_type) {
                lex_next(l);
            }

            int res = memvar_get_elem(ctx->vars, name, row, col, result);
            if (res == 0) return 0;
            if (res == -2) { ctx->error = "Array index out of bounds"; return -1; }
            ctx->error = "Variable is not an array";
            return -1;
        }

        /* Check for alias->field reference */
        if (lex_peek(l) == TOK_ARROW) {
            lex_next(l);
            if (lex_peek(l) != TOK_IDENT) {
                ctx->error = "Expected field name after ->";
                return -1;
            }
            char field_name[64];
            str_copy(field_name, l->current.text, sizeof(field_name));
            lex_next(l);

            if (ctx->area_lookup && field_name[0]) {
                dbf_t *adb = ctx->area_lookup(name);
                if (adb && dbf_is_open(adb) && adb->current_record != 0) {
                    int idx = dbf_find_field(adb, field_name);
                    if (idx >= 0) {
                        char raw[256];
                        dbf_get_field_raw(adb, idx, raw, sizeof(raw));
                        switch (adb->fields[idx].type) {
                        case 'C': *result = val_str(raw); return 0;
                        case 'F':
                        case 'N': *result = val_num(atof(raw)); return 0;
                        case 'D': *result = val_date(date_from_dbf(raw)); return 0;
                        case 'L': *result = val_logic(raw[0] == 'T' || raw[0] == 't'); return 0;
                        case 'M': {
                            int blk = atoi(raw);
                            char memo[256];
                            if (blk > 0 && dbf_memo_read(adb, blk, memo, sizeof(memo)) == 0)
                                *result = val_str(memo);
                            else
                                *result = val_str("");
                            return 0;
                        }
                        }
                    }
                }
            }
            {
                snprintf(ctx->err_msg, sizeof(ctx->err_msg), "Cannot resolve %s->%s", name, field_name);
                ctx->error = ctx->err_msg;
            }
            return -1;
        }

        /* Function call: name( */
        if (lex_peek(l) == TOK_LPAREN) {
            value_t *args = NULL;
            int nargs = 0;
            int rc;

            args = (value_t *)malloc(MAX_FUNC_ARGS * sizeof(value_t));
            if (!args) {
                ctx->error = "Out of memory";
                return -1;
            }
            memset(args, 0, MAX_FUNC_ARGS * sizeof(value_t));
            gc_push_roots(args, MAX_FUNC_ARGS);   /* popped with every free(args) below */
            lex_next(l); /* skip ( */

            if (lex_peek(l) != TOK_RPAREN) {
                for (;;) {
                    if (nargs >= MAX_FUNC_ARGS) {
                        gc_pop_roots(); free(args);
                        ctx->error = "Too many function arguments";
                        return -1;
                    }
                    if (parse_expr(ctx, l, &args[nargs]) != 0) {
                        gc_pop_roots(); free(args);
                        return -1;
                    }
                    nargs++;
                    if (lex_peek(l) == TOK_COMMA) {
                        lex_next(l);
                        continue;
                    }
                    break;
                }
            }

            if (lex_peek(l) != TOK_RPAREN) {
                gc_pop_roots(); free(args);
                ctx->error = "Missing ) in function call";
                return -1;
            }
            lex_next(l);

            rc = func_call(ctx, name, args, nargs, result);
            gc_pop_roots(); free(args);
            return rc;
        }

        /* Field reference */
        if (ctx->db && dbf_is_open(ctx->db)) {
            int idx = dbf_find_field(ctx->db, name);
            if (idx >= 0) {
                char raw[256];
                if (ctx->db->current_record != 0 &&
                    dbf_get_field_raw(ctx->db, idx, raw, sizeof(raw)) == 0) {
                    switch (ctx->db->fields[idx].type) {
                    case 'C': *result = val_str(raw); return 0;
                    case 'F':
                    case 'N': *result = val_num(atof(raw)); return 0;
                    case 'D': *result = val_date(date_from_dbf(raw)); return 0;
                    case 'L': *result = val_logic(raw[0] == 'T' || raw[0] == 't'); return 0;
                    case 'M': {
                        int blk = atoi(raw);
                        char memo[256];
                        if (blk > 0 && dbf_memo_read(ctx->db, blk, memo, sizeof(memo)) == 0)
                            *result = val_str(memo);
                        else
                            *result = val_str("");
                        return 0;
                    }
                    }
                } else {
                    /* No current record — return type-appropriate default.
                       This allows parse_clauses trial eval to find expression extent. */
                    switch (ctx->db->fields[idx].type) {
                    case 'C': case 'M': *result = val_str(""); return 0;
                    case 'F': case 'N': *result = val_num(0); return 0;
                    case 'D': *result = val_date(0); return 0;
                    case 'L': *result = val_logic(0); return 0;
                    }
                }
            }
        }

        /* Try memory variable lookup */
        if (ctx->vars) {
            value_t mv;
            if (memvar_find(ctx->vars, name, &mv) == 0) {
                *result = mv;
                return 0;
            }
        }

        /* Unknown identifier */
        {
            snprintf(ctx->err_msg, sizeof(ctx->err_msg), "Variable not found: %s", name);
            ctx->error = ctx->err_msg;
        }
        return -1;
    }

    default:
        ctx->error = "Unexpected token in expression";
        return -1;
    }
}

/* ---- Public API ---- */

int expr_eval(expr_ctx_t *ctx, const char **pp, value_t *result) {
    lexer_t l;
    lexer_init_ext(&l, *pp, ctx->vars);
    ctx->error = NULL;
    *result = val_nil();
    int res = parse_expr(ctx, &l, result);
    *pp = l.token_start;
    return res;
}

int expr_eval_str(expr_ctx_t *ctx, const char *str, value_t *result) {
    lexer_t l;
    lexer_init_ext(&l, str, ctx->vars);
    ctx->error = NULL;
    *result = val_nil();
    return parse_expr(ctx, &l, result);
}
