#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "expr.h"
#include "memvar.h"
#include "set.h"
#include "date.h"
#include "util.h"

/* Forward declarations for func.c */
int func_call(expr_ctx_t *ctx, const char *name, value_t *args, int nargs, value_t *result);

/* Forward declarations for recursive descent */
static int parse_or(expr_ctx_t *ctx, const char **pp, value_t *result);
static int parse_and(expr_ctx_t *ctx, const char **pp, value_t *result);
static int parse_not(expr_ctx_t *ctx, const char **pp, value_t *result);
static int parse_compare(expr_ctx_t *ctx, const char **pp, value_t *result);
static int parse_add(expr_ctx_t *ctx, const char **pp, value_t *result);
static int parse_mul(expr_ctx_t *ctx, const char **pp, value_t *result);
static int parse_unary(expr_ctx_t *ctx, const char **pp, value_t *result);
static int parse_primary(expr_ctx_t *ctx, const char **pp, value_t *result);

/* ---- Value constructors ---- */

value_t val_num(double n) {
    value_t v;
    v.type = VAL_NUM;
    v.num = n;
    return v;
}

value_t val_str(const char *s) {
    value_t v;
    v.type = VAL_CHAR;
    str_copy(v.str, s, sizeof(v.str));
    return v;
}

value_t val_logic(int b) {
    value_t v;
    v.type = VAL_LOGIC;
    v.logic = b ? 1 : 0;
    return v;
}

value_t val_date(int32_t jdn) {
    value_t v;
    v.type = VAL_DATE;
    v.date = jdn;
    return v;
}

value_t val_nil(void) {
    value_t v;
    v.type = VAL_NIL;
    v.num = 0;
    return v;
}

/* ---- Display formatting ---- */

void val_to_string(const value_t *v, char *buf, int size) {
    switch (v->type) {
    case VAL_NUM: {
        /* Print integer if no fractional part, else strip trailing zeros */
        double n = v->num;
        double intpart;
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
        str_copy(buf, v->str, size);
        break;
    case VAL_DATE: {
        char mdy[16];
        date_to_mdy(v->date, mdy);
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

/* ---- Helper: skip whitespace ---- */
static const char *skip(const char *p) {
    while (*p == ' ' || *p == '\t') p++;
    return p;
}

/* ---- Helper: check for .AND. / .OR. / .NOT. keywords ---- */
static int match_dot_keyword(const char *p, const char *kw) {
    /* Match .KW. case-insensitively */
    int len = strlen(kw);
    int i;
    if (*p != '.') return 0;
    p++;
    for (i = 0; i < len; i++) {
        char c = p[i];
        if (c >= 'a' && c <= 'z') c -= 32;
        if (c != kw[i]) return 0;
    }
    if (p[len] != '.') return 0;
    return len + 2; /* total chars consumed */
}

/* ---- Parser ---- */

/* expr → or_expr */
static int parse_expr(expr_ctx_t *ctx, const char **pp, value_t *result) {
    return parse_or(ctx, pp, result);
}

/* or_expr → and_expr [ .OR. and_expr ]* */
static int parse_or(expr_ctx_t *ctx, const char **pp, value_t *result) {
    const char *p;
    int n;

    if (parse_and(ctx, pp, result) != 0) return -1;

    for (;;) {
        p = skip(*pp);
        n = match_dot_keyword(p, "OR");
        if (n == 0) break;
        {
            value_t right;
            *pp = p + n;
            if (parse_and(ctx, pp, &right) != 0) return -1;
            if (result->type != VAL_LOGIC || right.type != VAL_LOGIC) {
                ctx->error = "Type mismatch in .OR.";
                return -1;
            }
            result->logic = result->logic || right.logic;
        }
    }
    return 0;
}

/* and_expr → not_expr [ .AND. not_expr ]* */
static int parse_and(expr_ctx_t *ctx, const char **pp, value_t *result) {
    const char *p;
    int n;

    if (parse_not(ctx, pp, result) != 0) return -1;

    for (;;) {
        p = skip(*pp);
        n = match_dot_keyword(p, "AND");
        if (n == 0) break;
        {
            value_t right;
            *pp = p + n;
            if (parse_not(ctx, pp, &right) != 0) return -1;
            if (result->type != VAL_LOGIC || right.type != VAL_LOGIC) {
                ctx->error = "Type mismatch in .AND.";
                return -1;
            }
            result->logic = result->logic && right.logic;
        }
    }
    return 0;
}

/* not_expr → [.NOT.] compare */
static int parse_not(expr_ctx_t *ctx, const char **pp, value_t *result) {
    const char *p = skip(*pp);
    int n = match_dot_keyword(p, "NOT");

    if (n > 0) {
        *pp = p + n;
        if (parse_compare(ctx, pp, result) != 0) return -1;
        if (result->type != VAL_LOGIC) {
            ctx->error = "Type mismatch in .NOT.";
            return -1;
        }
        result->logic = !result->logic;
        return 0;
    }
    return parse_compare(ctx, pp, result);
}

/* compare → add_expr [ (= | <> | # | < | > | <= | >= | $) add_expr ] */
static int parse_compare(expr_ctx_t *ctx, const char **pp, value_t *result) {
    const char *p;
    char op[3];
    value_t right;

    if (parse_add(ctx, pp, result) != 0) return -1;

    p = skip(*pp);
    op[0] = op[1] = op[2] = '\0';

    if (p[0] == '<' && p[1] == '>') { op[0] = '<'; op[1] = '>'; *pp = p + 2; }
    else if (p[0] == '<' && p[1] == '=') { op[0] = '<'; op[1] = '='; *pp = p + 2; }
    else if (p[0] == '>' && p[1] == '=') { op[0] = '>'; op[1] = '='; *pp = p + 2; }
    else if (p[0] == '<') { op[0] = '<'; *pp = p + 1; }
    else if (p[0] == '>') { op[0] = '>'; *pp = p + 1; }
    else if (p[0] == '=' && p[1] != '=') { op[0] = '='; *pp = p + 1; }
    else if (p[0] == '#') { op[0] = '#'; *pp = p + 1; }
    else if (p[0] == '$') { op[0] = '$'; *pp = p + 1; }
    else return 0; /* no comparison operator */

    if (parse_add(ctx, pp, &right) != 0) return -1;

    /* $ = substring test */
    if (op[0] == '$') {
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
            if (ctx->opts && !((set_options_t *)ctx->opts)->exact)
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

        if (op[0] == '=' && op[1] == '\0') *result = val_logic(cmp == 0);
        else if (op[0] == '<' && op[1] == '>') *result = val_logic(cmp != 0);
        else if (op[0] == '#') *result = val_logic(cmp != 0);
        else if (op[0] == '<' && op[1] == '=') *result = val_logic(cmp <= 0);
        else if (op[0] == '>' && op[1] == '=') *result = val_logic(cmp >= 0);
        else if (op[0] == '<') *result = val_logic(cmp < 0);
        else if (op[0] == '>') *result = val_logic(cmp > 0);
    }

    return 0;
}

/* add_expr → mul_expr [ (+ | -) mul_expr ]* */
static int parse_add(expr_ctx_t *ctx, const char **pp, value_t *result) {
    if (parse_mul(ctx, pp, result) != 0) return -1;

    for (;;) {
        const char *p = skip(*pp);
        char op = *p;

        /* Don't consume + or - if this is part of .AND./.OR./.NOT. or end of expression */
        if (op != '+' && op != '-') break;

        *pp = p + 1;

        {
            value_t right;
            if (parse_mul(ctx, pp, &right) != 0) return -1;

            if (op == '+') {
                if (result->type == VAL_NUM && right.type == VAL_NUM) {
                    result->num += right.num;
                } else if (result->type == VAL_CHAR && right.type == VAL_CHAR) {
                    /* String concatenation */
                    int len = strlen(result->str);
                    if (len + (int)strlen(right.str) < (int)sizeof(result->str))
                        strcat(result->str, right.str);
                } else if (result->type == VAL_DATE && right.type == VAL_NUM) {
                    result->date += (int32_t)right.num;
                } else if (result->type == VAL_NUM && right.type == VAL_DATE) {
                    *result = val_date(right.date + (int32_t)result->num);
                } else {
                    ctx->error = "Type mismatch in +";
                    return -1;
                }
            } else { /* '-' */
                if (result->type == VAL_NUM && right.type == VAL_NUM) {
                    result->num -= right.num;
                } else if (result->type == VAL_DATE && right.type == VAL_DATE) {
                    /* Date - Date = number of days */
                    *result = val_num((double)(result->date - right.date));
                } else if (result->type == VAL_DATE && right.type == VAL_NUM) {
                    result->date -= (int32_t)right.num;
                } else if (result->type == VAL_CHAR && right.type == VAL_CHAR) {
                    /* Trim-concatenate: trim trailing spaces from left, then concat */
                    int len = strlen(result->str);
                    while (len > 0 && result->str[len-1] == ' ') len--;
                    result->str[len] = '\0';
                    if (len + (int)strlen(right.str) < (int)sizeof(result->str))
                        strcat(result->str, right.str);
                } else {
                    ctx->error = "Type mismatch in -";
                    return -1;
                }
            }
        }
    }
    return 0;
}

/* mul_expr → unary [ (* | /) unary ]* */
static int parse_mul(expr_ctx_t *ctx, const char **pp, value_t *result) {
    if (parse_unary(ctx, pp, result) != 0) return -1;

    for (;;) {
        const char *p = skip(*pp);
        char op = *p;

        if (op != '*' && op != '/') break;
        *pp = p + 1;

        {
            value_t right;
            if (parse_unary(ctx, pp, &right) != 0) return -1;

            if (result->type != VAL_NUM || right.type != VAL_NUM) {
                ctx->error = "Type mismatch in * or /";
                return -1;
            }

            if (op == '*')
                result->num *= right.num;
            else {
                if (right.num == 0.0) {
                    ctx->error = "Division by zero";
                    return -1;
                }
                result->num /= right.num;
            }
        }
    }
    return 0;
}

/* unary → [-] primary */
static int parse_unary(expr_ctx_t *ctx, const char **pp, value_t *result) {
    const char *p = skip(*pp);

    if (*p == '-') {
        *pp = p + 1;
        if (parse_primary(ctx, pp, result) != 0) return -1;
        if (result->type != VAL_NUM) {
            ctx->error = "Unary minus requires number";
            return -1;
        }
        result->num = -result->num;
        return 0;
    }

    if (*p == '+') {
        *pp = p + 1;
    }

    return parse_primary(ctx, pp, result);
}

/* primary → number | string | .T./.F. | {date} | (expr) | func(args) | field_ref */
static int parse_primary(expr_ctx_t *ctx, const char **pp, value_t *result) {
    const char *p = skip(*pp);

    /* Number literal */
    if ((*p >= '0' && *p <= '9') || (*p == '.' && p[1] >= '0' && p[1] <= '9')) {
        char numbuf[64];
        int i = 0;
        while ((*p >= '0' && *p <= '9') || *p == '.') {
            if (i < (int)sizeof(numbuf) - 1) numbuf[i++] = *p;
            p++;
        }
        numbuf[i] = '\0';
        *result = val_num(atof(numbuf));
        *pp = p;
        return 0;
    }

    /* String literal */
    if (*p == '"' || *p == '\'') {
        char quote = *p++;
        int i = 0;
        char strbuf[256];
        while (*p && *p != quote && i < (int)sizeof(strbuf) - 1)
            strbuf[i++] = *p++;
        strbuf[i] = '\0';
        if (*p == quote) p++;
        *result = val_str(strbuf);
        *pp = p;
        return 0;
    }

    /* Logical literal .T. / .F. */
    if (*p == '.') {
        char c = p[1];
        if (c >= 'a' && c <= 'z') c -= 32;
        if ((c == 'T' || c == 'F') && p[2] == '.') {
            *result = val_logic(c == 'T');
            *pp = p + 3;
            return 0;
        }
        /* Could be .NOT. — fall through handled by parse_not caller */
    }

    /* Date literal {MM/DD/YY} */
    if (*p == '{') {
        const char *start = p;
        p++;
        while (*p && *p != '}') p++;
        {
            char datebuf[32];
            int len = (int)(p - start - 1);
            if (len > (int)sizeof(datebuf) - 1) len = (int)sizeof(datebuf) - 1;
            memcpy(datebuf, start + 1, len);
            datebuf[len] = '\0';
            if (*p == '}') p++;
            *result = val_date(date_from_mdy(datebuf));
        }
        *pp = p;
        return 0;
    }

    /* Parenthesized expression */
    if (*p == '(') {
        *pp = p + 1;
        if (parse_expr(ctx, pp, result) != 0) return -1;
        p = skip(*pp);
        if (*p != ')') {
            ctx->error = "Missing closing parenthesis";
            return -1;
        }
        *pp = p + 1;
        return 0;
    }

    /* Identifier: function call or field reference */
    if (is_ident_start(*p)) {
        char name[64];
        int i = 0;
        const char *after_name;

        while (is_ident_char(*p) && i < (int)sizeof(name) - 1)
            name[i++] = *p++;
        name[i] = '\0';
        after_name = p;

        p = skip(p);

        /* Function call: name( */
        if (*p == '(') {
            value_t args[8];
            int nargs = 0;
            p++; /* skip ( */
            p = skip(p);

            if (*p != ')') {
                for (;;) {
                    const char *q = p;
                    if (nargs >= 8) {
                        ctx->error = "Too many function arguments";
                        return -1;
                    }
                    *pp = q;
                    if (parse_expr(ctx, pp, &args[nargs]) != 0) return -1;
                    nargs++;
                    p = skip(*pp);
                    if (*p == ',') { p++; p = skip(p); continue; }
                    break;
                }
            }

            p = skip(p);
            if (*p != ')') {
                ctx->error = "Missing ) in function call";
                return -1;
            }
            p++;
            *pp = p;

            return func_call(ctx, name, args, nargs, result);
        }

        /* Field reference */
        *pp = after_name;
        if (ctx->db && dbf_is_open(ctx->db) && ctx->db->current_record != 0) {
            int idx = dbf_find_field(ctx->db, name);
            if (idx >= 0) {
                char raw[256];
                dbf_get_field_raw(ctx->db, idx, raw, sizeof(raw));
                switch (ctx->db->fields[idx].type) {
                case 'C': {
                    /* Preserve full field width including trailing spaces */
                    *result = val_str(raw);
                    return 0;
                }
                case 'N':
                    *result = val_num(atof(raw));
                    return 0;
                case 'D':
                    *result = val_date(date_from_dbf(raw));
                    return 0;
                case 'L':
                    *result = val_logic(raw[0] == 'T' || raw[0] == 't');
                    return 0;
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
            static char errbuf[80];
            snprintf(errbuf, sizeof(errbuf), "Variable not found: %s", name);
            ctx->error = errbuf;
        }
        return -1;
    }

    ctx->error = "Unexpected character in expression";
    return -1;
}

/* ---- Public API ---- */

int expr_eval(expr_ctx_t *ctx, const char **pp, value_t *result) {
    ctx->error = NULL;
    *result = val_nil();
    return parse_expr(ctx, pp, result);
}

int expr_eval_str(expr_ctx_t *ctx, const char *str, value_t *result) {
    const char *p = str;
    ctx->error = NULL;
    *result = val_nil();
    return parse_expr(ctx, &p, result);
}
