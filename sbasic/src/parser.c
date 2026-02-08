#include "parser.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* Forward declarations */
static expr_t *parse_expr(parser_t *p);
static stmt_t *parse_stmt(parser_t *p);

static void parser_error(parser_t *p, error_t err) {
    if (p->error == ERR_NONE) {
        p->error = err;
        p->error_line = lexer_peek(&p->lex)->line;
    }
}

/* Expect a specific token type, consume it; set error if not found */
static int expect(parser_t *p, token_type_t type) {
    if (lexer_check(&p->lex, type)) {
        lexer_next(&p->lex);
        return 1;
    }
    parser_error(p, ERR_UNEXPECTED_TOKEN);
    return 0;
}

/* Get variable type from identifier text */
static val_type_t var_type_from_name(const char *name) {
    int len = (int)strlen(name);
    if (len > 0) {
        char last = name[len - 1];
        if (last == '$') return VAL_STRING;
        if (last == '%') return VAL_INTEGER;
        if (last == '#') return VAL_DOUBLE;
    }
    return VAL_DOUBLE; /* bare name defaults to double */
}

/* Strip type suffix for internal name (keep suffix in stored name for uniqueness) */
static void var_base_name(const char *name, char *out, int outsize) {
    strncpy(out, name, outsize - 1);
    out[outsize - 1] = '\0';
}

/* --- Expression parsing (precedence climbing) --- */

/* Primary: literal, variable, function call, parenthesized expr */
static expr_t *parse_primary(parser_t *p) {
    token_t *tok = lexer_peek(&p->lex);

    if (tok->type == TOK_INTEGER_LIT) {
        token_t t = lexer_next(&p->lex);
        return expr_literal(val_integer(t.ival), t.line);
    }

    if (tok->type == TOK_DOUBLE_LIT) {
        token_t t = lexer_next(&p->lex);
        return expr_literal(val_double(t.dval), t.line);
    }

    if (tok->type == TOK_STRING_LIT) {
        token_t t = lexer_next(&p->lex);
        value_t v = val_string_cstr(t.text);
        expr_t *e = expr_literal(v, t.line);
        val_clear(&v);
        return e;
    }

    if (tok->type == TOK_IDENT) {
        token_t t = lexer_next(&p->lex);

        /* Check for function call: IDENT( */
        if (lexer_check(&p->lex, TOK_LPAREN)) {
            lexer_next(&p->lex); /* consume ( */
            expr_t *call = expr_call(t.text, t.line);
            if (!lexer_check(&p->lex, TOK_RPAREN)) {
                expr_t *arg = parse_expr(p);
                if (!arg) { expr_free(call); return NULL; }
                expr_call_add_arg(call, arg);
                while (lexer_match(&p->lex, TOK_COMMA)) {
                    arg = parse_expr(p);
                    if (!arg) { expr_free(call); return NULL; }
                    expr_call_add_arg(call, arg);
                }
            }
            if (!expect(p, TOK_RPAREN)) { expr_free(call); return NULL; }
            return call;
        }

        /* Variable reference */
        val_type_t vt = var_type_from_name(t.text);
        return expr_variable(t.text, vt, t.line);
    }

    if (tok->type == TOK_LPAREN) {
        lexer_next(&p->lex);
        expr_t *e = parse_expr(p);
        if (!e) return NULL;
        if (!expect(p, TOK_RPAREN)) { expr_free(e); return NULL; }
        return e;
    }

    parser_error(p, ERR_SYNTAX);
    return NULL;
}

/* Unary: NOT, unary minus */
static expr_t *parse_unary(parser_t *p) {
    if (lexer_check(&p->lex, TOK_MINUS)) {
        token_t t = lexer_next(&p->lex);
        expr_t *operand = parse_unary(p);
        if (!operand) return NULL;
        return expr_unary(OP_NEG, operand, t.line);
    }
    if (lexer_check(&p->lex, TOK_NOT)) {
        token_t t = lexer_next(&p->lex);
        expr_t *operand = parse_unary(p);
        if (!operand) return NULL;
        return expr_unary(OP_NOT, operand, t.line);
    }
    return parse_primary(p);
}

/* Power (right-associative) */
static expr_t *parse_power(parser_t *p) {
    expr_t *left = parse_unary(p);
    if (!left) return NULL;
    if (lexer_check(&p->lex, TOK_CARET)) {
        int line = lexer_peek(&p->lex)->line;
        lexer_next(&p->lex);
        expr_t *right = parse_power(p); /* right-associative */
        if (!right) { expr_free(left); return NULL; }
        return expr_binary(OP_POW, left, right, line);
    }
    return left;
}

/* Multiply, divide, integer divide, mod */
static expr_t *parse_mul(parser_t *p) {
    expr_t *left = parse_power(p);
    if (!left) return NULL;
    while (1) {
        binop_t op;
        if (lexer_check(&p->lex, TOK_STAR))      op = OP_MUL;
        else if (lexer_check(&p->lex, TOK_SLASH)) op = OP_DIV;
        else if (lexer_check(&p->lex, TOK_BACKSLASH)) op = OP_IDIV;
        else if (lexer_check(&p->lex, TOK_MOD))   op = OP_MOD;
        else break;
        int line = lexer_peek(&p->lex)->line;
        lexer_next(&p->lex);
        expr_t *right = parse_power(p);
        if (!right) { expr_free(left); return NULL; }
        left = expr_binary(op, left, right, line);
    }
    return left;
}

/* Add, subtract */
static expr_t *parse_add(parser_t *p) {
    expr_t *left = parse_mul(p);
    if (!left) return NULL;
    while (lexer_check(&p->lex, TOK_PLUS) || lexer_check(&p->lex, TOK_MINUS)) {
        binop_t op = lexer_check(&p->lex, TOK_PLUS) ? OP_ADD : OP_SUB;
        int line = lexer_peek(&p->lex)->line;
        lexer_next(&p->lex);
        expr_t *right = parse_mul(p);
        if (!right) { expr_free(left); return NULL; }
        left = expr_binary(op, left, right, line);
    }
    return left;
}

/* Comparison */
static expr_t *parse_comparison(parser_t *p) {
    expr_t *left = parse_add(p);
    if (!left) return NULL;

    cmpop_t op;
    int found = 0;
    if (lexer_check(&p->lex, TOK_EQ))      { op = CMP_EQ; found = 1; }
    else if (lexer_check(&p->lex, TOK_NE))  { op = CMP_NE; found = 1; }
    else if (lexer_check(&p->lex, TOK_LT))  { op = CMP_LT; found = 1; }
    else if (lexer_check(&p->lex, TOK_GT))  { op = CMP_GT; found = 1; }
    else if (lexer_check(&p->lex, TOK_LE))  { op = CMP_LE; found = 1; }
    else if (lexer_check(&p->lex, TOK_GE))  { op = CMP_GE; found = 1; }

    if (found) {
        int line = lexer_peek(&p->lex)->line;
        lexer_next(&p->lex);
        expr_t *right = parse_add(p);
        if (!right) { expr_free(left); return NULL; }
        left = expr_compare(op, left, right, line);
    }
    return left;
}

/* AND */
static expr_t *parse_and(parser_t *p) {
    expr_t *left = parse_comparison(p);
    if (!left) return NULL;
    while (lexer_check(&p->lex, TOK_AND)) {
        int line = lexer_peek(&p->lex)->line;
        lexer_next(&p->lex);
        expr_t *right = parse_comparison(p);
        if (!right) { expr_free(left); return NULL; }
        left = expr_binary(OP_AND, left, right, line);
    }
    return left;
}

/* OR (top-level expression) */
static expr_t *parse_expr(parser_t *p) {
    expr_t *left = parse_and(p);
    if (!left) return NULL;
    while (lexer_check(&p->lex, TOK_OR)) {
        int line = lexer_peek(&p->lex)->line;
        lexer_next(&p->lex);
        expr_t *right = parse_and(p);
        if (!right) { expr_free(left); return NULL; }
        left = expr_binary(OP_OR, left, right, line);
    }
    return left;
}

/* Skip EOLs */
static void skip_eol(parser_t *p) {
    while (lexer_check(&p->lex, TOK_EOL))
        lexer_next(&p->lex);
}

/* Check if at end of statement (EOL, EOF, colon, ELSE, etc.) */
static int at_stmt_end(parser_t *p) {
    token_type_t t = lexer_peek(&p->lex)->type;
    return t == TOK_EOL || t == TOK_EOF || t == TOK_COLON ||
           t == TOK_ELSE;
}

/* --- Statement parsing --- */

static stmt_t *parse_print(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume PRINT */

    stmt_t *s = stmt_print(line);

    /* PRINT with no args: just newline */
    if (at_stmt_end(p))
        return s;

    while (1) {
        expr_t *e = parse_expr(p);
        if (!e || p->error != ERR_NONE) {
            expr_free(e);
            stmt_free(s);
            return NULL;
        }

        /* Check for separator */
        char sep = '\0';
        if (lexer_check(&p->lex, TOK_SEMICOLON)) {
            lexer_next(&p->lex);
            sep = ';';
        } else if (lexer_check(&p->lex, TOK_COMMA)) {
            lexer_next(&p->lex);
            sep = ',';
        }

        stmt_print_add(s, e, sep);

        /* If no separator or at end of statement, done */
        if (sep == '\0' || at_stmt_end(p))
            break;
    }

    return s;
}

static stmt_t *parse_input(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume INPUT */

    char *prompt = NULL;

    /* Check for prompt string: INPUT "prompt"; var */
    if (lexer_check(&p->lex, TOK_STRING_LIT)) {
        token_t t = lexer_next(&p->lex);
        prompt = strdup(t.text);
        /* Expect ; or , after prompt */
        if (!lexer_match(&p->lex, TOK_SEMICOLON))
            lexer_match(&p->lex, TOK_COMMA);
    }

    stmt_t *s = stmt_input(prompt, line);
    free(prompt);

    /* Read variable list */
    if (!lexer_check(&p->lex, TOK_IDENT)) {
        parser_error(p, ERR_SYNTAX);
        stmt_free(s);
        return NULL;
    }

    token_t t = lexer_next(&p->lex);
    stmt_input_add_var(s, t.text, var_type_from_name(t.text));

    while (lexer_match(&p->lex, TOK_COMMA)) {
        if (!lexer_check(&p->lex, TOK_IDENT)) {
            parser_error(p, ERR_SYNTAX);
            stmt_free(s);
            return NULL;
        }
        t = lexer_next(&p->lex);
        stmt_input_add_var(s, t.text, var_type_from_name(t.text));
    }

    return s;
}

static stmt_t *parse_assign(parser_t *p, const char *name) {
    int line = lexer_peek(&p->lex)->line;
    val_type_t vt = var_type_from_name(name);

    /* Consume = */
    if (!expect(p, TOK_EQ)) return NULL;

    expr_t *val = parse_expr(p);
    if (!val) return NULL;

    return stmt_assign(name, vt, val, line);
}

/* Parse block of statements until END IF / ELSE / WEND / NEXT / END / EOF */
static stmt_t *parse_block(parser_t *p, int until_end_if, int until_else,
                           int until_wend, int until_next) {
    stmt_t *head = NULL;

    while (1) {
        skip_eol(p);

        token_type_t tt = lexer_peek(&p->lex)->type;

        /* Check for block terminators */
        if (tt == TOK_EOF) break;

        /* END IF check */
        if (tt == TOK_END && until_end_if) {
            /* Peek ahead to see if it's END IF */
            int save_pos = p->lex.pos;
            int save_line = p->lex.line;
            int save_has = p->lex.has_cur;
            token_t save_cur = p->lex.cur;

            lexer_next(&p->lex); /* consume END */
            if (lexer_check(&p->lex, TOK_IF)) {
                lexer_next(&p->lex); /* consume IF */
                break;
            }
            /* Not END IF - restore and parse as statement */
            p->lex.pos = save_pos;
            p->lex.line = save_line;
            p->lex.has_cur = save_has;
            p->lex.cur = save_cur;
        }

        if (tt == TOK_ELSE && until_else) break;
        if (tt == TOK_WEND && until_wend) break;
        if (tt == TOK_NEXT && until_next) break;

        /* Multi-statement line with : */
        if (lexer_check(&p->lex, TOK_COLON)) {
            lexer_next(&p->lex);
            continue;
        }

        stmt_t *s = parse_stmt(p);
        if (!s) {
            if (p->error != ERR_NONE) {
                stmt_free(head);
                return NULL;
            }
            break;
        }
        stmt_append(&head, s);
    }

    return head;
}

static stmt_t *parse_if(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume IF */

    expr_t *cond = parse_expr(p);
    if (!cond) return NULL;

    if (!expect(p, TOK_THEN)) {
        expr_free(cond);
        return NULL;
    }

    /* Check for single-line IF: IF cond THEN statement [ELSE statement] */
    if (!lexer_check(&p->lex, TOK_EOL) && !lexer_check(&p->lex, TOK_EOF)) {
        /* Single-line form */
        stmt_t *then_body = parse_stmt(p);
        stmt_t *else_body = NULL;

        if (lexer_check(&p->lex, TOK_ELSE)) {
            lexer_next(&p->lex);
            else_body = parse_stmt(p);
        }

        return stmt_if(cond, then_body, else_body, line);
    }

    /* Block IF form */
    stmt_t *then_body = parse_block(p, 1, 1, 0, 0);

    stmt_t *else_body = NULL;
    if (lexer_check(&p->lex, TOK_ELSE)) {
        lexer_next(&p->lex);
        else_body = parse_block(p, 1, 0, 0, 0);
    }

    /* END IF was consumed by parse_block */
    return stmt_if(cond, then_body, else_body, line);
}

static stmt_t *parse_for(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume FOR */

    if (!lexer_check(&p->lex, TOK_IDENT)) {
        parser_error(p, ERR_SYNTAX);
        return NULL;
    }
    token_t var = lexer_next(&p->lex);
    val_type_t vt = var_type_from_name(var.text);

    if (!expect(p, TOK_EQ)) return NULL;

    expr_t *start = parse_expr(p);
    if (!start) return NULL;

    if (!expect(p, TOK_TO)) { expr_free(start); return NULL; }

    expr_t *end = parse_expr(p);
    if (!end) { expr_free(start); return NULL; }

    expr_t *step = NULL;
    if (lexer_match(&p->lex, TOK_STEP)) {
        step = parse_expr(p);
        if (!step) { expr_free(start); expr_free(end); return NULL; }
    }

    /* Parse body until NEXT */
    stmt_t *body = parse_block(p, 0, 0, 0, 1);

    /* Consume NEXT */
    if (!lexer_check(&p->lex, TOK_NEXT)) {
        parser_error(p, ERR_FOR_WITHOUT_NEXT);
        expr_free(start); expr_free(end); expr_free(step);
        stmt_free(body);
        return NULL;
    }
    lexer_next(&p->lex); /* consume NEXT */

    /* Optional variable name after NEXT */
    if (lexer_check(&p->lex, TOK_IDENT))
        lexer_next(&p->lex);

    return stmt_for(var.text, vt, start, end, step, body, line);
}

static stmt_t *parse_while(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume WHILE */

    expr_t *cond = parse_expr(p);
    if (!cond) return NULL;

    stmt_t *body = parse_block(p, 0, 0, 1, 0);

    if (!lexer_check(&p->lex, TOK_WEND)) {
        parser_error(p, ERR_WHILE_WITHOUT_WEND);
        expr_free(cond);
        stmt_free(body);
        return NULL;
    }
    lexer_next(&p->lex); /* consume WEND */

    return stmt_while(cond, body, line);
}

static stmt_t *parse_stmt(parser_t *p) {
    if (p->error != ERR_NONE) return NULL;

    token_t *tok = lexer_peek(&p->lex);

    switch (tok->type) {
        case TOK_PRINT: return parse_print(p);
        case TOK_INPUT: return parse_input(p);
        case TOK_IF:    return parse_if(p);
        case TOK_FOR:   return parse_for(p);
        case TOK_WHILE: return parse_while(p);

        case TOK_LET: {
            lexer_next(&p->lex); /* consume LET */
            if (!lexer_check(&p->lex, TOK_IDENT)) {
                parser_error(p, ERR_SYNTAX);
                return NULL;
            }
            token_t var = lexer_next(&p->lex);
            return parse_assign(p, var.text);
        }

        case TOK_END: {
            lexer_next(&p->lex); /* consume END */
            return stmt_end(tok->line);
        }

        case TOK_REM:
        case TOK_EOL: {
            lexer_next(&p->lex);
            return stmt_alloc(STMT_REM, tok->line);
        }

        case TOK_IDENT: {
            /* Assignment: ident = expr */
            token_t var = lexer_next(&p->lex);
            if (lexer_check(&p->lex, TOK_EQ)) {
                return parse_assign(p, var.text);
            }
            /* Not an assignment — error */
            parser_error(p, ERR_SYNTAX);
            return NULL;
        }

        case TOK_DIM: {
            /* Placeholder for Stage 3 arrays — for now, just skip */
            parser_error(p, ERR_SYNTAX);
            return NULL;
        }

        default:
            if (tok->type == TOK_EOF)
                return NULL;
            parser_error(p, ERR_SYNTAX);
            return NULL;
    }
}

/* --- Public API --- */

void parser_init(parser_t *p, const char *src, int start_line) {
    lexer_init(&p->lex, src, start_line);
    p->error = ERR_NONE;
    p->error_line = 0;
}

stmt_t *parser_parse(parser_t *p) {
    stmt_t *head = NULL;

    while (!lexer_check(&p->lex, TOK_EOF)) {
        skip_eol(p);
        if (lexer_check(&p->lex, TOK_EOF)) break;

        stmt_t *s = parse_stmt(p);
        if (!s) {
            if (p->error != ERR_NONE) {
                stmt_free(head);
                return NULL;
            }
            continue;
        }
        stmt_append(&head, s);
    }

    return head;
}

stmt_t *parser_parse_line(parser_t *p) {
    skip_eol(p);
    if (lexer_check(&p->lex, TOK_EOF)) return NULL;
    return parse_stmt(p);
}

int parser_had_error(parser_t *p) {
    return p->error != ERR_NONE;
}
