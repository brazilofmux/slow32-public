#include "parser.h"
#include "fileio.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* Forward declarations */
static expr_t *parse_expr(parser_t *p);
static stmt_t *parse_stmt(parser_t *p);
static stmt_t *parse_print_file(parser_t *p);
static stmt_t *parse_input_file(parser_t *p);

static void parser_error(parser_t *p, error_t err) {
    if (p->error == ERR_NONE) {
        p->error = err;
        p->error_line = lexer_peek(&p->lex)->line;
    }
}

static int expect(parser_t *p, token_type_t type) {
    if (lexer_check(&p->lex, type)) {
        lexer_next(&p->lex);
        return 1;
    }
    parser_error(p, ERR_UNEXPECTED_TOKEN);
    return 0;
}

static val_type_t var_type_from_name(const char *name) {
    int len = (int)strlen(name);
    if (len > 0) {
        char last = name[len - 1];
        if (last == '$') return VAL_STRING;
        if (last == '%') return VAL_INTEGER;
        if (last == '#') return VAL_DOUBLE;
    }
    return VAL_DOUBLE;
}

/* --- Expression parsing (precedence climbing) --- */

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
        if (lexer_check(&p->lex, TOK_LPAREN)) {
            lexer_next(&p->lex);
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
        /* Check for field access: ident.field */
        if (lexer_check(&p->lex, TOK_DOT)) {
            lexer_next(&p->lex); /* consume . */
            if (!lexer_check(&p->lex, TOK_IDENT)) {
                parser_error(p, ERR_SYNTAX); return NULL;
            }
            token_t field = lexer_next(&p->lex);
            return expr_field_access(t.text, field.text, t.line);
        }
        return expr_variable(t.text, var_type_from_name(t.text), t.line);
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

static expr_t *parse_power(parser_t *p) {
    expr_t *left = parse_unary(p);
    if (!left) return NULL;
    if (lexer_check(&p->lex, TOK_CARET)) {
        int line = lexer_peek(&p->lex)->line;
        lexer_next(&p->lex);
        expr_t *right = parse_power(p);
        if (!right) { expr_free(left); return NULL; }
        return expr_binary(OP_POW, left, right, line);
    }
    return left;
}

static expr_t *parse_mul(parser_t *p) {
    expr_t *left = parse_power(p);
    if (!left) return NULL;
    while (1) {
        binop_t op;
        if (lexer_check(&p->lex, TOK_STAR))           op = OP_MUL;
        else if (lexer_check(&p->lex, TOK_SLASH))     op = OP_DIV;
        else if (lexer_check(&p->lex, TOK_BACKSLASH)) op = OP_IDIV;
        else if (lexer_check(&p->lex, TOK_MOD))       op = OP_MOD;
        else break;
        int line = lexer_peek(&p->lex)->line;
        lexer_next(&p->lex);
        expr_t *right = parse_power(p);
        if (!right) { expr_free(left); return NULL; }
        left = expr_binary(op, left, right, line);
    }
    return left;
}

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

/* --- Helpers --- */

static void skip_eol(parser_t *p) {
    while (lexer_check(&p->lex, TOK_EOL))
        lexer_next(&p->lex);
}

static int at_stmt_end(parser_t *p) {
    token_type_t t = lexer_peek(&p->lex)->type;
    return t == TOK_EOL || t == TOK_EOF || t == TOK_COLON ||
           t == TOK_ELSE;
}

/* Check if current token is END followed by a specific keyword */
static int check_end_keyword(parser_t *p, token_type_t kw) {
    if (!lexer_check(&p->lex, TOK_END)) return 0;
    int save_pos = p->lex.pos;
    int save_line = p->lex.line;
    int save_has = p->lex.has_cur;
    token_t save_cur = p->lex.cur;

    lexer_next(&p->lex);
    int result = lexer_check(&p->lex, kw);

    /* Restore */
    p->lex.pos = save_pos;
    p->lex.line = save_line;
    p->lex.has_cur = save_has;
    p->lex.cur = save_cur;
    return result;
}

/* Consume END <keyword> */
static void consume_end_keyword(parser_t *p, token_type_t kw) {
    lexer_next(&p->lex); /* END */
    lexer_next(&p->lex); /* keyword */
}

/* --- Block parsing --- */

/* Parse block of statements until a terminator is found.
   Terminators are checked by the caller; this function stops when:
   - EOF
   - END IF (if until_end_if)
   - ELSE (if until_else)
   - WEND (if until_wend)
   - NEXT (if until_next)
   - LOOP (if until_loop)
   - CASE / CASE ELSE / END SELECT (if until_case)
   - END SUB (if until_end_sub)
   - END FUNCTION (if until_end_func)
*/
#define BLK_END_IF   0x01
#define BLK_ELSE     0x02
#define BLK_WEND     0x04
#define BLK_NEXT     0x08
#define BLK_LOOP     0x10
#define BLK_CASE     0x20
#define BLK_END_SUB  0x40
#define BLK_END_FUNC 0x80
#define BLK_END_TYPE 0x100

static stmt_t *parse_block(parser_t *p, int flags) {
    stmt_t *head = NULL;
    stmt_t *tail = NULL;

    while (1) {
        skip_eol(p);
        token_type_t tt = lexer_peek(&p->lex)->type;
        if (tt == TOK_EOF) break;

        /* Check terminators */
        if ((flags & BLK_END_IF) && check_end_keyword(p, TOK_IF)) {
            consume_end_keyword(p, TOK_IF);
            break;
        }
        if ((flags & BLK_END_SUB) && check_end_keyword(p, TOK_SUB)) {
            consume_end_keyword(p, TOK_SUB);
            break;
        }
        if ((flags & BLK_END_FUNC) && check_end_keyword(p, TOK_FUNCTION)) {
            consume_end_keyword(p, TOK_FUNCTION);
            break;
        }
        if ((flags & BLK_END_IF) && check_end_keyword(p, TOK_SELECT)) {
            /* Don't consume — let the caller handle it */
            break;
        }
        if ((flags & BLK_ELSE) && tt == TOK_ELSE) break;
        if ((flags & BLK_WEND) && tt == TOK_WEND) break;
        if ((flags & BLK_NEXT) && tt == TOK_NEXT) break;
        if ((flags & BLK_LOOP) && tt == TOK_LOOP) break;
        if ((flags & BLK_CASE) && (tt == TOK_CASE || check_end_keyword(p, TOK_SELECT)))
            break;

        if (lexer_check(&p->lex, TOK_COLON)) {
            lexer_next(&p->lex);
            continue;
        }

        stmt_t *s = parse_stmt(p);
        if (!s) {
            if (p->error != ERR_NONE) { stmt_free(head); return NULL; }
            break;
        }
        stmt_append(&head, &tail, s);
    }
    return head;
}

/* --- Statement parsing --- */

static stmt_t *parse_print(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex);
    /* PRINT #n, ... → file output */
    if (lexer_check(&p->lex, TOK_HASH))
        return parse_print_file(p);
    stmt_t *s = stmt_print(line);
    if (at_stmt_end(p)) return s;
    /* PRINT USING "format"; expr; expr; ... */
    if (lexer_check(&p->lex, TOK_USING)) {
        lexer_next(&p->lex);
        if (!lexer_check(&p->lex, TOK_STRING_LIT)) {
            parser_error(p, ERR_SYNTAX); stmt_free(s); return NULL;
        }
        token_t fmt = lexer_next(&p->lex);
        s->print.using_fmt = strdup(fmt.text);
        if (!lexer_match(&p->lex, TOK_SEMICOLON)) {
            parser_error(p, ERR_SYNTAX); stmt_free(s); return NULL;
        }
    }
    while (1) {
        expr_t *e = parse_expr(p);
        if (!e || p->error != ERR_NONE) {
            expr_free(e); stmt_free(s); return NULL;
        }
        char sep = '\0';
        if (lexer_check(&p->lex, TOK_SEMICOLON)) {
            lexer_next(&p->lex); sep = ';';
        } else if (lexer_check(&p->lex, TOK_COMMA)) {
            lexer_next(&p->lex); sep = ',';
        }
        stmt_print_add(s, e, sep);
        if (sep == '\0' || at_stmt_end(p)) break;
    }
    return s;
}

static stmt_t *parse_input(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex);
    /* INPUT #n, var → file input */
    if (lexer_check(&p->lex, TOK_HASH))
        return parse_input_file(p);
    char *prompt = NULL;
    if (lexer_check(&p->lex, TOK_STRING_LIT)) {
        token_t t = lexer_next(&p->lex);
        prompt = strdup(t.text);
        if (!lexer_match(&p->lex, TOK_SEMICOLON))
            lexer_match(&p->lex, TOK_COMMA);
    }
    stmt_t *s = stmt_input(prompt, line);
    free(prompt);
    if (!lexer_check(&p->lex, TOK_IDENT)) {
        parser_error(p, ERR_SYNTAX); stmt_free(s); return NULL;
    }
    token_t t = lexer_next(&p->lex);
    stmt_input_add_var(s, t.text, var_type_from_name(t.text));
    while (lexer_match(&p->lex, TOK_COMMA)) {
        if (!lexer_check(&p->lex, TOK_IDENT)) {
            parser_error(p, ERR_SYNTAX); stmt_free(s); return NULL;
        }
        t = lexer_next(&p->lex);
        stmt_input_add_var(s, t.text, var_type_from_name(t.text));
    }
    return s;
}

static stmt_t *parse_assign(parser_t *p, const char *name) {
    int line = lexer_peek(&p->lex)->line;
    val_type_t vt = var_type_from_name(name);
    if (!expect(p, TOK_EQ)) return NULL;
    expr_t *val = parse_expr(p);
    if (!val) return NULL;
    return stmt_assign(name, vt, val, line);
}

static stmt_t *parse_if(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex);
    expr_t *cond = parse_expr(p);
    if (!cond) return NULL;
    if (!expect(p, TOK_THEN)) { expr_free(cond); return NULL; }

    /* Single-line form: not followed by EOL */
    if (!lexer_check(&p->lex, TOK_EOL) && !lexer_check(&p->lex, TOK_EOF)) {
        stmt_t *then_body = parse_stmt(p);
        stmt_t *else_body = NULL;
        if (lexer_check(&p->lex, TOK_ELSE)) {
            lexer_next(&p->lex);
            else_body = parse_stmt(p);
        }
        return stmt_if(cond, then_body, else_body, line);
    }

    /* Block form */
    stmt_t *then_body = parse_block(p, BLK_END_IF | BLK_ELSE);
    stmt_t *else_body = NULL;
    if (lexer_check(&p->lex, TOK_ELSE)) {
        lexer_next(&p->lex);
        else_body = parse_block(p, BLK_END_IF);
    }
    return stmt_if(cond, then_body, else_body, line);
}

static stmt_t *parse_for(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex);
    if (!lexer_check(&p->lex, TOK_IDENT)) {
        parser_error(p, ERR_SYNTAX); return NULL;
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
    stmt_t *body = parse_block(p, BLK_NEXT);
    if (!lexer_check(&p->lex, TOK_NEXT)) {
        parser_error(p, ERR_FOR_WITHOUT_NEXT);
        expr_free(start); expr_free(end); expr_free(step);
        stmt_free(body); return NULL;
    }
    lexer_next(&p->lex);
    if (lexer_check(&p->lex, TOK_IDENT))
        lexer_next(&p->lex);
    return stmt_for(var.text, vt, start, end, step, body, line);
}

static stmt_t *parse_while(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex);
    expr_t *cond = parse_expr(p);
    if (!cond) return NULL;
    stmt_t *body = parse_block(p, BLK_WEND);
    if (!lexer_check(&p->lex, TOK_WEND)) {
        parser_error(p, ERR_WHILE_WITHOUT_WEND);
        expr_free(cond); stmt_free(body); return NULL;
    }
    lexer_next(&p->lex);
    return stmt_while(cond, body, line);
}

static stmt_t *parse_do(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume DO */

    expr_t *pre_cond = NULL;
    int pre_until = 0;

    /* Check for DO WHILE/UNTIL expr */
    if (lexer_check(&p->lex, TOK_WHILE)) {
        lexer_next(&p->lex);
        pre_cond = parse_expr(p);
        if (!pre_cond) return NULL;
    } else if (lexer_check(&p->lex, TOK_UNTIL)) {
        lexer_next(&p->lex);
        pre_until = 1;
        pre_cond = parse_expr(p);
        if (!pre_cond) return NULL;
    }

    stmt_t *body = parse_block(p, BLK_LOOP);

    if (!lexer_check(&p->lex, TOK_LOOP)) {
        parser_error(p, ERR_DO_WITHOUT_LOOP);
        expr_free(pre_cond); stmt_free(body); return NULL;
    }
    lexer_next(&p->lex); /* consume LOOP */

    expr_t *post_cond = NULL;
    int post_until = 0;

    /* Check for LOOP WHILE/UNTIL expr */
    if (lexer_check(&p->lex, TOK_WHILE)) {
        lexer_next(&p->lex);
        post_cond = parse_expr(p);
        if (!post_cond) {
            expr_free(pre_cond); stmt_free(body); return NULL;
        }
    } else if (lexer_check(&p->lex, TOK_UNTIL)) {
        lexer_next(&p->lex);
        post_until = 1;
        post_cond = parse_expr(p);
        if (!post_cond) {
            expr_free(pre_cond); stmt_free(body); return NULL;
        }
    }

    return stmt_do_loop(pre_cond, pre_until, post_cond, post_until, body, line);
}

static stmt_t *parse_select(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume SELECT */
    if (!expect(p, TOK_CASE)) return NULL;

    expr_t *test = parse_expr(p);
    if (!test) return NULL;

    case_clause_t *clauses = NULL;
    case_clause_t **tail = &clauses;

    while (1) {
        skip_eol(p);
        if (lexer_check(&p->lex, TOK_EOF)) break;
        if (check_end_keyword(p, TOK_SELECT)) {
            consume_end_keyword(p, TOK_SELECT);
            break;
        }
        if (!lexer_check(&p->lex, TOK_CASE)) {
            parser_error(p, ERR_END_SELECT_EXPECTED);
            expr_free(test); case_clause_free(clauses); return NULL;
        }
        lexer_next(&p->lex); /* consume CASE */

        case_clause_t *cc = case_clause_alloc();

        /* CASE ELSE */
        if (lexer_check(&p->lex, TOK_ELSE)) {
            lexer_next(&p->lex);
            cc->is_else = 1;
            cc->body = parse_block(p, BLK_CASE);
            *tail = cc;
            tail = &cc->next;
            continue;
        }

        /* Parse match conditions (comma-separated) */
        while (1) {
            if (lexer_check(&p->lex, TOK_IS)) {
                /* CASE IS <op> expr */
                lexer_next(&p->lex);
                cmpop_t op;
                if (lexer_check(&p->lex, TOK_LT))      { op = CMP_LT; lexer_next(&p->lex); }
                else if (lexer_check(&p->lex, TOK_GT))  { op = CMP_GT; lexer_next(&p->lex); }
                else if (lexer_check(&p->lex, TOK_LE))  { op = CMP_LE; lexer_next(&p->lex); }
                else if (lexer_check(&p->lex, TOK_GE))  { op = CMP_GE; lexer_next(&p->lex); }
                else if (lexer_check(&p->lex, TOK_EQ))  { op = CMP_EQ; lexer_next(&p->lex); }
                else if (lexer_check(&p->lex, TOK_NE))  { op = CMP_NE; lexer_next(&p->lex); }
                else {
                    parser_error(p, ERR_SYNTAX);
                    case_clause_free(cc); expr_free(test);
                    case_clause_free(clauses); return NULL;
                }
                expr_t *val = parse_expr(p);
                if (!val) {
                    case_clause_free(cc); expr_free(test);
                    case_clause_free(clauses); return NULL;
                }
                case_clause_add_is(cc, op, val);
            } else {
                /* CASE expr [TO expr] */
                expr_t *val = parse_expr(p);
                if (!val) {
                    case_clause_free(cc); expr_free(test);
                    case_clause_free(clauses); return NULL;
                }
                if (lexer_match(&p->lex, TOK_TO)) {
                    expr_t *hi = parse_expr(p);
                    if (!hi) {
                        expr_free(val); case_clause_free(cc);
                        expr_free(test); case_clause_free(clauses);
                        return NULL;
                    }
                    case_clause_add_range(cc, val, hi);
                } else {
                    case_clause_add_value(cc, val);
                }
            }
            if (!lexer_match(&p->lex, TOK_COMMA)) break;
        }

        cc->body = parse_block(p, BLK_CASE);
        *tail = cc;
        tail = &cc->next;
    }

    return stmt_select(test, clauses, line);
}

static stmt_t *parse_exit(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume EXIT */

    exit_type_t what;
    if (lexer_check(&p->lex, TOK_FOR))           { what = EXIT_FOR; lexer_next(&p->lex); }
    else if (lexer_check(&p->lex, TOK_WHILE))    { what = EXIT_WHILE; lexer_next(&p->lex); }
    else if (lexer_check(&p->lex, TOK_DO))       { what = EXIT_DO; lexer_next(&p->lex); }
    else if (lexer_check(&p->lex, TOK_SUB))      { what = EXIT_SUB; lexer_next(&p->lex); }
    else if (lexer_check(&p->lex, TOK_FUNCTION)) { what = EXIT_FUNCTION; lexer_next(&p->lex); }
    else {
        parser_error(p, ERR_SYNTAX);
        return NULL;
    }
    return stmt_exit(what, line);
}

static stmt_t *parse_const(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume CONST */
    if (!lexer_check(&p->lex, TOK_IDENT)) {
        parser_error(p, ERR_SYNTAX); return NULL;
    }
    token_t name = lexer_next(&p->lex);
    if (!expect(p, TOK_EQ)) return NULL;
    expr_t *val = parse_expr(p);
    if (!val) return NULL;
    return stmt_const(name.text, var_type_from_name(name.text), val, line);
}

static stmt_t *parse_shared(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume SHARED */
    stmt_t *s = stmt_shared(line);
    if (!lexer_check(&p->lex, TOK_IDENT)) {
        parser_error(p, ERR_SYNTAX); stmt_free(s); return NULL;
    }
    token_t t = lexer_next(&p->lex);
    stmt_shared_add(s, t.text);
    while (lexer_match(&p->lex, TOK_COMMA)) {
        if (!lexer_check(&p->lex, TOK_IDENT)) {
            parser_error(p, ERR_SYNTAX); stmt_free(s); return NULL;
        }
        t = lexer_next(&p->lex);
        stmt_shared_add(s, t.text);
    }
    return s;
}

/* Parse SUB name(params) ... END SUB */
static stmt_t *parse_sub(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume SUB */
    if (!lexer_check(&p->lex, TOK_IDENT)) {
        parser_error(p, ERR_SYNTAX); return NULL;
    }
    token_t name = lexer_next(&p->lex);
    stmt_t *s = stmt_proc_def(STMT_SUB_DEF, name.text, line);

    /* Parameters */
    if (lexer_match(&p->lex, TOK_LPAREN)) {
        if (!lexer_check(&p->lex, TOK_RPAREN)) {
            token_t param = lexer_next(&p->lex);
            stmt_proc_add_param(s, param.text, var_type_from_name(param.text));
            while (lexer_match(&p->lex, TOK_COMMA)) {
                param = lexer_next(&p->lex);
                stmt_proc_add_param(s, param.text, var_type_from_name(param.text));
            }
        }
        expect(p, TOK_RPAREN);
    }

    stmt_t *body = parse_block(p, BLK_END_SUB);
    stmt_proc_set_body(s, body);
    return s;
}

/* Parse FUNCTION name(params) ... END FUNCTION */
static stmt_t *parse_function(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume FUNCTION */
    if (!lexer_check(&p->lex, TOK_IDENT)) {
        parser_error(p, ERR_SYNTAX); return NULL;
    }
    token_t name = lexer_next(&p->lex);
    stmt_t *s = stmt_proc_def(STMT_FUNC_DEF, name.text, line);
    s->proc_def.return_type = var_type_from_name(name.text);

    /* Parameters */
    if (lexer_match(&p->lex, TOK_LPAREN)) {
        if (!lexer_check(&p->lex, TOK_RPAREN)) {
            token_t param = lexer_next(&p->lex);
            stmt_proc_add_param(s, param.text, var_type_from_name(param.text));
            while (lexer_match(&p->lex, TOK_COMMA)) {
                param = lexer_next(&p->lex);
                stmt_proc_add_param(s, param.text, var_type_from_name(param.text));
            }
        }
        expect(p, TOK_RPAREN);
    }

    stmt_t *body = parse_block(p, BLK_END_FUNC);
    stmt_proc_set_body(s, body);
    return s;
}

/* Parse CALL sub(args) */
static stmt_t *parse_call(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume CALL */
    if (!lexer_check(&p->lex, TOK_IDENT)) {
        parser_error(p, ERR_SYNTAX); return NULL;
    }
    token_t name = lexer_next(&p->lex);
    stmt_t *s = stmt_call(name.text, line);
    if (lexer_match(&p->lex, TOK_LPAREN)) {
        if (!lexer_check(&p->lex, TOK_RPAREN)) {
            expr_t *arg = parse_expr(p);
            if (!arg) { stmt_free(s); return NULL; }
            stmt_call_add_arg(s, arg);
            while (lexer_match(&p->lex, TOK_COMMA)) {
                arg = parse_expr(p);
                if (!arg) { stmt_free(s); return NULL; }
                stmt_call_add_arg(s, arg);
            }
        }
        expect(p, TOK_RPAREN);
    }
    return s;
}

/* Parse DECLARE SUB/FUNCTION (forward declaration — parsed but ignored) */
static stmt_t *parse_declare(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume DECLARE */
    /* Skip SUB/FUNCTION name(params) */
    if (lexer_check(&p->lex, TOK_SUB) || lexer_check(&p->lex, TOK_FUNCTION))
        lexer_next(&p->lex);
    if (lexer_check(&p->lex, TOK_IDENT))
        lexer_next(&p->lex);
    if (lexer_match(&p->lex, TOK_LPAREN)) {
        /* Skip parameter list */
        int depth = 1;
        while (depth > 0 && !lexer_check(&p->lex, TOK_EOF)) {
            if (lexer_check(&p->lex, TOK_LPAREN)) depth++;
            if (lexer_check(&p->lex, TOK_RPAREN)) depth--;
            lexer_next(&p->lex);
        }
    }
    return stmt_alloc(STMT_DECLARE, line);
}

/* Parse implicit call: ident arg1, arg2, ... */
static stmt_t *parse_implicit_call(parser_t *p, const char *name, int line) {
    stmt_t *s = stmt_call(name, line);
    if (!at_stmt_end(p)) {
        expr_t *arg = parse_expr(p);
        if (!arg) { stmt_free(s); return NULL; }
        stmt_call_add_arg(s, arg);
        while (lexer_match(&p->lex, TOK_COMMA)) {
            arg = parse_expr(p);
            if (!arg) { stmt_free(s); return NULL; }
            stmt_call_add_arg(s, arg);
        }
    }
    return s;
}

/* --- Stage 3 parsing --- */

static stmt_t *parse_dim_or_redim(parser_t *p, int is_redim) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume DIM or REDIM */

    int preserve = 0;
    if (is_redim && lexer_check(&p->lex, TOK_IDENT)) {
        /* Check for PRESERVE (not a keyword, context-sensitive) */
        if (strcmp(lexer_peek(&p->lex)->text, "PRESERVE") == 0) {
            lexer_next(&p->lex);
            preserve = 1;
        }
    }

    if (!lexer_check(&p->lex, TOK_IDENT)) {
        parser_error(p, ERR_SYNTAX); return NULL;
    }
    token_t name = lexer_next(&p->lex);

    /* DIM var AS TypeName (no parens — user-defined type) */
    if (lexer_check(&p->lex, TOK_AS)) {
        lexer_next(&p->lex);
        if (!lexer_check(&p->lex, TOK_IDENT)) {
            parser_error(p, ERR_SYNTAX); return NULL;
        }
        token_t tname = lexer_next(&p->lex);
        /* Check if it's a primitive type name */
        if (strcmp(tname.text, "INTEGER") == 0 ||
            strcmp(tname.text, "DOUBLE") == 0 ||
            strcmp(tname.text, "STRING") == 0) {
            /* Scalar declaration with type — just ignore (var gets typed by suffix) */
            return stmt_alloc(STMT_REM, line);
        }
        return stmt_dim_as_type(name.text, tname.text, line);
    }

    if (!expect(p, TOK_LPAREN)) return NULL;

    expr_t *dims[8];
    int ndims = 0;
    dims[0] = parse_expr(p);
    if (!dims[0]) return NULL;
    ndims = 1;
    while (lexer_match(&p->lex, TOK_COMMA)) {
        if (ndims >= 8) {
            parser_error(p, ERR_SYNTAX);
            for (int i = 0; i < ndims; i++) expr_free(dims[i]);
            return NULL;
        }
        dims[ndims] = parse_expr(p);
        if (!dims[ndims]) {
            for (int i = 0; i < ndims; i++) expr_free(dims[i]);
            return NULL;
        }
        ndims++;
    }

    if (!expect(p, TOK_RPAREN)) {
        for (int i = 0; i < ndims; i++) expr_free(dims[i]);
        return NULL;
    }

    /* Default type from name suffix */
    val_type_t type = var_type_from_name(name.text);

    /* Optional AS INTEGER/DOUBLE/STRING */
    if (lexer_match(&p->lex, TOK_AS)) {
        if (!lexer_check(&p->lex, TOK_IDENT)) {
            parser_error(p, ERR_SYNTAX);
            for (int i = 0; i < ndims; i++) expr_free(dims[i]);
            return NULL;
        }
        token_t tn = lexer_next(&p->lex);
        if (strcmp(tn.text, "INTEGER") == 0) type = VAL_INTEGER;
        else if (strcmp(tn.text, "DOUBLE") == 0) type = VAL_DOUBLE;
        else if (strcmp(tn.text, "STRING") == 0) type = VAL_STRING;
        else {
            parser_error(p, ERR_SYNTAX);
            for (int i = 0; i < ndims; i++) expr_free(dims[i]);
            return NULL;
        }
    }

    return stmt_dim(name.text, type, dims, ndims, is_redim, preserve, line);
}

static stmt_t *parse_erase(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume ERASE */
    stmt_t *s = stmt_erase(line);
    if (!lexer_check(&p->lex, TOK_IDENT)) {
        parser_error(p, ERR_SYNTAX); stmt_free(s); return NULL;
    }
    token_t t = lexer_next(&p->lex);
    stmt_erase_add(s, t.text);
    while (lexer_match(&p->lex, TOK_COMMA)) {
        if (!lexer_check(&p->lex, TOK_IDENT)) {
            parser_error(p, ERR_SYNTAX); stmt_free(s); return NULL;
        }
        t = lexer_next(&p->lex);
        stmt_erase_add(s, t.text);
    }
    return s;
}

static stmt_t *parse_data(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume DATA */
    stmt_t *s = stmt_data(line);

    while (1) {
        if (lexer_check(&p->lex, TOK_STRING_LIT)) {
            token_t t = lexer_next(&p->lex);
            value_t v = val_string_cstr(t.text);
            stmt_data_add(s, v);
            val_clear(&v);
        } else if (lexer_check(&p->lex, TOK_MINUS)) {
            lexer_next(&p->lex);
            if (lexer_check(&p->lex, TOK_INTEGER_LIT)) {
                token_t t = lexer_next(&p->lex);
                value_t v = val_integer(-t.ival);
                stmt_data_add(s, v);
            } else if (lexer_check(&p->lex, TOK_DOUBLE_LIT)) {
                token_t t = lexer_next(&p->lex);
                value_t v = val_double(-t.dval);
                stmt_data_add(s, v);
            } else {
                parser_error(p, ERR_SYNTAX); stmt_free(s); return NULL;
            }
        } else if (lexer_check(&p->lex, TOK_INTEGER_LIT)) {
            token_t t = lexer_next(&p->lex);
            value_t v = val_integer(t.ival);
            stmt_data_add(s, v);
        } else if (lexer_check(&p->lex, TOK_DOUBLE_LIT)) {
            token_t t = lexer_next(&p->lex);
            value_t v = val_double(t.dval);
            stmt_data_add(s, v);
        } else {
            break;
        }
        if (!lexer_match(&p->lex, TOK_COMMA)) break;
    }
    return s;
}

static stmt_t *parse_read(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume READ */
    stmt_t *s = stmt_read(line);
    if (!lexer_check(&p->lex, TOK_IDENT)) {
        parser_error(p, ERR_SYNTAX); stmt_free(s); return NULL;
    }
    token_t t = lexer_next(&p->lex);
    stmt_read_add_var(s, t.text, var_type_from_name(t.text));
    while (lexer_match(&p->lex, TOK_COMMA)) {
        if (!lexer_check(&p->lex, TOK_IDENT)) {
            parser_error(p, ERR_SYNTAX); stmt_free(s); return NULL;
        }
        t = lexer_next(&p->lex);
        stmt_read_add_var(s, t.text, var_type_from_name(t.text));
    }
    return s;
}

static stmt_t *parse_restore(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume RESTORE */
    char label[64] = "";
    if (lexer_check(&p->lex, TOK_IDENT)) {
        token_t t = lexer_next(&p->lex);
        strncpy(label, t.text, 63);
    }
    return stmt_restore(label, line);
}

/* --- Stage 5: File I/O parsing --- */

/* Helper: parse #n file handle expression */
static expr_t *parse_file_handle(parser_t *p) {
    if (!lexer_match(&p->lex, TOK_HASH)) {
        parser_error(p, ERR_SYNTAX); return NULL;
    }
    return parse_expr(p);
}

/* OPEN file$ FOR INPUT|OUTPUT|APPEND AS #n */
static stmt_t *parse_open(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume OPEN */
    expr_t *filename = parse_expr(p);
    if (!filename) return NULL;
    if (!lexer_match(&p->lex, TOK_FOR)) {
        parser_error(p, ERR_SYNTAX); expr_free(filename); return NULL;
    }
    int mode;
    if (lexer_check(&p->lex, TOK_INPUT)) {
        mode = FMODE_INPUT; lexer_next(&p->lex);
    } else if (lexer_check(&p->lex, TOK_OUTPUT)) {
        mode = FMODE_OUTPUT; lexer_next(&p->lex);
    } else if (lexer_check(&p->lex, TOK_APPEND)) {
        mode = FMODE_APPEND; lexer_next(&p->lex);
    } else {
        parser_error(p, ERR_SYNTAX); expr_free(filename); return NULL;
    }
    if (!lexer_match(&p->lex, TOK_AS)) {
        parser_error(p, ERR_SYNTAX); expr_free(filename); return NULL;
    }
    expr_t *handle = parse_file_handle(p);
    if (!handle) { expr_free(filename); return NULL; }
    return stmt_open(filename, mode, handle, line);
}

/* CLOSE [#n [, #n ...]] */
static stmt_t *parse_close(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume CLOSE */
    if (at_stmt_end(p))
        return stmt_close(NULL, line); /* close all */
    expr_t *handle = parse_file_handle(p);
    return stmt_close(handle, line);
}

/* PRINT #n, items... */
static stmt_t *parse_print_file(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    /* PRINT already consumed by caller, # already peeked */
    expr_t *handle = parse_file_handle(p);
    if (!handle) return NULL;
    lexer_match(&p->lex, TOK_COMMA); /* consume separator after #n */
    stmt_t *s = stmt_print_file(handle, line);
    while (!at_stmt_end(p)) {
        expr_t *e = parse_expr(p);
        if (!e || p->error != ERR_NONE) {
            expr_free(e); stmt_free(s); return NULL;
        }
        char sep = '\0';
        if (lexer_check(&p->lex, TOK_SEMICOLON)) { lexer_next(&p->lex); sep = ';'; }
        else if (lexer_check(&p->lex, TOK_COMMA)) { lexer_next(&p->lex); sep = ','; }
        stmt_print_file_add(s, e, sep);
        if (sep == '\0' || at_stmt_end(p)) break;
    }
    return s;
}

/* WRITE #n, items... */
static stmt_t *parse_write_file(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume WRITE */
    expr_t *handle = parse_file_handle(p);
    if (!handle) return NULL;
    lexer_match(&p->lex, TOK_COMMA);
    stmt_t *s = stmt_write_file(handle, line);
    while (!at_stmt_end(p)) {
        expr_t *e = parse_expr(p);
        if (!e || p->error != ERR_NONE) {
            expr_free(e); stmt_free(s); return NULL;
        }
        char sep = ',';
        if (lexer_check(&p->lex, TOK_COMMA)) lexer_next(&p->lex);
        else sep = '\0';
        stmt_write_file_add(s, e, sep);
        if (sep == '\0' || at_stmt_end(p)) break;
    }
    return s;
}

/* INPUT #n, var, var$, ... */
static stmt_t *parse_input_file(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    /* INPUT already consumed by caller, # already peeked */
    expr_t *handle = parse_file_handle(p);
    if (!handle) return NULL;
    lexer_match(&p->lex, TOK_COMMA);
    stmt_t *s = stmt_input_file(handle, line);
    if (!lexer_check(&p->lex, TOK_IDENT)) {
        parser_error(p, ERR_SYNTAX); stmt_free(s); return NULL;
    }
    token_t t = lexer_next(&p->lex);
    stmt_input_file_add_var(s, t.text, var_type_from_name(t.text));
    while (lexer_match(&p->lex, TOK_COMMA)) {
        if (!lexer_check(&p->lex, TOK_IDENT)) {
            parser_error(p, ERR_SYNTAX); stmt_free(s); return NULL;
        }
        t = lexer_next(&p->lex);
        stmt_input_file_add_var(s, t.text, var_type_from_name(t.text));
    }
    return s;
}

/* LINE INPUT #n, var$ */
static stmt_t *parse_line_input(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume LINE */
    if (!lexer_match(&p->lex, TOK_INPUT)) {
        parser_error(p, ERR_SYNTAX); return NULL;
    }
    expr_t *handle = parse_file_handle(p);
    if (!handle) return NULL;
    lexer_match(&p->lex, TOK_COMMA);
    stmt_t *s = stmt_line_input(handle, line);
    if (!lexer_check(&p->lex, TOK_IDENT)) {
        parser_error(p, ERR_SYNTAX); stmt_free(s); return NULL;
    }
    token_t t = lexer_next(&p->lex);
    stmt_input_file_add_var(s, t.text, var_type_from_name(t.text));
    return s;
}

/* KILL file$ */
static stmt_t *parse_kill(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume KILL */
    expr_t *filename = parse_expr(p);
    if (!filename) return NULL;
    return stmt_kill(filename, line);
}

/* NAME old$ AS new$ */
static stmt_t *parse_name_stmt(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume NAME */
    expr_t *oldname = parse_expr(p);
    if (!oldname) return NULL;
    if (!lexer_match(&p->lex, TOK_AS)) {
        parser_error(p, ERR_SYNTAX); expr_free(oldname); return NULL;
    }
    expr_t *newname = parse_expr(p);
    if (!newname) { expr_free(oldname); return NULL; }
    return stmt_name(oldname, newname, line);
}

/* --- Stage 6: Line continuation support --- */

/* After scanning a token, check if the previous line ended with _
   We handle this in skip_eol: if the line before EOL ends with _,
   we treat it as line continuation and skip the EOL. */

/* --- Stage 6 parsing --- */

/* TYPE name ... END TYPE */
static stmt_t *parse_type_def(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume TYPE */
    if (!lexer_check(&p->lex, TOK_IDENT)) {
        parser_error(p, ERR_SYNTAX); return NULL;
    }
    token_t name = lexer_next(&p->lex);
    stmt_t *s = stmt_type_def(name.text, line);

    while (1) {
        skip_eol(p);
        if (lexer_check(&p->lex, TOK_EOF)) break;
        if (check_end_keyword(p, TOK_TYPE)) {
            consume_end_keyword(p, TOK_TYPE);
            break;
        }
        /* field AS type */
        if (!lexer_check(&p->lex, TOK_IDENT)) {
            parser_error(p, ERR_SYNTAX); stmt_free(s); return NULL;
        }
        token_t fname = lexer_next(&p->lex);
        if (!lexer_match(&p->lex, TOK_AS)) {
            parser_error(p, ERR_SYNTAX); stmt_free(s); return NULL;
        }
        if (!lexer_check(&p->lex, TOK_IDENT)) {
            parser_error(p, ERR_SYNTAX); stmt_free(s); return NULL;
        }
        token_t tname = lexer_next(&p->lex);
        val_type_t ft;
        if (strcmp(tname.text, "INTEGER") == 0) ft = VAL_INTEGER;
        else if (strcmp(tname.text, "DOUBLE") == 0) ft = VAL_DOUBLE;
        else if (strcmp(tname.text, "STRING") == 0) ft = VAL_STRING;
        else {
            parser_error(p, ERR_SYNTAX); stmt_free(s); return NULL;
        }
        stmt_type_def_add_field(s, fname.text, ft);
    }
    return s;
}

/* ON expr GOTO/GOSUB label1, label2, ... */
/* ON ERROR GOTO label / ON ERROR GOTO 0 */
static stmt_t *parse_on(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume ON */

    /* ON ERROR GOTO label */
    if (lexer_check(&p->lex, TOK_ERROR)) {
        lexer_next(&p->lex);
        if (!lexer_match(&p->lex, TOK_GOTO)) {
            parser_error(p, ERR_SYNTAX); return NULL;
        }
        /* ON ERROR GOTO 0 = disable error handling */
        if (lexer_check(&p->lex, TOK_INTEGER_LIT) &&
            lexer_peek(&p->lex)->ival == 0) {
            lexer_next(&p->lex);
            return stmt_on_error("", line);
        }
        if (!lexer_check(&p->lex, TOK_IDENT)) {
            parser_error(p, ERR_SYNTAX); return NULL;
        }
        token_t label = lexer_next(&p->lex);
        return stmt_on_error(label.text, line);
    }

    /* ON expr GOTO/GOSUB label1, label2, ... */
    expr_t *index = parse_expr(p);
    if (!index) return NULL;

    int is_gosub = 0;
    if (lexer_check(&p->lex, TOK_GOTO)) {
        lexer_next(&p->lex);
    } else if (lexer_check(&p->lex, TOK_GOSUB)) {
        lexer_next(&p->lex);
        is_gosub = 1;
    } else {
        parser_error(p, ERR_SYNTAX); expr_free(index); return NULL;
    }

    stmt_t *s = is_gosub ? stmt_on_gosub(index, line) : stmt_on_goto(index, line);

    if (!lexer_check(&p->lex, TOK_IDENT)) {
        parser_error(p, ERR_SYNTAX); stmt_free(s); return NULL;
    }
    token_t label = lexer_next(&p->lex);
    stmt_on_branch_add_label(s, label.text);
    while (lexer_match(&p->lex, TOK_COMMA)) {
        if (!lexer_check(&p->lex, TOK_IDENT)) {
            parser_error(p, ERR_SYNTAX); stmt_free(s); return NULL;
        }
        label = lexer_next(&p->lex);
        stmt_on_branch_add_label(s, label.text);
    }
    return s;
}

/* RESUME [NEXT] */
static stmt_t *parse_resume(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume RESUME */
    int resume_next = 0;
    if (lexer_check(&p->lex, TOK_NEXT)) {
        lexer_next(&p->lex);
        resume_next = 1;
    }
    return stmt_resume(resume_next, line);
}

/* ERROR n */
static stmt_t *parse_error_stmt(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume ERROR */
    expr_t *errnum = parse_expr(p);
    if (!errnum) return NULL;
    return stmt_error_raise(errnum, line);
}

/* DEFINT/DEFDBL/DEFSTR A-Z */
static stmt_t *parse_deftype(parser_t *p, val_type_t type) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex); /* consume DEFINT/DEFDBL/DEFSTR */
    if (!lexer_check(&p->lex, TOK_IDENT)) {
        parser_error(p, ERR_SYNTAX); return NULL;
    }
    token_t from_tok = lexer_next(&p->lex);
    char from = from_tok.text[0];
    char to = from;
    if (lexer_match(&p->lex, TOK_MINUS)) {
        if (!lexer_check(&p->lex, TOK_IDENT)) {
            parser_error(p, ERR_SYNTAX); return NULL;
        }
        token_t to_tok = lexer_next(&p->lex);
        to = to_tok.text[0];
    }
    return stmt_deftype(type, from, to, line);
}

/* Handle ident(...) which could be array assignment or call */
static stmt_t *parse_paren_dispatch(parser_t *p, const char *name, int line) {
    lexer_next(&p->lex); /* consume ( */

    expr_t *args[16];
    int nargs = 0;

    if (!lexer_check(&p->lex, TOK_RPAREN)) {
        args[0] = parse_expr(p);
        if (!args[0]) return NULL;
        nargs = 1;
        while (lexer_match(&p->lex, TOK_COMMA)) {
            if (nargs >= 16) {
                parser_error(p, ERR_SYNTAX);
                for (int i = 0; i < nargs; i++) expr_free(args[i]);
                return NULL;
            }
            args[nargs] = parse_expr(p);
            if (!args[nargs]) {
                for (int i = 0; i < nargs; i++) expr_free(args[i]);
                return NULL;
            }
            nargs++;
        }
    }

    if (!expect(p, TOK_RPAREN)) {
        for (int i = 0; i < nargs; i++) expr_free(args[i]);
        return NULL;
    }

    /* Array assignment: ident(indices) = expr */
    if (lexer_check(&p->lex, TOK_EQ)) {
        lexer_next(&p->lex);
        expr_t *value = parse_expr(p);
        if (!value) {
            for (int i = 0; i < nargs; i++) expr_free(args[i]);
            return NULL;
        }
        return stmt_array_assign(name, var_type_from_name(name),
                                 args, nargs, value, line);
    }

    /* Otherwise it's a call with parens */
    stmt_t *s = stmt_call(name, line);
    for (int i = 0; i < nargs; i++)
        stmt_call_add_arg(s, args[i]);
    return s;
}

static stmt_t *parse_option(parser_t *p, int line) {
    /* Already consumed "OPTION" as an ident. Expect BASE 0/1 */
    if (!lexer_check(&p->lex, TOK_IDENT) ||
        strcmp(lexer_peek(&p->lex)->text, "BASE") != 0) {
        parser_error(p, ERR_SYNTAX); return NULL;
    }
    lexer_next(&p->lex); /* consume BASE */
    if (!lexer_check(&p->lex, TOK_INTEGER_LIT)) {
        parser_error(p, ERR_SYNTAX); return NULL;
    }
    token_t t = lexer_next(&p->lex);
    if (t.ival != 0 && t.ival != 1) {
        parser_error(p, ERR_ILLEGAL_FUNCTION_CALL); return NULL;
    }
    return stmt_option_base(t.ival, line);
}

/* --- Main statement dispatch --- */

static stmt_t *parse_stmt(parser_t *p) {
    if (p->error != ERR_NONE) return NULL;
    token_t *tok = lexer_peek(&p->lex);

    switch (tok->type) {
        case TOK_PRINT:    return parse_print(p);
        case TOK_INPUT:    return parse_input(p);
        case TOK_IF:       return parse_if(p);
        case TOK_FOR:      return parse_for(p);
        case TOK_WHILE:    return parse_while(p);
        case TOK_DO:       return parse_do(p);
        case TOK_SELECT:   return parse_select(p);
        case TOK_EXIT:     return parse_exit(p);
        case TOK_CONST:    return parse_const(p);
        case TOK_SHARED:   return parse_shared(p);
        case TOK_SUB:      return parse_sub(p);
        case TOK_FUNCTION: return parse_function(p);
        case TOK_CALL:     return parse_call(p);
        case TOK_DECLARE:  return parse_declare(p);
        case TOK_DIM:      return parse_dim_or_redim(p, 0);
        case TOK_REDIM:    return parse_dim_or_redim(p, 1);
        case TOK_ERASE:    return parse_erase(p);
        case TOK_DATA:     return parse_data(p);
        case TOK_READ:     return parse_read(p);
        case TOK_RESTORE:  return parse_restore(p);

        case TOK_SWAP: {
            int line = tok->line;
            lexer_next(&p->lex);
            expr_t *lhs = parse_primary(p);
            if (!lhs) return NULL;
            if (!lexer_match(&p->lex, TOK_COMMA)) {
                expr_free(lhs);
                parser_error(p, ERR_SYNTAX); return NULL;
            }
            expr_t *rhs = parse_primary(p);
            if (!rhs) { expr_free(lhs); return NULL; }
            return stmt_swap(lhs, rhs, line);
        }

        case TOK_RANDOMIZE: {
            int line = tok->line;
            lexer_next(&p->lex);
            expr_t *seed = NULL;
            if (!at_stmt_end(p))
                seed = parse_expr(p);
            return stmt_randomize(seed, line);
        }

        case TOK_OPEN:     return parse_open(p);
        case TOK_CLOSE:    return parse_close(p);
        case TOK_WRITE:    return parse_write_file(p);
        case TOK_LINE:     return parse_line_input(p);
        case TOK_KILL:     return parse_kill(p);
        case TOK_NAME:     return parse_name_stmt(p);

        case TOK_TYPE:     return parse_type_def(p);
        case TOK_ON:       return parse_on(p);
        case TOK_RESUME:   return parse_resume(p);
        case TOK_ERROR:    return parse_error_stmt(p);
        case TOK_DEFINT:   return parse_deftype(p, VAL_INTEGER);
        case TOK_DEFDBL:   return parse_deftype(p, VAL_DOUBLE);
        case TOK_DEFSTR:   return parse_deftype(p, VAL_STRING);

        case TOK_GOTO: {
            int line = tok->line;
            lexer_next(&p->lex);
            if (!lexer_check(&p->lex, TOK_IDENT)) {
                parser_error(p, ERR_SYNTAX); return NULL;
            }
            token_t label = lexer_next(&p->lex);
            return stmt_goto(label.text, line);
        }

        case TOK_GOSUB: {
            int line = tok->line;
            lexer_next(&p->lex);
            if (!lexer_check(&p->lex, TOK_IDENT)) {
                parser_error(p, ERR_SYNTAX); return NULL;
            }
            token_t label = lexer_next(&p->lex);
            return stmt_gosub(label.text, line);
        }

        case TOK_RETURN:
            lexer_next(&p->lex);
            return stmt_return(tok->line);

        case TOK_LET: {
            lexer_next(&p->lex);
            if (!lexer_check(&p->lex, TOK_IDENT)) {
                parser_error(p, ERR_SYNTAX); return NULL;
            }
            token_t var = lexer_next(&p->lex);
            return parse_assign(p, var.text);
        }

        case TOK_END: {
            /* Check for END SUB, END FUNCTION, END IF, END SELECT —
               if none match, it's a bare END (halt) */
            if (check_end_keyword(p, TOK_SUB) ||
                check_end_keyword(p, TOK_FUNCTION) ||
                check_end_keyword(p, TOK_IF) ||
                check_end_keyword(p, TOK_SELECT)) {
                /* These should be caught by parse_block. If we get here,
                   it's a mismatched block terminator. */
                parser_error(p, ERR_SYNTAX);
                return NULL;
            }
            lexer_next(&p->lex);
            return stmt_end(tok->line);
        }

        case TOK_REM:
        case TOK_EOL:
            lexer_next(&p->lex);
            return stmt_alloc(STMT_REM, tok->line);

        case TOK_IDENT: {
            token_t var = lexer_next(&p->lex);
            /* Assignment: ident = expr */
            if (lexer_check(&p->lex, TOK_EQ))
                return parse_assign(p, var.text);
            /* Field assignment: ident.field = expr */
            if (lexer_check(&p->lex, TOK_DOT)) {
                lexer_next(&p->lex); /* consume . */
                if (!lexer_check(&p->lex, TOK_IDENT)) {
                    parser_error(p, ERR_SYNTAX); return NULL;
                }
                token_t field = lexer_next(&p->lex);
                if (!expect(p, TOK_EQ)) return NULL;
                expr_t *val = parse_expr(p);
                if (!val) return NULL;
                return stmt_field_assign(var.text, field.text, val, var.line);
            }
            /* Label: ident: */
            if (lexer_check(&p->lex, TOK_COLON)) {
                lexer_next(&p->lex);
                return stmt_label(var.text, var.line);
            }
            /* Array assignment or paren call: ident(...) */
            if (lexer_check(&p->lex, TOK_LPAREN))
                return parse_paren_dispatch(p, var.text, var.line);
            /* OPTION BASE */
            if (strcmp(var.text, "OPTION") == 0)
                return parse_option(p, var.line);
            /* STATIC keyword (context-sensitive in SUB/FUNCTION) */
            if (strcmp(var.text, "STATIC") == 0) {
                /* For now, treat STATIC as a declaration — just skip */
                return stmt_alloc(STMT_REM, var.line);
            }
            /* Implicit call: ident [args] */
            return parse_implicit_call(p, var.text, var.line);
        }

        default:
            if (tok->type == TOK_EOF) return NULL;
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
    stmt_t *tail = NULL;
    while (!lexer_check(&p->lex, TOK_EOF)) {
        skip_eol(p);
        if (lexer_check(&p->lex, TOK_EOF)) break;
        if (lexer_check(&p->lex, TOK_COLON)) {
            lexer_next(&p->lex);
            continue;
        }
        stmt_t *s = parse_stmt(p);
        if (!s) {
            if (p->error != ERR_NONE) { stmt_free(head); return NULL; }
            continue;
        }
        stmt_append(&head, &tail, s);
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
