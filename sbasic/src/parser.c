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

static stmt_t *parse_block(parser_t *p, int flags) {
    stmt_t *head = NULL;

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
        stmt_append(&head, s);
    }
    return head;
}

/* --- Statement parsing --- */

static stmt_t *parse_print(parser_t *p) {
    int line = lexer_peek(&p->lex)->line;
    lexer_next(&p->lex);
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
            if (!lexer_check(&p->lex, TOK_IDENT)) {
                parser_error(p, ERR_SYNTAX); return NULL;
            }
            token_t v1 = lexer_next(&p->lex);
            if (!lexer_match(&p->lex, TOK_COMMA)) {
                parser_error(p, ERR_SYNTAX); return NULL;
            }
            if (!lexer_check(&p->lex, TOK_IDENT)) {
                parser_error(p, ERR_SYNTAX); return NULL;
            }
            token_t v2 = lexer_next(&p->lex);
            return stmt_swap(v1.text, var_type_from_name(v1.text),
                             v2.text, var_type_from_name(v2.text), line);
        }

        case TOK_RANDOMIZE: {
            int line = tok->line;
            lexer_next(&p->lex);
            expr_t *seed = NULL;
            if (!at_stmt_end(p))
                seed = parse_expr(p);
            return stmt_randomize(seed, line);
        }

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
