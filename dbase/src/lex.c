#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "lex.h"
#include "util.h"
#include "date.h"

void lexer_init(lexer_t *l, const char *input) {
    l->input = input;
    l->p = input;
    l->error = NULL;
    lex_next(l); /* prime the first token */
}

int is_keyword(const char *ident, const char *kw) {
    int ident_len = strlen(ident);
    int kw_len = strlen(kw);
    int min_len = (kw_len < 4) ? kw_len : 4;
    
    if (ident_len < min_len) return 0;
    if (ident_len > kw_len) return 0;

    return str_nicmp(ident, kw, ident_len) == 0;
}

static int match_dot_keyword(const char *p, const char *kw, int *consumed) {
    int i = 0;
    if (p[i] != '.') return 0;
    i++;
    while (p[i] && p[i] != '.') {
        i++;
    }
    if (p[i] != '.') return 0;
    
    /* We have .something. - check 'something' against kw */
    {
        int len = i - 1;
        char buf[32];
        if (len >= (int)sizeof(buf)) len = sizeof(buf) - 1;
        memcpy(buf, p + 1, len);
        buf[len] = '\0';
        if (str_icmp(buf, kw) == 0) {
            *consumed = i + 1;
            return 1;
        }
    }
    return 0;
}

token_type_t lex_next(lexer_t *l) {
    const char *p = l->p;
    token_t *t = &l->current;

    /* Skip whitespace */
    while (*p == ' ' || *p == '\t' || *p == '\r' || *p == '\n') p++;

    l->token_start = p;

    if (*p == '\0') {
        t->type = TOK_EOF;
        t->text[0] = '\0';
        l->p = p;
        return t->type;
    }

    t->text[0] = *p;
    t->text[1] = '\0';

    /* Identifiers and potential Keywords */
    if (is_ident_start(*p)) {
        int i = 0;
        while (is_ident_char(*p) && i < (int)sizeof(t->text) - 1) {
            t->text[i++] = *p++;
        }
        t->text[i] = '\0';
        t->type = TOK_IDENT;
        l->p = p;
        return t->type;
    }

    /* Numbers */
    if (isdigit(*p) || (*p == '.' && isdigit(p[1]))) {
        int i = 0;
        while ((isdigit(*p) || *p == '.') && i < (int)sizeof(t->text) - 1) {
            t->text[i++] = *p++;
        }
        t->text[i] = '\0';
        t->num_val = atof(t->text);
        t->type = TOK_NUMBER;
        l->p = p;
        return t->type;
    }

    /* String literals */
    if (*p == '"' || *p == '\'') {
        char quote = *p++;
        int i = 0;
        while (*p && *p != quote && i < (int)sizeof(t->text) - 1) {
            t->text[i++] = *p++;
        }
        t->text[i] = '\0';
        if (*p == quote) p++;
        t->type = TOK_STRING;
        l->p = p;
        return t->type;
    }

    /* Logical literals or Dot Keywords */
    if (*p == '.') {
        int consumed = 0;
        if (match_dot_keyword(p, "AND", &consumed)) {
            t->type = TOK_AND;
            l->p = p + consumed;
            return t->type;
        }
        if (match_dot_keyword(p, "OR", &consumed)) {
            t->type = TOK_OR;
            l->p = p + consumed;
            return t->type;
        }
        if (match_dot_keyword(p, "NOT", &consumed)) {
            t->type = TOK_NOT;
            l->p = p + consumed;
            return t->type;
        }
        /* Check for .T. and .F. */
        if (toupper((unsigned char)p[1]) == 'T' && p[2] == '.') {
            t->type = TOK_LOGIC;
            t->logic_val = 1;
            l->p = p + 3;
            return t->type;
        }
        if (toupper((unsigned char)p[1]) == 'F' && p[2] == '.') {
            t->type = TOK_LOGIC;
            t->logic_val = 0;
            l->p = p + 3;
            return t->type;
        }
    }

    /* Date literals {MM/DD/YY} */
    if (*p == '{') {
        const char *start = p;
        p++;
        while (*p && *p != '}') p++;
        if (*p == '}') {
            int len = p - start - 1;
            char buf[64];
            if (len >= (int)sizeof(buf)) len = sizeof(buf) - 1;
            memcpy(buf, start + 1, len);
            buf[len] = '\0';
            t->date_val = date_from_mdy(buf);
            t->type = TOK_DATE;
            l->p = p + 1;
            return t->type;
        }
        p = start; /* backtrack if no closing brace */
    }

    /* Multi-character operators */
    if (p[0] == '<' && p[1] == '>') { t->type = TOK_NE; l->p = p + 2; return t->type; }
    if (p[0] == '<' && p[1] == '=') { t->type = TOK_LE; l->p = p + 2; return t->type; }
    if (p[0] == '>' && p[1] == '=') { t->type = TOK_GE; l->p = p + 2; return t->type; }
    if (p[0] == '-' && p[1] == '>') { t->type = TOK_ARROW; l->p = p + 2; return t->type; }
    if (p[0] == '*' && p[1] == '*') { t->type = TOK_POWER; l->p = p + 2; return t->type; }
    if (p[0] == '=' && p[1] == '=') { t->type = TOK_EXACT_EQ; l->p = p + 2; return t->type; }

    /* Single character operators */
    switch (*p) {
        case '+': t->type = TOK_PLUS; break;
        case '-': t->type = TOK_MINUS; break;
        case '*': t->type = TOK_MUL; break;
        case '/': t->type = TOK_DIV; break;
        case '(': t->type = TOK_LPAREN; break;
        case ')': t->type = TOK_RPAREN; break;
        case ',': t->type = TOK_COMMA; break;
        case '=': t->type = TOK_EQ; break;
        case '<': t->type = TOK_LT; break;
        case '>': t->type = TOK_GT; break;
        case '#': t->type = TOK_NE; break;
        case '$': t->type = TOK_SUBSTR; break;
        case '&': t->type = TOK_MACRO; break;
        case '^': t->type = TOK_POWER; break;
        case '!': t->type = TOK_NOT; break;
        default:
            t->type = TOK_ERROR;
            l->error = "Unexpected character";
            break;
    }
    
    l->p = p + 1;
    return t->type;
}

token_type_t lex_peek(lexer_t *l) {
    return l->current.type;
}