#ifndef LEX_H
#define LEX_H

#include <stdint.h>
#include "expr.h" /* for value_t if needed, or we can define token values separately */

typedef enum {
    TOK_EOF = 0,
    TOK_ERROR,
    
    /* Literals */
    TOK_IDENT,
    TOK_STRING,
    TOK_NUMBER,
    TOK_DATE,
    TOK_LOGIC,
    
    /* Operators */
    TOK_PLUS,       /* + */
    TOK_MINUS,      /* - */
    TOK_MUL,        /* * */
    TOK_DIV,        /* / */
    TOK_LPAREN,     /* ( */
    TOK_RPAREN,     /* ) */
    TOK_COMMA,      /* , */
    TOK_ARROW,      /* -> */
    
    /* Comparison */
    TOK_EQ,         /* = */
    TOK_NE,         /* <> or # */
    TOK_LT,         /* < */
    TOK_GT,         /* > */
    TOK_LE,         /* <= */
    TOK_GE,         /* >= */
    TOK_SUBSTR,     /* $ */
    
    /* Logical Keywords (delimited by dots) */
    TOK_AND,        /* .AND. */
    TOK_OR,         /* .OR. */
    TOK_NOT,        /* .NOT. */
    
    /* Macro */
    TOK_MACRO       /* & */
} token_type_t;

typedef struct {
    token_type_t type;
    char text[256];
    double num_val;
    int logic_val;
    int32_t date_val;
} token_t;

typedef struct {
    const char *input;
    const char *p;
    const char *token_start;
    token_t current;
    const char *error;
} lexer_t;

void lexer_init(lexer_t *l, const char *input);
token_type_t lex_next(lexer_t *l);
token_type_t lex_peek(lexer_t *l);

/* Helper to check for keywords with 4-char rule */
int is_keyword(const char *ident, const char *kw);
#define IS_KW(ident, kw) is_keyword(ident, kw)

#endif
