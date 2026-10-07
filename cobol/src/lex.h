/* lex.h -- the COBOL token scanner's interface (lex.rl, Ragel -G2) */
#ifndef S32_COBC_LEX_H
#define S32_COBC_LEX_H

enum {
    LX_SPACE,     /* spaces and tabs */
    LX_COMMENT,   /* *> to the end of the line */
    LX_PDELIM,    /* == */
    LX_LP, LX_RP, LX_COLON,
    LX_PERIOD,    /* a period that separates: before a space, the end, or == (a doubled one is one) */
    LX_SEP,       /* a comma or semicolon that separates: before a space, the end, or == */
    LX_COMMA,     /* a comma or semicolon tight to what follows */
    LX_DOT,       /* a period tight to what follows (the tokenizer refuses it, the text-word scanner runs it in) */
    LX_LIT,       /* a literal with its prefix letters (prefix) and quotes; bad: not closed on its line */
    LX_NUM,       /* a numeric literal: [+-] digits [. digits] | . digits, [E [+-] digits] (exp) */
    LX_WORD,      /* digits* letter (letters digits - _)* */
    LX_OP,        /* ** >= <= <> = < > + - * / & */
    LX_OTHER      /* any other byte, one at a time */
};

typedef struct { int kind; const char *s; int len; int prefix; int exp; int bad; } Lexeme;

int lx_next(const char *p, const char *pe, Lexeme *out);

#endif
