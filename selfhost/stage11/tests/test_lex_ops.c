#include "s32cc_lex.h"

/* Test operator and punctuation scanning */

int check(int tok, int expect, int errcode) {
    if (tok != expect) return errcode;
    return 0;
}

int main(void) {
    int rc;

    /* Simple single-char operators */
    lex_init("( ) [ ] { } ; , ~ ? :", 21);

    lex_next(); rc = check(lex_tok, TK_LPAREN, 1); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_RPAREN, 2); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_LBRACK, 3); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_RBRACK, 4); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_LBRACE, 5); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_RBRACE, 6); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_SEMI, 7); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_COMMA, 8); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_TILDE, 9); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_QMARK, 10); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_COLON, 11); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_EOF, 12); if (rc) return rc;

    /* Dot and ellipsis */
    lex_init(". ...", 5);
    lex_next(); rc = check(lex_tok, TK_DOT, 13); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_ELLIPSIS, 14); if (rc) return rc;

    /* Plus family: + += ++ */
    lex_init("+ += ++", 7);
    lex_next(); rc = check(lex_tok, TK_PLUS, 15); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_PLUSEQ, 16); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_INC, 17); if (rc) return rc;

    /* Minus family: - -= -- -> */
    lex_init("- -= -- ->", 10);
    lex_next(); rc = check(lex_tok, TK_MINUS, 18); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_MINUSEQ, 19); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_DEC, 20); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_ARROW, 21); if (rc) return rc;

    /* Star family: * *= */
    lex_init("* *=", 4);
    lex_next(); rc = check(lex_tok, TK_STAR, 22); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_STAREQ, 23); if (rc) return rc;

    /* Slash family: / /= */
    lex_init("/ /=", 4);
    lex_next(); rc = check(lex_tok, TK_SLASH, 24); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_SLASHEQ, 25); if (rc) return rc;

    /* Percent family: % %= */
    lex_init("% %=", 4);
    lex_next(); rc = check(lex_tok, TK_PERCENT, 26); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_PERCENTEQ, 27); if (rc) return rc;

    /* Ampersand family: & &= && */
    lex_init("& &= &&", 7);
    lex_next(); rc = check(lex_tok, TK_AMP, 28); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_AMPEQ, 29); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_LAND, 30); if (rc) return rc;

    /* Pipe family: | |= || */
    lex_init("| |= ||", 7);
    lex_next(); rc = check(lex_tok, TK_PIPE, 31); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_PIPEEQ, 32); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_LOR, 33); if (rc) return rc;

    /* Caret family: ^ ^= */
    lex_init("^ ^=", 4);
    lex_next(); rc = check(lex_tok, TK_CARET, 34); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_CARETEQ, 35); if (rc) return rc;

    /* Bang family: ! != */
    lex_init("! !=", 4);
    lex_next(); rc = check(lex_tok, TK_BANG, 36); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_NE, 37); if (rc) return rc;

    /* Assign family: = == */
    lex_init("= ==", 4);
    lex_next(); rc = check(lex_tok, TK_ASSIGN, 38); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_EQ, 39); if (rc) return rc;

    /* Less-than family: < <= << <<= */
    lex_init("< <= << <<=", 11);
    lex_next(); rc = check(lex_tok, TK_LT, 40); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_LE, 41); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_LSHIFT, 42); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_LSHIFTEQ, 43); if (rc) return rc;

    /* Greater-than family: > >= >> >>= */
    lex_init("> >= >> >>=", 11);
    lex_next(); rc = check(lex_tok, TK_GT, 44); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_GE, 45); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_RSHIFT, 46); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_RSHIFTEQ, 47); if (rc) return rc;

    /* Hash family: # ## */
    lex_init("# ##", 4);
    lex_next(); rc = check(lex_tok, TK_HASH, 48); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_HASHHASH, 49); if (rc) return rc;

    /* Operators without spaces (disambiguation) */
    lex_init("a->b", 4);
    lex_next(); rc = check(lex_tok, TK_IDENT, 50); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_ARROW, 51); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_IDENT, 52); if (rc) return rc;

    /* Tricky: <<= without spaces */
    lex_init("x<<=1", 5);
    lex_next(); rc = check(lex_tok, TK_IDENT, 53); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_LSHIFTEQ, 54); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_NUM, 55); if (rc) return rc;

    /* Tricky: >>= without spaces */
    lex_init("x>>=2", 5);
    lex_next(); rc = check(lex_tok, TK_IDENT, 56); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_RSHIFTEQ, 57); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_NUM, 58); if (rc) return rc;

    return 0;
}
