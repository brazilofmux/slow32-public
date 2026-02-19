#include "s32cc_lex.h"

/* Test identifier scanning */

int check(int tok, int expect, int errcode) {
    if (tok != expect) return errcode;
    return 0;
}

int main(void) {
    int rc;

    /* Simple identifier */
    lex_init("foo", 3);
    lex_next();
    rc = check(lex_tok, TK_IDENT, 1);
    if (rc) return rc;
    rc = check(strcmp(lex_str, "foo"), 0, 2);
    if (rc) return rc;

    /* Single letter */
    lex_init("x", 1);
    lex_next();
    rc = check(lex_tok, TK_IDENT, 3);
    if (rc) return rc;
    rc = check(strcmp(lex_str, "x"), 0, 4);
    if (rc) return rc;

    /* Underscore prefix */
    lex_init("_foo", 4);
    lex_next();
    rc = check(lex_tok, TK_IDENT, 5);
    if (rc) return rc;
    rc = check(strcmp(lex_str, "_foo"), 0, 6);
    if (rc) return rc;

    /* Double underscore */
    lex_init("__bar__", 7);
    lex_next();
    rc = check(lex_tok, TK_IDENT, 7);
    if (rc) return rc;
    rc = check(strcmp(lex_str, "__bar__"), 0, 8);
    if (rc) return rc;

    /* Mixed case with digits */
    lex_init("myVar42", 7);
    lex_next();
    rc = check(lex_tok, TK_IDENT, 9);
    if (rc) return rc;
    rc = check(strcmp(lex_str, "myVar42"), 0, 10);
    if (rc) return rc;

    /* Just underscore */
    lex_init("_", 1);
    lex_next();
    rc = check(lex_tok, TK_IDENT, 11);
    if (rc) return rc;
    rc = check(strcmp(lex_str, "_"), 0, 12);
    if (rc) return rc;

    /* Keyword prefix that is an identifier */
    lex_init("ifx", 3);
    lex_next();
    rc = check(lex_tok, TK_IDENT, 13);
    if (rc) return rc;
    rc = check(strcmp(lex_str, "ifx"), 0, 14);
    if (rc) return rc;

    /* Multiple identifiers */
    lex_init("abc def ghi", 11);
    lex_next();
    rc = check(lex_tok, TK_IDENT, 15);
    if (rc) return rc;
    rc = check(strcmp(lex_str, "abc"), 0, 16);
    if (rc) return rc;
    lex_next();
    rc = check(lex_tok, TK_IDENT, 17);
    if (rc) return rc;
    rc = check(strcmp(lex_str, "def"), 0, 18);
    if (rc) return rc;
    lex_next();
    rc = check(lex_tok, TK_IDENT, 19);
    if (rc) return rc;
    rc = check(strcmp(lex_str, "ghi"), 0, 20);
    if (rc) return rc;
    lex_next();
    rc = check(lex_tok, TK_EOF, 21);
    if (rc) return rc;

    /* Long identifier (up to 47 chars fits) */
    lex_init("abcdefghijklmnopqrstuvwxyz_12345", 32);
    lex_next();
    rc = check(lex_tok, TK_IDENT, 22);
    if (rc) return rc;

    return 0;
}
