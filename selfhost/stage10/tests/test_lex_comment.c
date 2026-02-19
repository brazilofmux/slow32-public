#include "s32cc_lex.h"

/* Test comment handling */

int check(int tok, int expect, int errcode) {
    if (tok != expect) return errcode;
    return 0;
}

int main(void) {
    int rc;
    char *src;

    /* Line comment — tokens before and after */
    src = "a // comment\nb";
    lex_init(src, strlen(src));
    lex_next();
    rc = check(lex_tok, TK_IDENT, 1);
    if (rc) return rc;
    rc = check(strcmp(lex_str, "a"), 0, 2);
    if (rc) return rc;
    lex_next();
    rc = check(lex_tok, TK_IDENT, 3);
    if (rc) return rc;
    rc = check(strcmp(lex_str, "b"), 0, 4);
    if (rc) return rc;

    /* Block comment */
    src = "a /* comment */ b";
    lex_init(src, strlen(src));
    lex_next();
    rc = check(lex_tok, TK_IDENT, 5);
    if (rc) return rc;
    lex_next();
    rc = check(lex_tok, TK_IDENT, 6);
    if (rc) return rc;
    rc = check(strcmp(lex_str, "b"), 0, 7);
    if (rc) return rc;

    /* Multi-line block comment */
    src = "a /* line1\nline2\nline3 */ b";
    lex_init(src, strlen(src));
    lex_next();
    rc = check(lex_tok, TK_IDENT, 8);
    if (rc) return rc;
    lex_next();
    rc = check(lex_tok, TK_IDENT, 9);
    if (rc) return rc;
    rc = check(strcmp(lex_str, "b"), 0, 10);
    if (rc) return rc;

    /* C-style comment with star inside */
    src = "a /* ** */ b";
    lex_init(src, strlen(src));
    lex_next();
    rc = check(lex_tok, TK_IDENT, 11);
    if (rc) return rc;
    lex_next();
    rc = check(lex_tok, TK_IDENT, 12);
    if (rc) return rc;

    /* Only comments — should give EOF */
    src = "// just a comment";
    lex_init(src, strlen(src));
    lex_next();
    rc = check(lex_tok, TK_EOF, 13);
    if (rc) return rc;

    /* Block comment only */
    src = "/* block */";
    lex_init(src, strlen(src));
    lex_next();
    rc = check(lex_tok, TK_EOF, 14);
    if (rc) return rc;

    /* // inside block comment is ignored */
    src = "/* // not a line comment */ x";
    lex_init(src, strlen(src));
    lex_next();
    rc = check(lex_tok, TK_IDENT, 15);
    if (rc) return rc;
    rc = check(strcmp(lex_str, "x"), 0, 16);
    if (rc) return rc;

    /* Multiple comments in a row */
    src = "/* a */ /* b */ x";
    lex_init(src, strlen(src));
    lex_next();
    rc = check(lex_tok, TK_IDENT, 17);
    if (rc) return rc;
    rc = check(strcmp(lex_str, "x"), 0, 18);
    if (rc) return rc;

    return 0;
}
