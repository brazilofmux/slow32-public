#include "s32cc_lex.h"

/* Test character literal scanning */

int check(int tok, int expect, int errcode) {
    if (tok != expect) return errcode;
    return 0;
}

int main(void) {
    int rc;

    /* Simple char 'x' = 120 */
    lex_init("'x'", 3);
    lex_next();
    rc = check(lex_tok, TK_CHARLIT, 1);
    if (rc) return rc;
    rc = check(lex_val, 120, 2);
    if (rc) return rc;

    /* Char 'A' = 65 */
    lex_init("'A'", 3);
    lex_next();
    rc = check(lex_tok, TK_CHARLIT, 3);
    if (rc) return rc;
    rc = check(lex_val, 65, 4);
    if (rc) return rc;

    /* Char '0' = 48 */
    lex_init("'0'", 3);
    lex_next();
    rc = check(lex_tok, TK_CHARLIT, 5);
    if (rc) return rc;
    rc = check(lex_val, 48, 6);
    if (rc) return rc;

    /* Escape \n = 10 */
    lex_init("'\\n'", 4);
    lex_next();
    rc = check(lex_tok, TK_CHARLIT, 7);
    if (rc) return rc;
    rc = check(lex_val, 10, 8);
    if (rc) return rc;

    /* Escape \t = 9 */
    lex_init("'\\t'", 4);
    lex_next();
    rc = check(lex_tok, TK_CHARLIT, 9);
    if (rc) return rc;
    rc = check(lex_val, 9, 10);
    if (rc) return rc;

    /* Escape \0 = 0 */
    lex_init("'\\0'", 4);
    lex_next();
    rc = check(lex_tok, TK_CHARLIT, 11);
    if (rc) return rc;
    rc = check(lex_val, 0, 12);
    if (rc) return rc;

    /* Escape \\ = 92 */
    lex_init("'\\\\'", 4);
    lex_next();
    rc = check(lex_tok, TK_CHARLIT, 13);
    if (rc) return rc;
    rc = check(lex_val, 92, 14);
    if (rc) return rc;

    /* Escape \' = 39 */
    lex_init("'\\''", 4);
    lex_next();
    rc = check(lex_tok, TK_CHARLIT, 15);
    if (rc) return rc;
    rc = check(lex_val, 39, 16);
    if (rc) return rc;

    /* Hex escape \x41 = 65 = 'A' */
    lex_init("'\\x41'", 6);
    lex_next();
    rc = check(lex_tok, TK_CHARLIT, 17);
    if (rc) return rc;
    rc = check(lex_val, 65, 18);
    if (rc) return rc;

    /* Octal escape \101 = 65 = 'A' */
    lex_init("'\\101'", 6);
    lex_next();
    rc = check(lex_tok, TK_CHARLIT, 19);
    if (rc) return rc;
    rc = check(lex_val, 65, 20);
    if (rc) return rc;

    return 0;
}
