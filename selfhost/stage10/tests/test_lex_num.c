#include "s32cc_lex.h"

/* Test numeric literal scanning */

int check(int tok, int expect, int errcode) {
    if (tok != expect) return errcode;
    return 0;
}

int main(void) {
    int rc;

    /* Decimal zero */
    lex_init("0", 1);
    lex_next();
    rc = check(lex_tok, TK_NUM, 1);
    if (rc) return rc;
    rc = check(lex_val, 0, 2);
    if (rc) return rc;

    /* Simple decimal */
    lex_init("1", 1);
    lex_next();
    rc = check(lex_tok, TK_NUM, 3);
    if (rc) return rc;
    rc = check(lex_val, 1, 4);
    if (rc) return rc;

    /* Multi-digit decimal */
    lex_init("42", 2);
    lex_next();
    rc = check(lex_tok, TK_NUM, 5);
    if (rc) return rc;
    rc = check(lex_val, 42, 6);
    if (rc) return rc;

    /* Larger decimal */
    lex_init("12345", 5);
    lex_next();
    rc = check(lex_tok, TK_NUM, 7);
    if (rc) return rc;
    rc = check(lex_val, 12345, 8);
    if (rc) return rc;

    /* Hex: 0x0 */
    lex_init("0x0", 3);
    lex_next();
    rc = check(lex_tok, TK_NUM, 9);
    if (rc) return rc;
    rc = check(lex_val, 0, 10);
    if (rc) return rc;

    /* Hex: 0xFF */
    lex_init("0xFF", 4);
    lex_next();
    rc = check(lex_tok, TK_NUM, 11);
    if (rc) return rc;
    rc = check(lex_val, 255, 12);
    if (rc) return rc;

    /* Hex: 0xDEAD */
    lex_init("0xDEAD", 6);
    lex_next();
    rc = check(lex_tok, TK_NUM, 13);
    if (rc) return rc;
    rc = check(lex_val, 0xDEAD, 14);
    if (rc) return rc;

    /* Hex: uppercase 0X */
    lex_init("0X1A", 4);
    lex_next();
    rc = check(lex_tok, TK_NUM, 15);
    if (rc) return rc;
    rc = check(lex_val, 26, 16);
    if (rc) return rc;

    /* Octal: 07 */
    lex_init("07", 2);
    lex_next();
    rc = check(lex_tok, TK_NUM, 17);
    if (rc) return rc;
    rc = check(lex_val, 7, 18);
    if (rc) return rc;

    /* Octal: 0777 = 511 */
    lex_init("0777", 4);
    lex_next();
    rc = check(lex_tok, TK_NUM, 19);
    if (rc) return rc;
    rc = check(lex_val, 511, 20);
    if (rc) return rc;

    /* Suffix: 42U */
    lex_init("42U", 3);
    lex_next();
    rc = check(lex_tok, TK_NUM, 21);
    if (rc) return rc;
    rc = check(lex_val, 42, 22);
    if (rc) return rc;

    /* Suffix: 42L */
    lex_init("42L", 3);
    lex_next();
    rc = check(lex_tok, TK_NUM, 23);
    if (rc) return rc;
    rc = check(lex_val, 42, 24);
    if (rc) return rc;

    /* Suffix: 42UL */
    lex_init("42UL", 4);
    lex_next();
    rc = check(lex_tok, TK_NUM, 25);
    if (rc) return rc;
    rc = check(lex_val, 42, 26);
    if (rc) return rc;

    /* Number followed by other tokens */
    lex_init("123+456", 7);
    lex_next();
    rc = check(lex_tok, TK_NUM, 27);
    if (rc) return rc;
    rc = check(lex_val, 123, 28);
    if (rc) return rc;
    lex_next();
    rc = check(lex_tok, TK_PLUS, 29);
    if (rc) return rc;
    lex_next();
    rc = check(lex_tok, TK_NUM, 30);
    if (rc) return rc;
    rc = check(lex_val, 456, 31);
    if (rc) return rc;

    return 0;
}
