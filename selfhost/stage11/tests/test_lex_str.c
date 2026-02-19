#include "s32cc_lex.h"

/* Test string literal scanning */

int check(int tok, int expect, int errcode) {
    if (tok != expect) return errcode;
    return 0;
}

int main(void) {
    int rc;

    /* Empty string */
    lex_init("\"\"", 2);
    lex_next();
    rc = check(lex_tok, TK_STRING, 1);
    if (rc) return rc;
    rc = check(lex_slen, 0, 2);
    if (rc) return rc;

    /* Simple string */
    lex_init("\"hello\"", 7);
    lex_next();
    rc = check(lex_tok, TK_STRING, 3);
    if (rc) return rc;
    rc = check(lex_slen, 5, 4);
    if (rc) return rc;
    rc = check(strcmp(lex_str, "hello"), 0, 5);
    if (rc) return rc;

    /* String with \n escape */
    lex_init("\"a\\nb\"", 6);
    lex_next();
    rc = check(lex_tok, TK_STRING, 6);
    if (rc) return rc;
    rc = check(lex_slen, 3, 7);
    if (rc) return rc;
    rc = check(lex_str[0], 97, 8);  /* 'a' */
    if (rc) return rc;
    rc = check(lex_str[1], 10, 9);  /* newline */
    if (rc) return rc;
    rc = check(lex_str[2], 98, 10); /* 'b' */
    if (rc) return rc;

    /* String with \t escape */
    lex_init("\"a\\tb\"", 6);
    lex_next();
    rc = check(lex_tok, TK_STRING, 11);
    if (rc) return rc;
    rc = check(lex_str[1], 9, 12);  /* tab */
    if (rc) return rc;

    /* String with \\ escape */
    lex_init("\"a\\\\b\"", 6);
    lex_next();
    rc = check(lex_tok, TK_STRING, 13);
    if (rc) return rc;
    rc = check(lex_str[1], 92, 14); /* backslash */
    if (rc) return rc;

    /* String with \" escape */
    lex_init("\"a\\\"b\"", 6);
    lex_next();
    rc = check(lex_tok, TK_STRING, 15);
    if (rc) return rc;
    rc = check(lex_str[1], 34, 16); /* double quote */
    if (rc) return rc;

    /* String with hex escape \x41 = 'A' */
    lex_init("\"\\x41\"", 6);
    lex_next();
    rc = check(lex_tok, TK_STRING, 17);
    if (rc) return rc;
    rc = check(lex_slen, 1, 18);
    if (rc) return rc;
    rc = check(lex_str[0], 65, 19); /* 'A' = 0x41 */
    if (rc) return rc;

    /* String with octal escape \101 = 'A' */
    lex_init("\"\\101\"", 6);
    lex_next();
    rc = check(lex_tok, TK_STRING, 20);
    if (rc) return rc;
    rc = check(lex_slen, 1, 21);
    if (rc) return rc;
    rc = check(lex_str[0], 65, 22); /* 'A' = 0101 */
    if (rc) return rc;

    /* String with \0 (null char) */
    lex_init("\"a\\0b\"", 6);
    lex_next();
    rc = check(lex_tok, TK_STRING, 23);
    if (rc) return rc;
    rc = check(lex_slen, 3, 24);
    if (rc) return rc;
    rc = check(lex_str[0], 97, 25);  /* 'a' */
    if (rc) return rc;
    rc = check(lex_str[1], 0, 26);   /* NUL */
    if (rc) return rc;
    /* Note: lex_str[2] is 'b' but strlen won't reach it */

    /* String with all named escapes */
    lex_init("\"\\a\\b\\f\\r\\v\"", 12);
    lex_next();
    rc = check(lex_tok, TK_STRING, 27);
    if (rc) return rc;
    rc = check(lex_str[0], 7, 28);   /* \a = bell */
    if (rc) return rc;
    rc = check(lex_str[1], 8, 29);   /* \b = backspace */
    if (rc) return rc;
    rc = check(lex_str[2], 12, 30);  /* \f = form feed */
    if (rc) return rc;
    rc = check(lex_str[3], 13, 31);  /* \r = CR */
    if (rc) return rc;
    rc = check(lex_str[4], 11, 32);  /* \v = vertical tab */
    if (rc) return rc;

    /* String pool index increments */
    lex_init("\"first\" \"second\"", 16);
    lex_next();
    rc = check(lex_tok, TK_STRING, 33);
    if (rc) return rc;
    rc = check(lex_val, 0, 34);  /* first string index */
    if (rc) return rc;
    lex_next();
    rc = check(lex_tok, TK_STRING, 35);
    if (rc) return rc;
    rc = check(lex_val, 1, 36);  /* second string index */
    if (rc) return rc;

    return 0;
}
