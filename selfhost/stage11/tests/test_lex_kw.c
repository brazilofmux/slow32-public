#include "s32cc_lex.h"

/* Test keyword recognition and near-miss identifiers */

int check(int tok, int expect, int errcode) {
    if (tok != expect) return errcode;
    return 0;
}

int test_keywords(void) {
    int rc;

    /* auto */
    lex_init("auto", 4);
    lex_next();
    rc = check(lex_tok, TK_AUTO, 1);
    if (rc) return rc;

    /* break */
    lex_init("break", 5);
    lex_next();
    rc = check(lex_tok, TK_BREAK, 2);
    if (rc) return rc;

    /* case */
    lex_init("case", 4);
    lex_next();
    rc = check(lex_tok, TK_CASE, 3);
    if (rc) return rc;

    /* char */
    lex_init("char", 4);
    lex_next();
    rc = check(lex_tok, TK_CHAR, 4);
    if (rc) return rc;

    /* const */
    lex_init("const", 5);
    lex_next();
    rc = check(lex_tok, TK_CONST, 5);
    if (rc) return rc;

    /* continue */
    lex_init("continue", 8);
    lex_next();
    rc = check(lex_tok, TK_CONTINUE, 6);
    if (rc) return rc;

    /* default */
    lex_init("default", 7);
    lex_next();
    rc = check(lex_tok, TK_DEFAULT, 7);
    if (rc) return rc;

    /* do */
    lex_init("do", 2);
    lex_next();
    rc = check(lex_tok, TK_DO, 8);
    if (rc) return rc;

    /* double */
    lex_init("double", 6);
    lex_next();
    rc = check(lex_tok, TK_DOUBLE, 9);
    if (rc) return rc;

    /* else */
    lex_init("else", 4);
    lex_next();
    rc = check(lex_tok, TK_ELSE, 10);
    if (rc) return rc;

    /* enum */
    lex_init("enum", 4);
    lex_next();
    rc = check(lex_tok, TK_ENUM, 11);
    if (rc) return rc;

    /* extern */
    lex_init("extern", 6);
    lex_next();
    rc = check(lex_tok, TK_EXTERN, 12);
    if (rc) return rc;

    /* float */
    lex_init("float", 5);
    lex_next();
    rc = check(lex_tok, TK_FLOAT, 13);
    if (rc) return rc;

    /* for */
    lex_init("for", 3);
    lex_next();
    rc = check(lex_tok, TK_FOR, 14);
    if (rc) return rc;

    /* goto */
    lex_init("goto", 4);
    lex_next();
    rc = check(lex_tok, TK_GOTO, 15);
    if (rc) return rc;

    /* if */
    lex_init("if", 2);
    lex_next();
    rc = check(lex_tok, TK_IF, 16);
    if (rc) return rc;

    /* inline */
    lex_init("inline", 6);
    lex_next();
    rc = check(lex_tok, TK_INLINE, 17);
    if (rc) return rc;

    /* int */
    lex_init("int", 3);
    lex_next();
    rc = check(lex_tok, TK_INT, 18);
    if (rc) return rc;

    /* long */
    lex_init("long", 4);
    lex_next();
    rc = check(lex_tok, TK_LONG, 19);
    if (rc) return rc;

    /* register */
    lex_init("register", 8);
    lex_next();
    rc = check(lex_tok, TK_REGISTER, 20);
    if (rc) return rc;

    /* restrict */
    lex_init("restrict", 8);
    lex_next();
    rc = check(lex_tok, TK_RESTRICT, 21);
    if (rc) return rc;

    /* return */
    lex_init("return", 6);
    lex_next();
    rc = check(lex_tok, TK_RETURN, 22);
    if (rc) return rc;

    /* short */
    lex_init("short", 5);
    lex_next();
    rc = check(lex_tok, TK_SHORT, 23);
    if (rc) return rc;

    /* signed */
    lex_init("signed", 6);
    lex_next();
    rc = check(lex_tok, TK_SIGNED, 24);
    if (rc) return rc;

    /* sizeof */
    lex_init("sizeof", 6);
    lex_next();
    rc = check(lex_tok, TK_SIZEOF, 25);
    if (rc) return rc;

    /* static */
    lex_init("static", 6);
    lex_next();
    rc = check(lex_tok, TK_STATIC, 26);
    if (rc) return rc;

    /* struct */
    lex_init("struct", 6);
    lex_next();
    rc = check(lex_tok, TK_STRUCT, 27);
    if (rc) return rc;

    /* switch */
    lex_init("switch", 6);
    lex_next();
    rc = check(lex_tok, TK_SWITCH, 28);
    if (rc) return rc;

    /* typedef */
    lex_init("typedef", 7);
    lex_next();
    rc = check(lex_tok, TK_TYPEDEF, 29);
    if (rc) return rc;

    /* union */
    lex_init("union", 5);
    lex_next();
    rc = check(lex_tok, TK_UNION, 30);
    if (rc) return rc;

    /* unsigned */
    lex_init("unsigned", 8);
    lex_next();
    rc = check(lex_tok, TK_UNSIGNED, 31);
    if (rc) return rc;

    /* void */
    lex_init("void", 4);
    lex_next();
    rc = check(lex_tok, TK_VOID, 32);
    if (rc) return rc;

    /* volatile */
    lex_init("volatile", 8);
    lex_next();
    rc = check(lex_tok, TK_VOLATILE, 33);
    if (rc) return rc;

    /* while */
    lex_init("while", 5);
    lex_next();
    rc = check(lex_tok, TK_WHILE, 34);
    if (rc) return rc;

    return 0;
}

int test_near_misses(void) {
    int rc;

    /* "iff" is not a keyword */
    lex_init("iff", 3);
    lex_next();
    rc = check(lex_tok, TK_IDENT, 40);
    if (rc) return rc;
    rc = check(strcmp(lex_str, "iff"), 0, 41);
    if (rc) return rc;

    /* "integer" is not a keyword */
    lex_init("integer", 7);
    lex_next();
    rc = check(lex_tok, TK_IDENT, 42);
    if (rc) return rc;

    /* "returning" is not a keyword */
    lex_init("returning", 9);
    lex_next();
    rc = check(lex_tok, TK_IDENT, 43);
    if (rc) return rc;

    /* "ifx" is not a keyword */
    lex_init("ifx", 3);
    lex_next();
    rc = check(lex_tok, TK_IDENT, 44);
    if (rc) return rc;

    /* "fortune" is not a keyword */
    lex_init("fortune", 7);
    lex_next();
    rc = check(lex_tok, TK_IDENT, 45);
    if (rc) return rc;

    /* "voidptr" is not a keyword */
    lex_init("voidptr", 7);
    lex_next();
    rc = check(lex_tok, TK_IDENT, 46);
    if (rc) return rc;

    return 0;
}

int test_kw_followed_by_token(void) {
    int rc;

    /* "int x" => TK_INT, TK_IDENT */
    lex_init("int x", 5);
    lex_next();
    rc = check(lex_tok, TK_INT, 50);
    if (rc) return rc;
    lex_next();
    rc = check(lex_tok, TK_IDENT, 51);
    if (rc) return rc;
    rc = check(strcmp(lex_str, "x"), 0, 52);
    if (rc) return rc;
    lex_next();
    rc = check(lex_tok, TK_EOF, 53);
    if (rc) return rc;

    return 0;
}

int main(void) {
    int rc;

    rc = test_keywords();
    if (rc) return rc;

    rc = test_near_misses();
    if (rc) return rc;

    rc = test_kw_followed_by_token();
    if (rc) return rc;

    return 0;
}
