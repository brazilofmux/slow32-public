#include "s32cc_lex.h"

/* Test real C code fragments — full token stream validation */

int check(int tok, int expect, int errcode) {
    if (tok != expect) return errcode;
    return 0;
}

int test_return_stmt(void) {
    int rc;
    /* "int main(void) { return 42; }" */
    char *src;
    src = "int main(void) { return 42; }";

    lex_init(src, strlen(src));

    lex_next(); rc = check(lex_tok, TK_INT, 1); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_IDENT, 2); if (rc) return rc;
    rc = check(strcmp(lex_str, "main"), 0, 3); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_LPAREN, 4); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_VOID, 5); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_RPAREN, 6); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_LBRACE, 7); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_RETURN, 8); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_NUM, 9); if (rc) return rc;
    rc = check(lex_val, 42, 10); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_SEMI, 11); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_RBRACE, 12); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_EOF, 13); if (rc) return rc;

    return 0;
}

int test_if_stmt(void) {
    int rc;
    /* "if (x >= 0 && x < 100) { y = x * 2; }" */
    char *src;
    src = "if (x >= 0 && x < 100) { y = x * 2; }";

    lex_init(src, strlen(src));

    lex_next(); rc = check(lex_tok, TK_IF, 20); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_LPAREN, 21); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_IDENT, 22); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_GE, 23); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_NUM, 24); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_LAND, 25); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_IDENT, 26); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_LT, 27); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_NUM, 28); if (rc) return rc;
    rc = check(lex_val, 100, 29); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_RPAREN, 30); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_LBRACE, 31); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_IDENT, 32); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_ASSIGN, 33); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_IDENT, 34); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_STAR, 35); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_NUM, 36); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_SEMI, 37); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_RBRACE, 38); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_EOF, 39); if (rc) return rc;

    return 0;
}

int test_struct(void) {
    int rc;
    /* "struct point { int x; int y; };" */
    char *src;
    src = "struct point { int x; int y; };";

    lex_init(src, strlen(src));

    lex_next(); rc = check(lex_tok, TK_STRUCT, 40); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_IDENT, 41); if (rc) return rc;
    rc = check(strcmp(lex_str, "point"), 0, 42); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_LBRACE, 43); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_INT, 44); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_IDENT, 45); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_SEMI, 46); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_INT, 47); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_IDENT, 48); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_SEMI, 49); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_RBRACE, 50); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_SEMI, 51); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_EOF, 52); if (rc) return rc;

    return 0;
}

int test_string_decl(void) {
    int rc;
    char *src;
    /* char *s = "hello\n"; */
    src = "char *s = \"hello\\n\";";

    lex_init(src, strlen(src));

    lex_next(); rc = check(lex_tok, TK_CHAR, 60); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_STAR, 61); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_IDENT, 62); if (rc) return rc;
    rc = check(strcmp(lex_str, "s"), 0, 63); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_ASSIGN, 64); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_STRING, 65); if (rc) return rc;
    rc = check(lex_slen, 6, 66); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_SEMI, 67); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_EOF, 68); if (rc) return rc;

    return 0;
}

int test_for_loop(void) {
    int rc;
    char *src;
    /* for (i = 0; i < n; i += 1) { sum = sum + a[i]; } */
    src = "for (i = 0; i < n; i += 1) { sum = sum + a[i]; }";

    lex_init(src, strlen(src));

    lex_next(); rc = check(lex_tok, TK_FOR, 70); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_LPAREN, 71); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_IDENT, 72); if (rc) return rc;  /* i */
    lex_next(); rc = check(lex_tok, TK_ASSIGN, 73); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_NUM, 74); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_SEMI, 75); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_IDENT, 76); if (rc) return rc;  /* i */
    lex_next(); rc = check(lex_tok, TK_LT, 77); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_IDENT, 78); if (rc) return rc;  /* n */
    lex_next(); rc = check(lex_tok, TK_SEMI, 79); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_IDENT, 80); if (rc) return rc;  /* i */
    lex_next(); rc = check(lex_tok, TK_PLUSEQ, 81); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_NUM, 82); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_RPAREN, 83); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_LBRACE, 84); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_IDENT, 85); if (rc) return rc;  /* sum */
    lex_next(); rc = check(lex_tok, TK_ASSIGN, 86); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_IDENT, 87); if (rc) return rc;  /* sum */
    lex_next(); rc = check(lex_tok, TK_PLUS, 88); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_IDENT, 89); if (rc) return rc;  /* a */
    lex_next(); rc = check(lex_tok, TK_LBRACK, 90); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_IDENT, 91); if (rc) return rc;  /* i */
    lex_next(); rc = check(lex_tok, TK_RBRACK, 92); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_SEMI, 93); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_RBRACE, 94); if (rc) return rc;
    lex_next(); rc = check(lex_tok, TK_EOF, 95); if (rc) return rc;

    return 0;
}

int main(void) {
    int rc;

    rc = test_return_stmt();
    if (rc) return rc;

    rc = test_if_stmt();
    if (rc) return rc;

    rc = test_struct();
    if (rc) return rc;

    rc = test_string_decl();
    if (rc) return rc;

    rc = test_for_loop();
    if (rc) return rc;

    return 0;
}
