#include "../c_lexer_gen.c"

int main(void) {
    char *src;

    src = "puts(\"auto\");";
    lex_init(src, 13);

    lex_next();
    if (lex_tok != TK_IDENT) return 1;
    lex_next();
    if (lex_tok != TK_LPAREN) return 2;
    lex_next();
    if (lex_tok != TK_STRING) return 3;
    if (lex_slen != 4) return 4;
    if (lex_str[0] != 97) return 5;
    if (lex_str[1] != 117) return 6;
    if (lex_str[2] != 116) return 7;
    if (lex_str[3] != 111) return 8;
    lex_next();
    if (lex_tok != TK_RPAREN) return 9;
    lex_next();
    if (lex_tok != TK_SEMI) return 10;
    lex_next();
    if (lex_tok != TK_EOF) return 11;
    return 0;
}
