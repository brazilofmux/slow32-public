/* lex_test.c -- Test harness for Ragel -G2 C lexer (stage04)
 *
 * Tokenizes embedded C source strings and checks token sequences.
 * Returns 0 on success, non-zero on first failure.
 *
 * Build: merge c_lexer_gen.c + this file, compile with s32-cc.
 * Uses fd-based I/O functions (fdputs, fdputc, fdputuint).
 * I/O wrappers so the libc's FILE*-based fputs is not called.
 */

#include "c_lexer_gen.c"

/* fd-based I/O provided by libc (stdio.c) */

static int test_pass;
static int test_fail;
static int test_total;

static void check_tok(int expect, char *name) {
    test_total = test_total + 1;
    if (lex_tok != expect) {
        fdputs("FAIL: expected ", 2);
        fdputs(name, 2);
        fdputs(" (", 2);
        fdputuint(2, expect);
        fdputs(") got ", 2);
        fdputuint(2, lex_tok);
        fdputc(10, 2);
        test_fail = test_fail + 1;
    } else {
        test_pass = test_pass + 1;
    }
}

static void check_val(int expect) {
    if (lex_val != expect) {
        fdputs("FAIL: expected val=", 2);
        fdputuint(2, expect);
        fdputs(" got ", 2);
        fdputuint(2, lex_val);
        fdputc(10, 2);
        test_fail = test_fail + 1;
        test_pass = test_pass - 1;  /* undo the pass from check_tok */
    }
}

static void check_str(char *expect) {
    if (strcmp(lex_str, expect) != 0) {
        fdputs("FAIL: expected str='", 2);
        fdputs(expect, 2);
        fdputs("' got '", 2);
        fdputs(lex_str, 2);
        fdputs("'\n", 2);
        test_fail = test_fail + 1;
        test_pass = test_pass - 1;
    }
}

/* Test 1: Basic declaration */
static int test_basic(void) {
    char *src;
    src = "int x = 42;";
    lex_init(src, strlen(src));
    lex_next(); check_tok(TK_INT, "INT");
    lex_next(); check_tok(TK_IDENT, "IDENT"); check_str("x");
    lex_next(); check_tok(TK_ASSIGN, "ASSIGN");
    lex_next(); check_tok(TK_NUM, "NUM"); check_val(42);
    lex_next(); check_tok(TK_SEMI, "SEMI");
    lex_next(); check_tok(TK_EOF, "EOF");
    return 0;
}

/* Test 2: Operators */
static int test_ops(void) {
    char *src;
    src = "a + b - c * d / e % f";
    lex_init(src, strlen(src));
    lex_next(); check_tok(TK_IDENT, "a");
    lex_next(); check_tok(TK_PLUS, "PLUS");
    lex_next(); check_tok(TK_IDENT, "b");
    lex_next(); check_tok(TK_MINUS, "MINUS");
    lex_next(); check_tok(TK_IDENT, "c");
    lex_next(); check_tok(TK_STAR, "STAR");
    lex_next(); check_tok(TK_IDENT, "d");
    lex_next(); check_tok(TK_SLASH, "SLASH");
    lex_next(); check_tok(TK_IDENT, "e");
    lex_next(); check_tok(TK_PERCENT, "PERCENT");
    lex_next(); check_tok(TK_IDENT, "f");
    lex_next(); check_tok(TK_EOF, "EOF");
    return 0;
}

/* Test 3: Compound operators */
static int test_compound_ops(void) {
    char *src;
    src = "+= -= *= /= %= &= |= ^= <<= >>= == != <= >= << >> && || ++ -- ->";
    lex_init(src, strlen(src));
    lex_next(); check_tok(TK_PLUSEQ, "PLUSEQ");
    lex_next(); check_tok(TK_MINUSEQ, "MINUSEQ");
    lex_next(); check_tok(TK_STAREQ, "STAREQ");
    lex_next(); check_tok(TK_SLASHEQ, "SLASHEQ");
    lex_next(); check_tok(TK_PERCENTEQ, "PERCENTEQ");
    lex_next(); check_tok(TK_AMPEQ, "AMPEQ");
    lex_next(); check_tok(TK_PIPEEQ, "PIPEEQ");
    lex_next(); check_tok(TK_CARETEQ, "CARETEQ");
    lex_next(); check_tok(TK_LSHIFTEQ, "LSHIFTEQ");
    lex_next(); check_tok(TK_RSHIFTEQ, "RSHIFTEQ");
    lex_next(); check_tok(TK_EQ, "EQ");
    lex_next(); check_tok(TK_NE, "NE");
    lex_next(); check_tok(TK_LE, "LE");
    lex_next(); check_tok(TK_GE, "GE");
    lex_next(); check_tok(TK_LSHIFT, "LSHIFT");
    lex_next(); check_tok(TK_RSHIFT, "RSHIFT");
    lex_next(); check_tok(TK_LAND, "LAND");
    lex_next(); check_tok(TK_LOR, "LOR");
    lex_next(); check_tok(TK_INC, "INC");
    lex_next(); check_tok(TK_DEC, "DEC");
    lex_next(); check_tok(TK_ARROW, "ARROW");
    lex_next(); check_tok(TK_EOF, "EOF");
    return 0;
}

/* Test 4: Numbers */
static int test_numbers(void) {
    char *src;
    src = "0 42 0xFF 0377 100";
    lex_init(src, strlen(src));
    lex_next(); check_tok(TK_NUM, "zero"); check_val(0);
    lex_next(); check_tok(TK_NUM, "42"); check_val(42);
    lex_next(); check_tok(TK_NUM, "0xFF"); check_val(255);
    lex_next(); check_tok(TK_NUM, "0377"); check_val(255);
    lex_next(); check_tok(TK_NUM, "100"); check_val(100);
    lex_next(); check_tok(TK_EOF, "EOF");
    return 0;
}

/* Test 5: Keywords */
static int test_keywords(void) {
    char *src;
    src = "if else while for return int char void struct typedef enum sizeof static extern switch case break continue default do goto const";
    lex_init(src, strlen(src));
    lex_next(); check_tok(TK_IF, "IF");
    lex_next(); check_tok(TK_ELSE, "ELSE");
    lex_next(); check_tok(TK_WHILE, "WHILE");
    lex_next(); check_tok(TK_FOR, "FOR");
    lex_next(); check_tok(TK_RETURN, "RETURN");
    lex_next(); check_tok(TK_INT, "INT");
    lex_next(); check_tok(TK_CHAR, "CHAR");
    lex_next(); check_tok(TK_VOID, "VOID");
    lex_next(); check_tok(TK_STRUCT, "STRUCT");
    lex_next(); check_tok(TK_TYPEDEF, "TYPEDEF");
    lex_next(); check_tok(TK_ENUM, "ENUM");
    lex_next(); check_tok(TK_SIZEOF, "SIZEOF");
    lex_next(); check_tok(TK_STATIC, "STATIC");
    lex_next(); check_tok(TK_EXTERN, "EXTERN");
    lex_next(); check_tok(TK_SWITCH, "SWITCH");
    lex_next(); check_tok(TK_CASE, "CASE");
    lex_next(); check_tok(TK_BREAK, "BREAK");
    lex_next(); check_tok(TK_CONTINUE, "CONTINUE");
    lex_next(); check_tok(TK_DEFAULT, "DEFAULT");
    lex_next(); check_tok(TK_DO, "DO");
    lex_next(); check_tok(TK_GOTO, "GOTO");
    lex_next(); check_tok(TK_CONST, "CONST");
    lex_next(); check_tok(TK_EOF, "EOF");
    return 0;
}

/* Test 6: String literal */
static int test_string(void) {
    char *src;
    src = "\"hello world\"";
    lex_init(src, strlen(src));
    lex_next(); check_tok(TK_STRING, "STRING"); check_str("hello world");
    lex_next(); check_tok(TK_EOF, "EOF");
    return 0;
}

/* Test 7: Char literal */
static int test_char(void) {
    char *src;
    src = "'A' '\\n' '\\0'";
    lex_init(src, strlen(src));
    lex_next(); check_tok(TK_CHARLIT, "CHARLIT"); check_val(65);
    lex_next(); check_tok(TK_CHARLIT, "CHARLIT_n"); check_val(10);
    lex_next(); check_tok(TK_CHARLIT, "CHARLIT_0"); check_val(0);
    lex_next(); check_tok(TK_EOF, "EOF");
    return 0;
}

/* Test 8: Comments */
static int test_comments(void) {
    char *src;
    src = "a /* block\ncomment */ b // line comment\nc";
    lex_init(src, strlen(src));
    lex_next(); check_tok(TK_IDENT, "a"); check_str("a");
    lex_next(); check_tok(TK_IDENT, "b"); check_str("b");
    lex_next(); check_tok(TK_IDENT, "c"); check_str("c");
    lex_next(); check_tok(TK_EOF, "EOF");
    return 0;
}

/* Test 9: Line counting */
static int test_lines(void) {
    char *src;
    int line_at_b;
    src = "a\n\nb\n";
    lex_init(src, strlen(src));
    lex_next(); /* a on line 1 */
    lex_next(); /* b on line 3 */
    line_at_b = lex_line;
    if (line_at_b != 3) {
        fdputs("FAIL: expected line=3 at 'b', got ", 2);
        fdputuint(2, line_at_b);
        fdputc(10, 2);
        test_total = test_total + 1;
        test_fail = test_fail + 1;
    } else {
        test_total = test_total + 1;
        test_pass = test_pass + 1;
    }
    return 0;
}

/* Test 10: Punctuation mix */
static int test_punct(void) {
    char *src;
    src = "({[]});,. ...?:~#";
    lex_init(src, strlen(src));
    lex_next(); check_tok(TK_LPAREN, "LPAREN");
    lex_next(); check_tok(TK_LBRACE, "LBRACE");
    lex_next(); check_tok(TK_LBRACK, "LBRACK");
    lex_next(); check_tok(TK_RBRACK, "RBRACK");
    lex_next(); check_tok(TK_RBRACE, "RBRACE");
    lex_next(); check_tok(TK_RPAREN, "RPAREN");
    lex_next(); check_tok(TK_SEMI, "SEMI");
    lex_next(); check_tok(TK_COMMA, "COMMA");
    lex_next(); check_tok(TK_DOT, "DOT");
    lex_next(); check_tok(TK_ELLIPSIS, "ELLIPSIS");
    lex_next(); check_tok(TK_QMARK, "QMARK");
    lex_next(); check_tok(TK_COLON, "COLON");
    lex_next(); check_tok(TK_TILDE, "TILDE");
    lex_next(); check_tok(TK_HASH, "HASH");
    lex_next(); check_tok(TK_EOF, "EOF");
    return 0;
}

int main(void) {
    test_pass = 0;
    test_fail = 0;
    test_total = 0;

    test_basic();
    test_ops();
    test_compound_ops();
    test_numbers();
    test_keywords();
    test_string();
    test_char();
    test_comments();
    test_lines();
    test_punct();

    fdputs("Lexer tests: ", 2);
    fdputuint(2, test_pass);
    fdputc(47, 2);  /* / */
    fdputuint(2, test_total);
    fdputs(" passed", 2);
    if (test_fail > 0) {
        fdputs(", ", 2);
        fdputuint(2, test_fail);
        fdputs(" failed", 2);
    }
    fdputc(10, 2);

    if (test_fail > 0) return 1;
    return 0;
}
