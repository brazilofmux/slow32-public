/* Phase 19 test: function-like macros */

int write(int fd, char *buf, int len);

int my_strlen(char *s) {
    int n;
    n = 0;
    while (*(s + n) != 0) {
        n = n + 1;
    }
    return n;
}

int my_puts(char *s) {
    int len;
    len = my_strlen(s);
    write(1, s, len);
    return 0;
}

/* --- Macro definitions --- */

#define DOUBLE(x) ((x)+(x))
#define MAX(a,b) ((a)>(b)?(a):(b))
#define ZERO() 0
#define ADD3(a,b,c) ((a)+(b)+(c))
#define EXPR (10+20)
#define INT_CONST 42
#define IDENTITY(x) x

/* --- Test 1: simple one-param macro --- */
int test_simple(void) {
    int r;
    r = DOUBLE(5);
    if (r != 10) return 1;
    r = DOUBLE(3);
    if (r != 6) return 2;
    return 0;
}

/* --- Test 2: two-param macro --- */
int test_two_param(void) {
    int r;
    r = MAX(3, 7);
    if (r != 7) return 1;
    r = MAX(10, 2);
    if (r != 10) return 2;
    return 0;
}

/* --- Test 3: zero-param macro --- */
int test_zero_param(void) {
    if (ZERO() != 0) return 1;
    return 0;
}

/* --- Test 4: expressions as arguments --- */
int test_expr_args(void) {
    int r;
    r = MAX(1+2, 4*5);
    if (r != 20) return 1;
    r = DOUBLE(3+4);
    if (r != 14) return 2;
    return 0;
}

/* --- Test 5: nested parens in arguments --- */
int test_nested_parens(void) {
    int r;
    r = MAX((3), 2);
    if (r != 3) return 1;
    r = MAX((1+2), (3+4));
    if (r != 7) return 2;
    return 0;
}

/* --- Test 6: nested macro calls --- */
int test_nested_macros(void) {
    int r;
    r = MAX(DOUBLE(3), 5);
    if (r != 6) return 1;
    r = DOUBLE(MAX(2, 4));
    if (r != 8) return 2;
    return 0;
}

/* --- Test 7: three-param macro --- */
int test_three_param(void) {
    int r;
    r = ADD3(1, 2, 3);
    if (r != 6) return 1;
    r = ADD3(10, 20, 30);
    if (r != 60) return 2;
    return 0;
}

/* --- Test 8: object-like text macro --- */
int test_text_macro(void) {
    int r;
    r = EXPR;
    if (r != 30) return 1;
    return 0;
}

/* --- Test 9: integer macros still work --- */
int test_int_macro(void) {
    if (INT_CONST != 42) return 1;
    return 0;
}

/* --- Test 10: function-like macro name without () --- */
int ZERO;

int test_no_invoke(void) {
    ZERO = 99;
    if (ZERO != 99) return 1;
    if (ZERO() != 0) return 2;
    return 0;
}

/* --- Test 11: identity macro passes through expressions --- */
int test_identity(void) {
    int r;
    r = IDENTITY(42);
    if (r != 42) return 1;
    r = IDENTITY(3 + 7);
    if (r != 10) return 2;
    return 0;
}

/* --- Main --- */
int main(void) {
    int rc;

    rc = test_simple();
    if (rc) return rc;

    rc = test_two_param();
    if (rc) return rc + 10;

    rc = test_zero_param();
    if (rc) return rc + 20;

    rc = test_expr_args();
    if (rc) return rc + 30;

    rc = test_nested_parens();
    if (rc) return rc + 40;

    rc = test_nested_macros();
    if (rc) return rc + 50;

    rc = test_three_param();
    if (rc) return rc + 60;

    rc = test_text_macro();
    if (rc) return rc + 70;

    rc = test_int_macro();
    if (rc) return rc + 80;

    rc = test_no_invoke();
    if (rc) return rc + 90;

    rc = test_identity();
    if (rc) return rc + 100;

    my_puts("Phase 19: all tests passed\n");
    return 0;
}
