/* Phase 9 test: function pointers */

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

/* --- Helper functions --- */

int add_one(int x) { return x + 1; }
int double_it(int x) { return x * 2; }
int add(int a, int b) { return a + b; }
int sub(int a, int b) { return a - b; }
int mul(int a, int b) { return a * b; }

/* --- Test 1: basic function pointer assign and call --- */

int test_basic_fp(void) {
    int (*fp)(int);
    fp = add_one;
    if (fp(41) != 42) return 1;
    fp = double_it;
    if (fp(21) != 42) return 2;
    return 0;
}

/* --- Test 2: function pointer with multiple args --- */

int test_multi_arg_fp(void) {
    int (*fp)(int, int);
    fp = add;
    if (fp(10, 20) != 30) return 1;
    fp = sub;
    if (fp(50, 8) != 42) return 2;
    fp = mul;
    if (fp(6, 7) != 42) return 3;
    return 0;
}

/* --- Test 3: function pointer as parameter --- */

int apply(int (*f)(int), int x) {
    return f(x);
}

int test_fp_param(void) {
    if (apply(add_one, 9) != 10) return 1;
    if (apply(double_it, 5) != 10) return 2;
    return 0;
}

/* --- Test 4: function pointer via typedef --- */

typedef int (*UnaryOp)(int);
typedef int (*BinaryOp)(int, int);

int test_fp_typedef(void) {
    UnaryOp u;
    BinaryOp b;
    u = double_it;
    b = add;
    if (u(21) != 42) return 1;
    if (b(20, 22) != 42) return 2;
    return 0;
}

/* --- Test 5: reassign function pointer --- */

int test_reassign_fp(void) {
    int (*fp)(int, int);
    int r;
    fp = add;
    r = fp(10, 20);
    if (r != 30) return 1;
    fp = mul;
    r = fp(5, 6);
    if (r != 30) return 2;
    return 0;
}

/* --- Test 6: function pointer stored in global --- */

int (*g_op)(int, int);

int test_global_fp(void) {
    g_op = add;
    if (g_op(3, 4) != 7) return 1;
    g_op = mul;
    if (g_op(3, 4) != 12) return 2;
    return 0;
}

/* --- Main --- */
int main(void) {
    int rc;

    rc = test_basic_fp();
    if (rc) return rc;

    rc = test_multi_arg_fp();
    if (rc) return rc + 10;

    rc = test_fp_param();
    if (rc) return rc + 20;

    rc = test_fp_typedef();
    if (rc) return rc + 30;

    rc = test_reassign_fp();
    if (rc) return rc + 40;

    rc = test_global_fp();
    if (rc) return rc + 50;

    my_puts("Phase 9: all tests passed\n");
    return 0;
}
