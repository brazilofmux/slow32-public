/* Phase 7 test: static/const, global initializers, goto/labels */

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

/* --- static qualifier tests --- */

static int static_var;

static int static_func(int x) {
    return x + 10;
}

int test_static(void) {
    static_var = 42;
    if (static_var != 42) return 1;
    if (static_func(5) != 15) return 2;
    return 0;
}

/* --- const qualifier test --- */

static const int const_val = 99;

int test_const(void) {
    if (const_val != 99) return 1;
    return 0;
}

/* --- global initializer tests --- */

int g_ten = 10;
int g_neg = -5;
int g_zero = 0;
int g_big = 1000;

int test_global_init(void) {
    if (g_ten != 10) return 1;
    if (g_neg != -5) return 2;
    if (g_zero != 0) return 3;
    if (g_big != 1000) return 4;
    /* Verify we can still modify them */
    g_ten = 20;
    if (g_ten != 20) return 5;
    return 0;
}

/* --- goto forward test --- */

int test_goto_forward(void) {
    int x;
    x = 0;
    goto skip;
    x = 99;
skip:
    if (x != 0) return 1;
    return 0;
}

/* --- goto backward test --- */

int test_goto_backward(void) {
    int count;
    int sum;
    count = 0;
    sum = 0;
again:
    if (count >= 5) goto done;
    sum = sum + count;
    count = count + 1;
    goto again;
done:
    /* sum = 0+1+2+3+4 = 10 */
    if (sum != 10) return 1;
    if (count != 5) return 2;
    return 0;
}

/* --- multiple labels in one function --- */

int test_multi_label(void) {
    int x;
    x = 1;
    goto step1;
step3:
    x = x + 100;
    goto finish;
step1:
    x = x + 10;
    goto step2;
step2:
    x = x + 20;
    goto step3;
finish:
    /* x = 1 + 10 + 20 + 100 = 131 */
    if (x != 131) return 1;
    return 0;
}

/* --- Main --- */
int main(void) {
    int rc;

    rc = test_static();
    if (rc) return rc;

    rc = test_const();
    if (rc) return rc + 10;

    rc = test_global_init();
    if (rc) return rc + 20;

    rc = test_goto_forward();
    if (rc) return rc + 30;

    rc = test_goto_backward();
    if (rc) return rc + 40;

    rc = test_multi_label();
    if (rc) return rc + 50;

    my_puts("Phase 7: all tests passed\n");
    return 0;
}
