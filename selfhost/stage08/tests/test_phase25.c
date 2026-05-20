/* test_phase25.c -- void* pointer arithmetic + f64 double support */
int write(int fd, char *buf, int len);

int my_strlen(char *s) {
    int n;
    n = 0;
    while (*(s + n) != 0) {
        n = n + 1;
    }
    return n;
}

void my_puts(char *s) {
    write(1, s, my_strlen(s));
}

int failed;

void check(int cond, char *msg) {
    if (!cond) {
        my_puts("FAIL: ");
        my_puts(msg);
        my_puts("\n");
        failed = failed + 1;
    }
}

/* --- void* tests --- */

void *my_memcpy(void *dst, void *src, int n) {
    int i;
    i = 0;
    while (i < n) {
        *((char *)dst + i) = *((char *)src + i);
        i = i + 1;
    }
    return dst;
}

void test_void_ptr(void) {
    char buf[16];
    char *src;
    void *vp;
    int i;

    src = "Hello";
    /* void* arithmetic should scale by 1 (like char*) */
    vp = (void *)buf;
    i = 0;
    while (i < 6) {
        *((char *)vp + i) = *(src + i);
        i = i + 1;
    }
    check(buf[0] == 'H', "void* byte copy: buf[0]=='H'");
    check(buf[4] == 'o', "void* byte copy: buf[4]=='o'");
    check(buf[5] == 0, "void* byte copy: buf[5]==0");

    /* void* memcpy-like function */
    my_memcpy(buf, "World", 6);
    check(buf[0] == 'W', "void* memcpy: buf[0]=='W'");
    check(buf[4] == 'd', "void* memcpy: buf[4]=='d'");
}

/* --- Multiple declarators per line --- */

void test_multi_decl(void) {
    int a, b, c;
    a = 10;
    b = 20;
    c = a + b;
    check(c == 30, "multi-decl: int a, b, c");

    int *p, *q;
    p = &a;
    q = &b;
    check(*p == 10, "multi-decl: int *p, *q (p)");
    check(*q == 20, "multi-decl: int *p, *q (q)");

    int x, y;
    x = 100;
    y = 200;
    check(x + y == 300, "multi-decl: int x, y with values");
}

/* --- f64 double tests --- */

void test_double_basic(void) {
    double a;
    double b;
    double c;
    int i;

    /* Test 1: double constant and cast to int */
    a = 3.14;
    i = (int)a;
    check(i == 3, "double to int: (int)3.14 == 3");

    /* Test 2: double arithmetic - add */
    a = 10.0;
    b = 3.0;
    c = a + b;
    i = (int)c;
    check(i == 13, "double add: 10.0 + 3.0 == 13");

    /* Test 3: double subtraction */
    c = a - b;
    i = (int)c;
    check(i == 7, "double sub: 10.0 - 3.0 == 7");

    /* Test 4: double multiplication */
    a = 4.0;
    b = 5.0;
    c = a * b;
    i = (int)c;
    check(i == 20, "double mul: 4.0 * 5.0 == 20");

    /* Test 5: double division */
    a = 20.0;
    b = 4.0;
    c = a / b;
    i = (int)c;
    check(i == 5, "double div: 20.0 / 4.0 == 5");

    /* Test 6: int to double cast */
    i = 42;
    a = (double)i;
    i = (int)a;
    check(i == 42, "int to double to int: 42");
}

void test_double_cmp(void) {
    double a;
    double b;

    /* Test 7: less than */
    a = 1.0;
    b = 2.0;
    check(a < b, "double cmp: 1.0 < 2.0");

    /* Test 8: greater than */
    check(b > a, "double cmp: 2.0 > 1.0");

    /* Test 9: equal */
    a = 5.0;
    b = 5.0;
    check(a == b, "double cmp: 5.0 == 5.0");

    /* Test 10: not equal */
    b = 6.0;
    check(a != b, "double cmp: 5.0 != 6.0");

    /* Test 11: less-equal */
    a = 3.0;
    b = 3.0;
    check(a <= b, "double cmp: 3.0 <= 3.0");
    b = 4.0;
    check(a <= b, "double cmp: 3.0 <= 4.0");

    /* Test 12: greater-equal */
    a = 5.0;
    b = 5.0;
    check(a >= b, "double cmp: 5.0 >= 5.0");
    b = 4.0;
    check(a >= b, "double cmp: 5.0 >= 4.0");
}

void test_double_negate(void) {
    double a;
    int i;

    /* Test 13: negation */
    a = 7.0;
    a = -a;
    i = (int)a;
    check(i == -7, "double negate: -(7.0) == -7");
}

void test_double_cast(void) {
    float f;
    double d;
    int i;

    /* Test 14: float → double */
    f = 2.5f;
    d = (double)f;
    i = (int)d;
    check(i == 2, "float to double: (double)2.5f == 2");

    /* Test 15: double → float */
    d = 7.0;
    f = (float)d;
    i = (int)f;
    check(i == 7, "double to float: (float)7.0 == 7");

    /* Test 16: negative double cast */
    d = -3.0;
    i = (int)d;
    check(i == -3, "double cast: (int)-3.0 == -3");
}

int main(void) {
    failed = 0;

    test_void_ptr();
    test_multi_decl();
    test_double_basic();
    test_double_cmp();
    test_double_negate();
    test_double_cast();

    if (failed == 0) {
        my_puts("test_phase25: ALL PASSED\n");
    }
    return failed;
}
