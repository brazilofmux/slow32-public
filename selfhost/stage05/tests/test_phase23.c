/* test_phase23.c -- __VA_ARGS__ variadic macros */
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

/* Simple variadic: just forwards args */
#define ADD3(a, b, c) ((a) + (b) + (c))
#define VA_ADD3(...) ADD3(__VA_ARGS__)

/* Variadic with fixed + variable args */
#define FMT(prefix, ...) prefix + VA_ADD3(__VA_ARGS__)

/* Zero variadic args */
#define JUST_FIXED(x, ...) (x)

/* Single variadic arg */
#define WRAP(tag, ...) (tag) + (__VA_ARGS__)

int sum3(int a, int b, int c) {
    return a + b + c;
}

/* Macro that calls a function with variadic args */
#define CALL_SUM3(...) sum3(__VA_ARGS__)

struct Point { int x; int y; };
struct Big { int a; int b; int c; int d; };

int main(void) {
    int r;

    failed = 0;

    /* Test 1: __VA_ARGS__ with three args */
    r = VA_ADD3(10, 20, 30);
    check(r == 60, "VA_ADD3(10, 20, 30)");

    /* Test 2: mixed fixed + variadic */
    r = FMT(100, 1, 2, 3);
    check(r == 106, "FMT(100, 1, 2, 3)");

    /* Test 3: variadic with single arg */
    r = WRAP(10, 5);
    check(r == 15, "WRAP(10, 5)");

    /* Test 4: function call via variadic macro */
    r = CALL_SUM3(100, 200, 300);
    check(r == 600, "CALL_SUM3(100, 200, 300)");

    /* Test 5: nested variadic + function-like */
    r = VA_ADD3(1, 2, 3) + CALL_SUM3(4, 5, 6);
    check(r == 21, "nested VA_ADD3 + CALL_SUM3");

    /* --- Struct assignment tests --- */
    {
        struct Point p1;
        struct Point p2;
        p1.x = 10;
        p1.y = 20;
        p2 = p1;
        check(p2.x == 10, "struct assign p2.x");
        check(p2.y == 20, "struct assign p2.y");

        /* Modify p1, verify p2 is independent copy */
        p1.x = 99;
        check(p2.x == 10, "struct copy independent");
    }
    {
        struct Big s1;
        struct Big s2;
        s1.a = 1;
        s1.b = 2;
        s1.c = 3;
        s1.d = 4;
        s2 = s1;
        check(s2.a == 1, "big struct a");
        check(s2.b == 2, "big struct b");
        check(s2.c == 3, "big struct c");
        check(s2.d == 4, "big struct d");
    }

    if (failed == 0) {
        my_puts("test_phase23: ALL PASSED\n");
    }
    return failed;
}
