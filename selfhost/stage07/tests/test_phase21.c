/* test_phase21.c -- 64-bit integer (long long) tests */
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

int test_count;
int fail_count;

void check(int cond, char *msg) {
    test_count = test_count + 1;
    if (!cond) {
        fail_count = fail_count + 1;
        my_puts("FAIL: ");
        my_puts(msg);
        my_puts("\n");
    }
}

/* Helper: check lo and hi words of a 64-bit value */
void check64(long long val, int expect_lo, int expect_hi, char *msg) {
    int lo;
    int hi;
    lo = (int)val;
    hi = (int)(val >> 32);
    test_count = test_count + 1;
    if (lo != expect_lo || hi != expect_hi) {
        fail_count = fail_count + 1;
        my_puts("FAIL: ");
        my_puts(msg);
        my_puts("\n");
    }
}

long long add64(long long a, long long b) {
    return a + b;
}

long long sub64(long long a, long long b) {
    return a - b;
}

long long mul64(long long a, long long b) {
    return a * b;
}

long long div64(long long a, long long b) {
    return a / b;
}

long long mod64(long long a, long long b) {
    return a % b;
}

long long shl64(long long a, int n) {
    return a << n;
}

long long shr64(long long a, int n) {
    return a >> n;
}

long long and64(long long a, long long b) {
    return a & b;
}

long long or64(long long a, long long b) {
    return a | b;
}

long long xor64(long long a, long long b) {
    return a ^ b;
}

long long neg64(long long a) {
    return -a;
}

long long not64(long long a) {
    return ~a;
}

long long identity64(long long x) {
    return x;
}

/* Global 64-bit variable */
long long g_val;
long long g_init = 42;

int main(void) {
    long long a;
    long long b;
    long long c;
    int lo;
    int hi;

    test_count = 0;
    fail_count = 0;

    /* --- Basic assignment and return --- */
    a = 0;
    check64(a, 0, 0, "zero");

    a = 42;
    check64(a, 42, 0, "small positive");

    a = -1;
    check64(a, -1, -1, "minus one");

    a = -100;
    check64(a, -100, -1, "negative");

    /* --- Function call with 64-bit param/return --- */
    a = 100;
    b = identity64(a);
    check64(b, 100, 0, "identity64");

    /* --- 64-bit addition --- */
    a = 10;
    b = 20;
    c = add64(a, b);
    check64(c, 30, 0, "add small");

    /* Carry propagation: -1 + 2 = 1 */
    a = -1;
    b = 2;
    c = add64(a, b);
    check64(c, 1, 0, "add carry (-1 + 2 = 1)");

    /* --- 64-bit subtraction --- */
    a = 30;
    b = 10;
    c = sub64(a, b);
    check64(c, 20, 0, "sub small");

    a = 0;
    b = 1;
    c = sub64(a, b);
    check64(c, -1, -1, "sub borrow (0 - 1 = -1)");

    /* --- 64-bit bitwise --- */
    a = -1;  /* all bits set */
    b = 0;
    c = and64(a, b);
    check64(c, 0, 0, "and with zero");

    a = -1;
    b = -1;
    c = and64(a, b);
    check64(c, -1, -1, "and all ones");

    a = 0;
    b = -1;
    c = or64(a, b);
    check64(c, -1, -1, "or with all ones");

    a = -1;
    b = -1;
    c = xor64(a, b);
    check64(c, 0, 0, "xor same");

    /* --- 64-bit negate --- */
    a = 1;
    c = neg64(a);
    check64(c, -1, -1, "negate 1");

    a = -1;
    c = neg64(a);
    check64(c, 1, 0, "negate -1");

    a = 0;
    c = neg64(a);
    check64(c, 0, 0, "negate 0");

    /* --- 64-bit bitwise NOT --- */
    a = 0;
    c = not64(a);
    check64(c, -1, -1, "not 0");

    a = -1;
    c = not64(a);
    check64(c, 0, 0, "not -1");

    /* --- 64-bit left shift --- */
    a = 1;
    c = shl64(a, 0);
    check64(c, 1, 0, "shl 0");

    a = 1;
    c = shl64(a, 1);
    check64(c, 2, 0, "shl 1");

    a = 1;
    c = shl64(a, 31);
    /* 1 << 31 = 0x80000000 in lo, 0 in hi */
    lo = (int)c;
    hi = (int)(c >> 32);
    check(lo == (int)(1 << 31), "shl 31 lo");
    check(hi == 0, "shl 31 hi");

    a = 1;
    c = shl64(a, 32);
    check64(c, 0, 1, "shl 32");

    a = 1;
    c = shl64(a, 33);
    check64(c, 0, 2, "shl 33");

    /* --- 64-bit right shift (arithmetic) --- */
    a = -1;
    c = shr64(a, 1);
    check64(c, -1, -1, "ashr -1 by 1");

    a = 4;
    c = shr64(a, 1);
    check64(c, 2, 0, "ashr 4 by 1");

    /* --- 64-bit comparisons --- */
    a = 10;
    b = 20;
    check(a < b, "lt: 10 < 20");
    check(!(b < a), "lt: !(20 < 10)");
    check(a <= b, "le: 10 <= 20");
    check(a <= a, "le: 10 <= 10");
    check(b > a, "gt: 20 > 10");
    check(a >= a, "ge: 10 >= 10");
    check(a == a, "eq: 10 == 10");
    check(a != b, "ne: 10 != 20");

    /* Comparison with negative */
    a = -1;
    b = 1;
    check(a < b, "lt: -1 < 1");
    check(!(b < a), "lt: !(1 < -1)");

    /* --- 64-bit multiply --- */
    a = 6;
    b = 7;
    c = mul64(a, b);
    check64(c, 42, 0, "mul 6*7");

    a = -3;
    b = 5;
    c = mul64(a, b);
    check64(c, -15, -1, "mul -3*5");

    a = 100;
    b = 100;
    c = mul64(a, b);
    check64(c, 10000, 0, "mul 100*100");

    /* --- 64-bit divide --- */
    a = 42;
    b = 6;
    c = div64(a, b);
    check64(c, 7, 0, "div 42/6");

    a = -42;
    b = 6;
    c = div64(a, b);
    check64(c, -7, -1, "div -42/6");

    a = 100;
    b = 3;
    c = div64(a, b);
    check64(c, 33, 0, "div 100/3");

    /* --- 64-bit modulo --- */
    a = 100;
    b = 3;
    c = mod64(a, b);
    check64(c, 1, 0, "mod 100%3");

    a = 42;
    b = 7;
    c = mod64(a, b);
    check64(c, 0, 0, "mod 42%7");

    a = -7;
    b = 3;
    c = mod64(a, b);
    check64(c, -1, -1, "mod -7%3");

    /* --- Mixed 32/64 promotion --- */
    a = 10;
    c = a + 5;
    check64(c, 15, 0, "mixed add 10+5");

    /* --- Global 64-bit variables --- */
    g_val = 99;
    check64(g_val, 99, 0, "global assign");

    check64(g_init, 42, 0, "global init");

    /* --- Summary --- */
    if (fail_count == 0) {
        my_puts("test_phase21: all tests passed\n");
    } else {
        my_puts("test_phase21: FAILURES\n");
    }
    return fail_count;
}
