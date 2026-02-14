/* test8.c — advanced features */
int putchar(int c);

void print_int(int n) {
    if (n < 0) { putchar(45); n = 0 - n; }
    if (n >= 10) print_int(n / 10);
    putchar(48 + n % 10);
}

void print_nl() { putchar(10); }

/* Test compound assignments */
int test_compound() {
    int x = 10;
    x += 5;
    x -= 3;
    x *= 2;
    x /= 4;
    x %= 5;
    return x;
}

/* Test ternary operator */
int max(int a, int b) {
    return a > b ? a : b;
}

/* Test pre/post increment */
int test_incr() {
    int x = 5;
    ++x;
    int a = x;    /* a = 6 */
    x++;
    int b = x;    /* b = 7 */
    return a * 10 + b;   /* 67 */
}

/* Global initializer */
int global_val = 42;

/* Test bitwise compound assignments */
int test_bitwise() {
    int x = 255;
    x &= 15;     /* 15 */
    x |= 48;     /* 63 */
    x ^= 5;      /* 58 */
    return x;
}

/* Function pointer test */
int add(int a, int b) { return a + b; }
int mul(int a, int b) { return a * b; }

int apply(int (*fn)(int, int), int a, int b) {
    return fn(a, b);
}

int main() {
    print_int(test_compound());    /* 1 */
    putchar(32);
    print_int(max(3, 7));          /* 7 */
    putchar(32);
    print_int(max(9, 4));          /* 9 */
    putchar(32);
    print_int(test_incr());        /* 67 */
    putchar(32);
    print_int(global_val);         /* 42 */
    putchar(32);
    print_int(test_bitwise());     /* 58 */
    putchar(32);
    print_int(apply(add, 3, 4));   /* 7 */
    putchar(32);
    print_int(apply(mul, 3, 4));   /* 12 */
    print_nl();
    return 0;
}
