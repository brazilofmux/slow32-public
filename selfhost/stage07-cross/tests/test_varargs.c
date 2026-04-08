/* test_varargs.c — va_start / va_arg tests for x86-64 cross-compiler
 *
 * Tests variadic functions with int, pointer, and long long args.
 * Expected exit code: 0 (all pass)
 */

typedef char *va_list;

int sum_ints(int count, ...) {
    va_list ap;
    int total;
    int i;
    va_start(ap, count);
    total = 0;
    i = 0;
    while (i < count) {
        total = total + va_arg(ap, int);
        i = i + 1;
    }
    va_end(ap);
    return total;
}

int first_of_three(int a, int b, int c, ...) {
    va_list ap;
    int val;
    va_start(ap, c);
    val = va_arg(ap, int);
    va_end(ap);
    return val;
}

char *get_nth_str(int n, ...) {
    va_list ap;
    char *val;
    int i;
    va_start(ap, n);
    i = 0;
    val = 0;
    while (i < n) {
        val = va_arg(ap, char *);
        i = i + 1;
    }
    va_end(ap);
    return val;
}

long long sum_longs(int count, ...) {
    va_list ap;
    long long total;
    int i;
    va_start(ap, count);
    total = 0;
    i = 0;
    while (i < count) {
        total = total + va_arg(ap, long long);
        i = i + 1;
    }
    va_end(ap);
    return total;
}

int sum_promoted_chars(int count, ...) {
    va_list ap;
    int total;
    int i;
    va_start(ap, count);
    total = 0;
    i = 0;
    while (i < count) {
        total = total + (char)va_arg(ap, int);
        i = i + 1;
    }
    va_end(ap);
    return total;
}

int sum_promoted_shorts(int count, ...) {
    va_list ap;
    int total;
    int i;
    va_start(ap, count);
    total = 0;
    i = 0;
    while (i < count) {
        total = total + (short)va_arg(ap, int);
        i = i + 1;
    }
    va_end(ap);
    return total;
}

int sum_many_ints(int count, ...) {
    va_list ap;
    int total;
    int i;
    va_start(ap, count);
    total = 0;
    i = 0;
    while (i < count) {
        total = total + va_arg(ap, int);
        i = i + 1;
    }
    va_end(ap);
    return total;
}

int stack_only_varargs(int a, int b, int c, int d, int e, int f, ...) {
    va_list ap;
    int total;
    total = 0;
    va_start(ap, f);
    total = total + va_arg(ap, int);
    total = total + va_arg(ap, int);
    va_end(ap);
    return total;
}

int side_fx;

int next_value(void) {
    side_fx = side_fx + 1;
    return side_fx;
}

int take_eight(int a, int b, int c, int d, int e, int f, int g, int h) {
    return g * 10 + h;
}

int main(int argc, char **argv) {
    int fails;
    char *s;
    long long ll;
    fails = 0;

    /* Basic: sum_ints(3, 10, 20, 30) = 60 */
    if (sum_ints(3, 10, 20, 30) != 60) fails = fails + 1;

    /* Single arg: sum_ints(1, 42) = 42 */
    if (sum_ints(1, 42) != 42) fails = fails + 2;

    /* All 6 register slots: sum_ints(5, 1, 2, 3, 4, 5) = 15 */
    if (sum_ints(5, 1, 2, 3, 4, 5) != 15) fails = fails + 4;

    /* Multiple named params: first_of_three(100, 200, 300, 999) = 999 */
    if (first_of_three(100, 200, 300, 999) != 999) fails = fails + 8;

    /* Pointer varargs */
    s = get_nth_str(2, "hello", "world");
    if (s[0] != 'w') fails = fails + 16;

    /* Long long varargs */
    ll = sum_longs(3, (long long)100, (long long)200, (long long)300);
    if (ll != 600) fails = fails + 32;

    /* Default-promotion varargs: char and short travel as int */
    if (sum_promoted_chars(3, (char)1, (char)2, (char)3) != 6) fails = fails + 1024;
    if (sum_promoted_shorts(3, (short)10, (short)20, (short)30) != 60)
        fails = fails + 2048;

    /* Mixed register/stack varargs: 5 unnamed register args + 2 stack args */
    if (sum_many_ints(7, 1, 2, 3, 4, 5, 6, 7) != 28) fails = fails + 64;

    /* Six named params: variadics start directly on the caller stack */
    if (stack_only_varargs(10, 20, 30, 40, 50, 60, 70, 80) != 150)
        fails = fails + 128;

    /* 7+ arg calls must not re-evaluate stack arguments while marshalling */
    side_fx = 0;
    if (take_eight(100, 100, 100, 100, 100, 100, next_value(), next_value()) != 12)
        fails = fails + 256;
    if (side_fx != 2) fails = fails + 512;

    return fails;
}
