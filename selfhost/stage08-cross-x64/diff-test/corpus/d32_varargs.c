/* Callee-side variadic functions -- cc-a64 implementation of
 * HI_VA_START / HI_VA_ARG / HI_VA_NEXT in hir_codegen_a64.h
 * (closes ISSUES.md #48).  Exercises:
 *   - 1, 5, 7, and 8 variadic int args (boundary at register / stack
 *     spill — AAPCS64 has 8 GP arg slots, so >= 8 args spill).
 *   - long long varargs.
 *   - pointer varargs.
 *   - 9 named args + varargs (named param spills to stack too).
 *
 * Uses <stdarg.h> for gcc compatibility; cc-x64 / cc-a64 pick up the
 * shim header that just typedefs va_list = char *. */
#include <stdarg.h>

static int sumv(int n, ...) {
    va_list ap;
    int s, i;
    va_start(ap, n);
    s = 0;
    for (i = 0; i < n; i = i + 1) {
        s = s + va_arg(ap, int);
    }
    va_end(ap);
    return s;
}

static long long sum_ll(int n, ...) {
    va_list ap;
    long long s;
    int i;
    va_start(ap, n);
    s = 0;
    for (i = 0; i < n; i = i + 1) {
        s = s + va_arg(ap, long long);
    }
    va_end(ap);
    return s;
}

static int sum_via_ptrs(int n, ...) {
    va_list ap;
    int s, i;
    int *p;
    va_start(ap, n);
    s = 0;
    for (i = 0; i < n; i = i + 1) {
        p = va_arg(ap, int *);
        s = s + *p;
    }
    va_end(ap);
    return s;
}

static int many(int a, int b, int c, int d, int e, int f, int g, int h, int i, ...) {
    va_list ap;
    int s, j;
    va_start(ap, i);
    s = a + b + c + d + e + f + g + h + i;
    for (j = 0; j < 4; j = j + 1) {
        s = s + va_arg(ap, int);
    }
    va_end(ap);
    return s;
}

int main(void) {
    int x = 7, y = 13, z = 19, w = 23;
    int t1 = sumv(1, 42);
    int t2 = sumv(5, 1, 2, 3, 4, 5);
    int t3 = sumv(7, 10, 20, 30, 40, 50, 60, 70);    /* fills all 7 reg slots */
    int t4 = sumv(8, 100, 200, 300, 400, 500, 600, 700, 800);  /* spills 1 to stack */
    int t5 = sumv(12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12);  /* spills 5 to stack */
    long long ll = sum_ll(4, 100000000000LL, 200000000000LL, 300LL, 400LL);
    int p = sum_via_ptrs(4, &x, &y, &z, &w);
    int m = many(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40);

    int acc = 0;
    acc = acc * 17 + t1;
    acc = acc * 17 + t2;
    acc = acc * 17 + t3;
    acc = acc * 17 + t4;
    acc = acc * 17 + t5;
    acc = acc * 17 + (int)(ll & 0xFFFF);
    acc = acc * 17 + (int)((ll >> 32) & 0xFFFF);
    acc = acc * 17 + p;
    acc = acc * 17 + m;
    return acc & 0xff;
}
