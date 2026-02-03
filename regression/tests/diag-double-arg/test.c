/*
 * Diagnostic: Double passed as regular (non-varargs) function argument
 *
 * dtoa's signature is: char *dtoa(double d, int mode, int ndigits, ...)
 * The double is the FIRST parameter. The test checks whether the
 * caller and callee agree on which registers hold the two halves.
 *
 * Also tests the pattern used by dump() in feature-dtoa-basic:
 *   void dump(const char *tag, double val, int mode, int ndigits)
 * where the double is the SECOND parameter (after a pointer).
 */
#include <stdio.h>

/* Pattern 1: double as first arg (like dtoa) */
void __attribute__((noinline)) recv_first(double d, int a, int b) {
    union { double d; unsigned int w[2]; } u;
    u.d = d;
    printf("f1: w0=%08x w1=%08x a=%d b=%d\n", u.w[0], u.w[1], a, b);
}

/* Pattern 2: double as second arg after pointer (like dump) */
void __attribute__((noinline)) recv_second(const char *tag, double d, int a) {
    union { double d; unsigned int w[2]; } u;
    u.d = d;
    printf("f2: tag=%s w0=%08x w1=%08x a=%d\n", tag, u.w[0], u.w[1], a);
}

/* Pattern 3: double as second arg after int */
void __attribute__((noinline)) recv_after_int(int x, double d, int y) {
    union { double d; unsigned int w[2]; } u;
    u.d = d;
    printf("f3: x=%d w0=%08x w1=%08x y=%d\n", x, u.w[0], u.w[1], y);
}

/* Pattern 4: two doubles */
void __attribute__((noinline)) recv_two_doubles(double a, double b) {
    union { double d; unsigned int w[2]; } ua, ub;
    ua.d = a;
    ub.d = b;
    printf("f4: a0=%08x a1=%08x b0=%08x b1=%08x\n",
           ua.w[0], ua.w[1], ub.w[0], ub.w[1]);
}

/* Pattern 5: dtoa-like signature */
void __attribute__((noinline)) dtoa_like(double d, int mode, int ndigits,
                                         int *decpt, int *sign) {
    union { double dd; unsigned int w[2]; } u;
    u.dd = d;
    *decpt = (int)(u.w[1] >> 20) & 0x7FF;  /* extract biased exponent */
    *sign = (u.w[1] >> 31) & 1;
    printf("f5: w0=%08x w1=%08x mode=%d ndig=%d exp=%d sign=%d\n",
           u.w[0], u.w[1], mode, ndigits, *decpt, *sign);
}

/* Pattern 6: chain - receive double, pass to another function */
void __attribute__((noinline)) chain_inner(double d, int x) {
    union { double d; unsigned int w[2]; } u;
    u.d = d;
    printf("f6: w0=%08x w1=%08x x=%d\n", u.w[0], u.w[1], x);
}

void __attribute__((noinline)) chain_outer(const char *tag, double d, int mode, int ndigits) {
    printf("f6tag: %s\n", tag);
    chain_inner(d, mode + ndigits);
}

int main() {
    /* 1.25 = 0x3FF4000000000000: w0=00000000, w1=3FF40000 */
    recv_first(1.25, 42, 99);

    /* 1.5 = 0x3FF8000000000000: w0=00000000, w1=3FF80000 */
    recv_second("hello", 1.5, 77);

    recv_after_int(10, 1.25, 20);

    /* 1.25 and 3.75 = 0x400E000000000000 */
    recv_two_doubles(1.25, 3.75);

    int decpt = 0, sign = 0;
    dtoa_like(1.25, 2, 7, &decpt, &sign);

    chain_outer("chain", 1.25, 2, 7);

    return 0;
}
