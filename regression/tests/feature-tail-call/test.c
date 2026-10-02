/* Tail calls (llvm-backend: SLOW32ISD::TAIL).  A call in tail position
 * is a jump: the caller's frame is popped first.  Each case below either
 * prints what it computed -- arguments, saved registers and results all
 * have to come through the popped frame intact -- or could not have
 * finished at all if the frames had been kept: a million activations
 * deep, on a stack that holds a few thousand. */
#include <stdio.h>

#define NOINLINE __attribute__((noinline))

/* mutual recursion in tail position: no stack growth */
NOINLINE static int is_odd(unsigned n);
NOINLINE static int is_even(unsigned n) { if (n == 0) return 1; return is_odd(n - 1); }
NOINLINE static int is_odd(unsigned n) { if (n == 0) return 0; return is_even(n - 1); }

/* the same through a pointer */
typedef int (*step_fn)(unsigned, unsigned);
static step_fn steps[2];
NOINLINE static int count_a(unsigned n, unsigned acc) { if (n == 0) return (int)acc; return steps[n & 1](n - 1, acc + 1); }
NOINLINE static int count_b(unsigned n, unsigned acc) { if (n == 0) return (int)acc; return steps[n & 1](n - 1, acc + 2); }

/* eight register arguments, after real calls, from a frame too large
 * for a 12-bit offset: the restores need a scratch register, and it
 * must not be one of the eight */
NOINLINE static int mix8(int a, int b, int c, int d, int e, int f, int g, int h) {
    return a + 2 * b + 3 * c + 5 * d + 7 * e + 11 * f + 13 * g + 17 * h;
}
NOINLINE static int twice(int x) { return 2 * x; }
volatile int at;
NOINLINE static int big_frame(int a) {
    int arr[1500];
    arr[at] = a;
    arr[at + 700] = a + 1;
    int x = twice(arr[at]);
    int y = twice(x + arr[at + 700]);
    return mix8(a, x, y, a + 1, x + 1, y + 1, a + 2, arr[at]);
}
static int (*mix8_ptr)(int, int, int, int, int, int, int, int) = mix8;
NOINLINE static int big_frame_indirect(int a) {
    int arr[1500];
    arr[at] = a;
    int x = twice(arr[at]);
    int y = twice(x);
    return mix8_ptr(a, x, y, a + 1, x + 1, y + 1, a + 2, arr[at]);
}

/* a pointer held in a callee-saved register across a call, then jumped through */
NOINLINE static int add3(int x) { return x + 3; }
NOINLINE static int call_then_jump(int a, int (*f)(int)) { int x = twice(a); return f(x + a); }

/* a 64-bit result passes through */
NOINLINE static long long wide(int a) { return 0x100000000LL * a + 7; }
NOINLINE static long long wide_fwd(int a) { return wide(a + 1); }

/* the shape it is for: a short path, and otherwise the general routine */
static int general_calls;
NOINLINE static int general(int *p, int n) { general_calls++; return *p + n; }
NOINLINE static int entry(int *p, int n) {
    if (n < 10) return *p - n;
    return general(p, n);
}

/* a variadic callee, all arguments in registers */
NOINLINE static int report(const char *what, int v) { return printf("%s %d\n", what, v); }

int main(void) {
    int cell = 100;
    steps[0] = count_a;
    steps[1] = count_b;
    printf("even(1000000) %d odd(1000000) %d even(999999) %d\n", is_even(1000000), is_odd(1000000), is_even(999999));
    printf("through a pointer, a million deep: %d\n", count_a(1000000, 0));
    printf("big frame %d\n", big_frame(5));
    printf("big frame, indirect %d\n", big_frame_indirect(5));
    printf("call then jump %d\n", call_then_jump(10, add3));
    printf("wide %lld\n", wide_fwd(4));
    printf("entry short %d general %d calls %d\n", entry(&cell, 3), entry(&cell, 30), general_calls);
    printf("printf returned %d\n", report("variadic", 42));
    return 0;
}
