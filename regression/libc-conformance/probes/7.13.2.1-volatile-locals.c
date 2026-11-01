/* 7.13.2.1p3: after longjmp, volatile automatic objects changed since setjmp have their latest values, and statics keep theirs */
#include <setjmp.h>
#include <stdio.h>

static jmp_buf env;
static int sv;

static void spoil(int n) {
    volatile int a[40];
    int i;
    for (i = 0; i < 40; i++) a[i] = n * i;
    if (n > 0) spoil(n - 1);
    else longjmp(env, 1);
}

int main(void) {
    volatile int v = 1;
    volatile long long ll = 10;
    volatile double d = 0.5;
    char *volatile p = "first";
    if (setjmp(env) == 0) {
        v = 2;
        ll = 1LL << 40;
        d = 2.25;
        p = "second";
        sv = 7;
        spoil(5);
    }
    printf("%d %lld %g %s %d\n", v, ll, d, p, sv);
    return 0;
}
