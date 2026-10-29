// 6.7.5p6/6.2.8p3  locals aligned beyond the stack's own alignment, at every depth: leaves, recursion, large frames, stack arguments
#include <stdio.h>
#include <stdint.h>
static int bad;
static void check(void *p, unsigned a) { if ((uintptr_t)p % a) bad = bad + 1; }
static int leaf(int x) {
    _Alignas(16) int v = x;
    check(&v, 16);
    return v + 1;
}
static int many(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j) {
    _Alignas(64) char buf[3];
    buf[0] = (char)i;
    check(buf, 64);
    return a + b + c + d + e + f + g + h + i * 10 + j * 100 + buf[0];
}
static int big(int n) {
    char pad[3000];
    _Alignas(128) double d[2];
    int k;
    for (k = 0; k < 3000; k++) pad[k] = (char)k;
    d[0] = n;
    d[1] = pad[2999];
    check(d, 128);
    return (int)(d[0] + d[1]);
}
static int deep2(int n);
static int deep(int n) {
    _Alignas(32) short s[3];
    volatile char odd[5];
    s[0] = (short)n;
    odd[0] = 1;
    check(s, 32);
    if (n == 0) return odd[0];
    return s[0] + deep2(n - 1) + odd[0];
}
static int deep2(int n) {
    volatile int w[3];
    w[0] = n;
    if (n == 0) return 2;
    return w[0] + deep(n - 1);
}
int main(void) {
    int r = 0;
    int k;
    for (k = 0; k < 7; k++) {
        r += leaf(k);
        r += many(1, 2, 3, 4, 5, 6, 7, 8, k, 10);
        r += big(k);
        r += deep(k + 3);
    }
    printf("%d %d\n", r, bad);
    return 0;
}
