/* 7.22.5.2 qsort: sorted, reversed, sawtooth and few-valued inputs of a few thousand elements */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

static long calls;
static int cmp(const void *a, const void *b) {
    int x = *(const int *)a, y = *(const int *)b;
    calls++;
    return (x > y) - (x < y);
}
static int v[3000];
static void check(const char *name, int n) {
    int i, ok = 1;
    long s0 = 0, s1 = 0;
    for (i = 0; i < n; i++) s0 += v[i];
    qsort(v, n, sizeof v[0], cmp);
    for (i = 0; i < n; i++) { s1 += v[i]; if (i && v[i - 1] > v[i]) ok = 0; }
    printf("%-9s ok=%d sum=%d\n", name, ok, s0 == s1);
}
int main(void) {
    int i, n = 3000;
    for (i = 0; i < n; i++) v[i] = i;            check("sorted", n);
    for (i = 0; i < n; i++) v[i] = n - i;        check("reversed", n);
    for (i = 0; i < n; i++) v[i] = i % 17;       check("sawtooth", n);
    for (i = 0; i < n; i++) v[i] = i & 1;        check("two", n);
    for (i = 0; i < n; i++) v[i] = (i * 7919) % 3001 - 1500; check("scatter", n);
    printf("compares bounded: %d\n", calls < 5L * 1000 * 1000);
    return 0;
}
