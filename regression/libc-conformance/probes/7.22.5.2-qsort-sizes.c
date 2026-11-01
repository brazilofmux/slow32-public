/* 7.22.5.2 qsort: elements of odd and large sizes move whole, and the result is a permutation */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

static size_t SZ;
static int cmp(const void *a, const void *b) {
    unsigned char x = *(const unsigned char *)a, y = *(const unsigned char *)b;
    return (x > y) - (x < y);
}
static unsigned seed = 99;
static unsigned rnd(void) { seed = seed * 1103515245u + 12345u; return seed >> 16; }
static unsigned char buf[200 * 101];
static void run(size_t sz, size_t n) {
    size_t i, k;
    unsigned long sum0 = 0, sum1 = 0;
    int sorted = 1, whole = 1;
    SZ = sz;
    for (i = 0; i < n; i++) {
        unsigned char key = (unsigned char)(rnd() % 23);
        for (k = 0; k < sz; k++) buf[i * sz + k] = (unsigned char)(key + k);   /* every byte follows the key */
        sum0 += key;
    }
    qsort(buf, n, sz, cmp);
    for (i = 0; i < n; i++) {
        unsigned char key = buf[i * sz];
        for (k = 0; k < sz; k++) if (buf[i * sz + k] != (unsigned char)(key + k)) whole = 0;
        if (i && buf[(i - 1) * sz] > key) sorted = 0;
        sum1 += key;
    }
    printf("size %3u n %3u: sorted=%d whole=%d same=%d\n", (unsigned)sz, (unsigned)n, sorted, whole, sum0 == sum1);
}
int main(void) {
    size_t sizes[] = {1, 2, 3, 4, 5, 7, 8, 12, 16, 33, 101};
    size_t i;
    for (i = 0; i < sizeof sizes / sizeof sizes[0]; i++) { run(sizes[i], 200); run(sizes[i], 3); }
    return 0;
}
