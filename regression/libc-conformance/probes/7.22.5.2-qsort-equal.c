/* 7.22.5.2 qsort: ascending order by the comparison, equal elements and all-equal arrays included */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

static int cmp(const void *a, const void *b) {
    int x = *(const int *)a, y = *(const int *)b;
    return (x > y) - (x < y);
}
static void show(int *v, size_t n) {
    size_t i;
    qsort(v, n, sizeof v[0], cmp);
    for (i = 0; i < n; i++) printf("%d ", v[i]);
    printf("\n");
}
int main(void) {
    int a[] = {5, 3, 5, 1, 3, 5, 0, -2, 3, 1};
    int b[] = {4, 4, 4, 4, 4, 4, 4};
    int c[] = {2, 1};
    int d[] = {9};
    int e[1] = {0};
    show(a, 10); show(b, 7); show(c, 2); show(d, 1); show(e, 0);
    return 0;
}
