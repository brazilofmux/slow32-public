// 6.7.3p8 6.7.3.1  restrict-qualified pointers and parameters
#include <stdio.h>
static void add(int n, int *restrict a, const int *restrict b) { for (int i = 0; i < n; i++) a[i] += b[i]; }
int main(void) {
    int x[4] = { 1, 2, 3, 4 }, y[4] = { 10, 20, 30, 40 };
    int *restrict p = x;
    add(4, p, y);
    printf("%d %d\n", x[3], x[0]);
    return 0;
}
