// 6.5.2.1p3  successive subscripts of a multidimensional array; a partial subscript is an array that decays
#include <stdio.h>
static int take(int (*r)[4], int n) { int s = 0, i, j; for (i = 0; i < n; i++) for (j = 0; j < 4; j++) s += r[i][j]; return s; }
int main(void) {
    int a[2][3][4];
    int i, j, k;
    for (i = 0; i < 2; i++) for (j = 0; j < 3; j++) for (k = 0; k < 4; k++) a[i][j][k] = i * 100 + j * 10 + k;
    printf("%d %d %d %d\n", (*(a[1] + 2))[1], **a[1], *(*(*(a + 1) + 2) + 3), take(a[0], 3));
    printf("%d %d %d\n", (int)(&a[1] - &a[0]), (int)((char *)a[1][2] - (char *)a), (int)(a[1][2] - a[1][0]));
    return 0;
}
