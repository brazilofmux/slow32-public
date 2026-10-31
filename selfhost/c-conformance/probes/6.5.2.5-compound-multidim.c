// 6.5.2.5p3-p6  compound literals of array-of-array type, at block and file scope
#include <stdio.h>
int (*fp)[2][2] = (int[][2][2]){ { {1, 2}, {3, 4} }, { {5, 6}, {7, 8} } };
static int sum(int (*a)[2], int n) { int s = 0, i, j; for (i = 0; i < n; i++) for (j = 0; j < 2; j++) s += a[i][j] * (i + 1); return s; }
int main(void) {
    int (*lp)[2][2] = (int[][2][2]){ { {9} }, { {8, 7}, {6} } };
    printf("%d %d %d %d\n", sum((int[2][2]){ {1, 2}, {3, 4} }, 2), fp[1][1][0], lp[1][0][1], lp[0][1][1]);
    return 0;
}
