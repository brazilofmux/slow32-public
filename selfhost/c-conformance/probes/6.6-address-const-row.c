// 6.6p9  address constants into a two-dimensional array: &m[i][j], &m[i], m[i], m + i
#include <stdio.h>
static int m[3][3] = { { 1, 2, 3 }, { 4, 5, 6 }, { 7, 8, 9 } };
static int *q = &m[1][0], *r = &m[2][1] + 1, *s = m[1];
static int (*pa)[3] = &m[1], (*pb)[3] = m + 2;
int main(void) {
    printf("%d %d %d %d %d\n", *q, *r, s[2], pa[0][1], (*pb)[0]);
    return 0;
}
