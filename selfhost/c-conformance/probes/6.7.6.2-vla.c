// 6.7.6.2p4-5  variable length arrays: size evaluated at runtime, sizeof runtime, 2-D VLA
// optional: __STDC_NO_VLA__
#include <stdio.h>
static int fill(int n) {
    int a[n];
    for (int i = 0; i < n; i++) a[i] = i * i;
    int s = 0; for (int i = 0; i < n; i++) s += a[i];
    return s * 1000 + (int)sizeof a;
}
static int grid(int r, int c) {
    int g[r][c];
    for (int i = 0; i < r; i++) for (int j = 0; j < c; j++) g[i][j] = i * 10 + j;
    return g[r - 1][c - 1] + (int)sizeof g[0];
}
int main(void) {
    printf("%d %d\n", fill(5), fill(10));
    printf("%d\n", grid(3, 4));
    return 0;
}
