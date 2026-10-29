// 6.7.6.2p6/6.5.6p8-9  pointers to VLAs step by the runtime row size: ++, +=, differences, three dimensions, structs, typedefs
#include <stdio.h>
struct pt { int x; short y; };
static long many(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j) {
    return a + b + c + d + e + f + g + h + i * 100 + j * 1000;
}
static int rec(int n) {
    int v[n + 1];
    int k;
    for (k = 0; k <= n; k++) v[k] = k;
    if (n == 0) return v[0];
    return v[n] + rec(n - 1);
}
static int cube(int p, int q, int r) {
    int c[p][q][r];
    int i, j, k, s = 0;
    for (i = 0; i < p; i++) for (j = 0; j < q; j++) for (k = 0; k < r; k++) c[i][j][k] = i * 100 + j * 10 + k;
    for (i = 0; i < p; i++) s += c[i][q - 1][r - 1];
    return s + (int)(sizeof c / sizeof c[0][0][0]) * 1000 + (int)sizeof c[1];
}
static int rows(int n, int m) {
    int g[n][m];
    int (*p)[m] = g;
    int (*q)[m];
    int i, j;
    for (i = 0; i < n; i++) for (j = 0; j < m; j++) g[i][j] = i * m + j;
    q = p + 2;
    p++;
    q += 1;
    return (*p)[1] + q[0][m - 1] * 10 + (int)(q - p) * 1000 + (int)((char *)q - (char *)p);
}
static int structs(int n) {
    struct pt s[n];
    typedef struct pt row[n];
    row t[2];
    int i;
    for (i = 0; i < n; i++) { s[i].x = i; s[i].y = (short)(i * 2); t[1][i] = s[i]; }
    return t[1][n - 1].x + t[1][n - 1].y * 10 + (int)sizeof t * 100;
}
static int aligned(int n) {
    _Alignas(16) int a[4];
    int v[n];
    v[n - 1] = 7;
    a[0] = (int)(((unsigned long)a) % 16);
    return a[0] + v[n - 1] + (int)many(1, 2, 3, 4, 5, 6, 7, 8, v[n - 1], n);
}
int main(void) {
    int n = 5;
    char buf[n];
    buf[4] = 9;
    printf("%d %d %d %d %d %ld\n", rec(10), cube(3, 4, 5), rows(5, 3), structs(4), aligned(3),
           many(1, 2, 3, 4, 5, 6, 7, 8, buf[4], (int)sizeof buf));
    return 0;
}
