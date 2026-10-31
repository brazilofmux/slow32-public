// 6.7.6.2p3  arrays of arrays of arrays: block scope, file scope, parameters, pointers to them
#include <stdio.h>
int g[2][3][4];
int g4[2][2][3][5];
static int sum(int a[][3][4], int n) {
    int i, j, k, s = 0;
    for (i = 0; i < n; i++) for (j = 0; j < 3; j++) for (k = 0; k < 4; k++) s += a[i][j][k];
    return s + (int)sizeof a[0];
}
int main(void) {
    char b[3][4][5];
    int i, j, k, l, s = 0;
    int (*q)[3][4] = g;
    for (i = 0; i < 3; i++) for (j = 0; j < 4; j++) for (k = 0; k < 5; k++) b[i][j][k] = (char)(i * 20 + j * 5 + k);
    for (i = 0; i < 2; i++) for (j = 0; j < 3; j++) for (k = 0; k < 4; k++) g[i][j][k] = i * 100 + j * 10 + k;
    for (i = 0; i < 2; i++) for (j = 0; j < 2; j++) for (k = 0; k < 3; k++) for (l = 0; l < 5; l++) g4[i][j][k][l] = i + j + k + l;
    for (i = 0; i < 3; i++) for (j = 0; j < 4; j++) for (k = 0; k < 5; k++) s += b[i][j][k] * (k + 1);
    printf("%d %u %u %u %d\n", s, (unsigned)sizeof b, (unsigned)sizeof b[1], (unsigned)sizeof b[1][2], b[2][3][4]);
    printf("%d %d %d %u\n", sum(g, 2), q[1][2][3], g4[1][1][2][4], (unsigned)sizeof g4[1][1]);
    return 0;
}
