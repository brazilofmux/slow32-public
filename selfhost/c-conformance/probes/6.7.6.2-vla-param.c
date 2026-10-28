// 6.7.6.2p5/6.7.6.3p7  VLA parameter types: int a[n][m] and [*] in a prototype
#include <stdio.h>
int trace(int n, int a[*][*]);
int trace(int n, int a[n][n]) { int s = 0; for (int i = 0; i < n; i++) s += a[i][i]; return s; }
int main(void) {
    int m[3][3] = { { 1, 0, 0 }, { 0, 5, 0 }, { 0, 0, 9 } };
    printf("%d\n", trace(3, m));
    return 0;
}
