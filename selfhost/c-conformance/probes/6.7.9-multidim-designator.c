// 6.7.9p17-p18  designators into arrays of arrays: [i][j] names a row or an element; the next initializer follows it
#include <stdio.h>
int d[2][3][4] = { [1][2][3] = 9, [0][1] = { 5, 6 }, 7 };
int d2[2][3] = { [1][2] = 5, [0] = { 1 } };
int main(void) {
    int e[2][2][2] = { [1][1][0] = 4, [0][0][1] = 3 };
    int e2[3][2] = { [2] = { 8, 9 }, [0][1] = 2 };
    printf("%d %d %d %d %d\n", d[1][2][3], d[0][1][0], d[0][1][1], d[0][2][0], d[0][0][0]);
    printf("%d %d %d %d %d %d\n", d2[1][2], d2[0][0], e[1][1][0], e[0][0][1], e2[2][1], e2[0][1]);
    return 0;
}
