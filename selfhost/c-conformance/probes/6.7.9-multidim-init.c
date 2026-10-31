// 6.7.9p17-p21  initializers for arrays of arrays: nested braces, brace elision, missing rows, size from the initializer
#include <stdio.h>
int part[2][3][4] = { { {1}, {2, 3} }, { {4, 5, 6, 7} } };
int flat[2][2][2] = { 1, 2, 3, 4, 5, 6, 7, 8 };
int infer[][2][2] = { { {1, 2}, {3, 4} }, { {5} } };
char names[2][3][6] = { { "ab", "cde", "f" }, { "ghij", "", "klmno" } };
int main(void) {
    static int sl[2][2][2] = { { {1, 2}, {3, 4} }, { {5, 6}, {7, 8} } };
    int lp[2][3][4] = { { {1}, {2, 3} }, { {4, 5, 6, 7} } };
    int li[][2][3] = { { {1, 2, 3} }, { {4} } };
    int i, j, k, s = 0;
    for (i = 0; i < 2; i++) for (j = 0; j < 3; j++) for (k = 0; k < 4; k++) s += (part[i][j][k] + 2 * lp[i][j][k]) * (i * 12 + j * 4 + k + 1);
    printf("%d %d %d %u %d\n", s, flat[1][0][1], infer[1][0][0], (unsigned)sizeof infer, sl[1][1][0]);
    printf("%s %s [%s] %s %u %u %d\n", names[0][1], names[1][0], names[1][1], names[1][2],
           (unsigned)sizeof li, (unsigned)sizeof names, li[1][0][0]);
    return 0;
}
