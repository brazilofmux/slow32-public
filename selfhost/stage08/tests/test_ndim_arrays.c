/* Arrays of more than two dimensions, refused until 2026-10-30 ("arrays
 * of more than 2 dimensions unsupported"; SLOW-32 xBase's label.c and
 * report.c each declare one), and the multi-dimensional initializer
 * forms that failed even for two: a 2-D member in a local struct's
 * initializer, designators into rows, compound literals of array type.
 * Each array is held flat; arr_cols is what the first subscript steps
 * over and arr_sub (ast.h ast_sub_cols) the strides below it. */
struct T { int t; short m[2][2][3]; int u; };
struct T gt = { 5, { { {1, 2, 3}, {4, 5, 6} }, { {7, 8, 9}, {10, 11, 12} } }, 99 };
int g[2][3][4];
int g4[2][2][3][5];
int part[2][3][4] = { { {1}, {2, 3} }, { {4, 5, 6, 7} } };
int flat[2][2][2] = { 1, 2, 3, 4, 5, 6, 7, 8 };
int infer[][2][2] = { { {1, 2}, {3, 4} }, { {5} } };
char names[2][3][6] = { { "ab", "cde", "f" }, { "ghij", "", "klmno" } };
int d[2][3][4] = { [1][2][3] = 9, [0][1] = { 5, 6 }, 7 };
int d2[2][3] = { [1][2] = 5, [0] = { 1 } };
int (*fp)[2][2] = (int[][2][2]){ { {1, 2}, {3, 4} }, { {5, 6}, {7, 8} } };

static int sum(int a[][3][4], int n) {
    int i, j, k, s = 0;
    for (i = 0; i < n; i++) for (j = 0; j < 3; j++) for (k = 0; k < 4; k++) s += a[i][j][k];
    return s + (int)sizeof a[0];
}
static int take(int (*r)[4], int n) { int s = 0, i, j; for (i = 0; i < n; i++) for (j = 0; j < 4; j++) s += r[i][j]; return s; }
static int streq(const char *a, const char *b) { while (*a && *a == *b) { a++; b++; } return *a == *b; }

int main(void) {
    char b[3][4][5];
    int a[2][3][4];
    int lp[2][3][4] = { { {1}, {2, 3} }, { {4, 5, 6, 7} } };
    int li[][2][3] = { { {1, 2, 3} }, { {4} } };
    int e[2][2][2] = { [1][1][0] = 4, [0][0][1] = 3 };
    struct T lt = { 6, { { {21, 22, 23} }, { {31}, {41, 42} } }, 77 };
    struct { char k; short m2[2][3]; } l2 = { 'x', { {1, 2}, {3} } };
    int (*q)[3][4] = g;
    int (*lc)[2][2] = (int[][2][2]){ { {9} }, { {8, 7}, {6} } };
    int i, j, k, l, s;
    for (i = 0; i < 3; i++) for (j = 0; j < 4; j++) for (k = 0; k < 5; k++) b[i][j][k] = (char)(i * 20 + j * 5 + k);
    for (i = 0; i < 2; i++) for (j = 0; j < 3; j++) for (k = 0; k < 4; k++) { a[i][j][k] = i * 100 + j * 10 + k; g[i][j][k] = a[i][j][k]; }
    for (i = 0; i < 2; i++) for (j = 0; j < 2; j++) for (k = 0; k < 3; k++) for (l = 0; l < 5; l++) g4[i][j][k][l] = i * 1000 + j * 100 + k * 10 + l;
    s = 0;
    for (i = 0; i < 3; i++) for (j = 0; j < 4; j++) for (k = 0; k < 5; k++) s += b[i][j][k] * (k + 1);
    if (s != 5430 || sizeof b != 60 || sizeof b[1] != 20 || sizeof b[1][2] != 5 || b[2][3][4] != 59) return 1;
    if (sum(g, 2) != 1524 || q[1][2][3] != 123 || g4[1][1][2][4] != 1124 || sizeof g4[1][1] != 60) return 2;
    if ((*(a[1] + 2))[1] != 121 || **a[1] != 100 || *(*(*(a + 1) + 2) + 3) != 123 || take(a[0], 3) != 138) return 3;
    if (&a[1] - &a[0] != 1 || (char *)a[1][2] - (char *)a != 80 || a[1][2] - a[1][0] != 8) return 4;
    s = 0;
    for (i = 0; i < 2; i++) for (j = 0; j < 3; j++) for (k = 0; k < 4; k++) s += (part[i][j][k] + 2 * lp[i][j][k]) * (i * 12 + j * 4 + k + 1);
    if (s != 1059 || flat[1][0][1] != 6 || infer[1][0][0] != 5 || sizeof infer != 32) return 5;
    if (!streq(names[0][1], "cde") || !streq(names[1][0], "ghij") || names[1][1][0] != 0 || !streq(names[1][2], "klmno")) return 6;
    if (sizeof li != 48 || li[1][0][0] != 4 || li[0][0][2] != 3 || li[1][1][2] != 0) return 7;
    if (d[1][2][3] != 9 || d[0][1][0] != 5 || d[0][1][1] != 6 || d[0][2][0] != 7 || d[0][0][0] != 0) return 8;
    if (d2[1][2] != 5 || d2[0][0] != 1 || e[1][1][0] != 4 || e[0][0][1] != 3 || e[0][0][0] != 0) return 9;
    if (gt.m[1][0][2] != 9 || gt.m[1][1][0] != 10 || gt.u != 99 || sizeof gt != 32 || sizeof gt.m[1] != 12) return 10;
    if (lt.m[0][0][1] != 22 || lt.m[1][0][0] != 31 || lt.m[1][1][1] != 42 || lt.u != 77 || lt.m[0][1][0] != 0) return 11;
    if (l2.m2[0][1] != 2 || l2.m2[1][0] != 3 || l2.m2[1][1] != 0) return 12;
    if (fp[1][1][0] != 7 || fp[0][1][1] != 4 || lc[1][0][1] != 7 || lc[0][1][1] != 0) return 13;
    return 0;
}
