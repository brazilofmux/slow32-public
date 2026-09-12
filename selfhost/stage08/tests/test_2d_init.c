/* Local 2D numeric array initializers with runtime values (regal's
 * valuation.c builds a 3x4 augmented matrix this way).  Rows may be
 * partial (zero-filled), braces may be elided within a row, and the
 * outer count may be inferred. */
static double det3(double m[3][3]) {
    return m[0][0] * (m[1][1] * m[2][2] - m[1][2] * m[2][1])
         - m[0][1] * (m[1][0] * m[2][2] - m[1][2] * m[2][0])
         + m[0][2] * (m[1][0] * m[2][1] - m[1][1] * m[2][0]);
}

int main(void) {
    int n = 3;
    double sx = 6.0, sx2 = 14.0, sy = 12.0;
    double aug[3][4] = {
        {(double)n, sx,  sx2, sy},
        {sx,        sx2, 36.0, 28.0},
        {sx2,       36.0, 98.0, 68.0}
    };
    int part[2][3] = { {1, 2}, {4} };          /* zero-filled tails */
    int flat[2][2] = { 1, 2, 3, 4 };            /* rows without braces */
    long inf[][2] = { {5, 6}, {7, 8}, {9, 10} }; /* inferred row count */
    double m[3][3] = { {2, 0, 0}, {0, 3, 0}, {0, 0, 4} };
    char rows[2][4] = { "ab", "cde" };          /* the old string-row path */

    if (aug[0][0] != 3.0 || aug[1][2] != 36.0 || aug[2][3] != 68.0) return 1;
    if (part[0][2] != 0 || part[1][1] != 0 || part[1][0] != 4) return 2;
    if (flat[1][0] != 3 || flat[0][1] != 2) return 3;
    if (sizeof(inf) != 6 * sizeof(long) || inf[2][1] != 10) return 4;
    if (det3(m) != 24.0) return 5;
    if (rows[0][2] != 0 || rows[1][2] != 'e' || rows[1][3] != 0) return 6;
    return 0;
}
