/* selfhost ISSUES-82: the integer promotions (C90 6.2.1.1) were skipped.
 * An unsigned char or unsigned short operand passed its unsigned flag
 * straight into an arithmetic, comparison, shift or unary result, so
 * (c >= '0' && c <= '9' ? c - '0' : -1) < 0 with c an unsigned char was
 * an unsigned compare, never true.  Found by cobol's selfhost-libcob
 * gate (TEST-FORMATTED-DATETIME of "123456x78" returning 0, not 7).  A
 * char or short, signed or unsigned, is an int in those contexts. */
static int dig(const unsigned char *p, int i, int n) { return (i < n && p[i] >= '0' && p[i] <= '9') ? p[i] - '0' : -1; }
static int t_ternary(const unsigned char *p) { return ((p[0] >= '0' && p[0] <= '9' ? p[0] - '0' : -1) < 0); }
static int t_sub(const unsigned char *p) { return p[0] - 200 < 0; }
static int t_neg(unsigned char c) { return -c < 0; }
static int t_not(unsigned char c) { return ~c < 0; }
static int t_short(unsigned short s) { return s - 70000 < 0; }
static int t_cmp(unsigned char c) { return c > -1; }
static int t_shift(unsigned char c) { return (c << 24) >> 24; }
static int t_uint(unsigned int u) { return u - 1 < 0; }
static int t_loop(const unsigned char *p, int n) {
    int i = 6; int k;
    for (k = 0; k < 3; k++) { if (dig(p, i, n) < 0) return i + 1; i++; }
    return 0;
}

int main(void) {
    const unsigned char *x = (const unsigned char *)"123456x78";
    if (t_ternary(x + 6) != 1) return 1;
    if (t_sub(x) != 1) return 2;                /* '1' - 200 = -151 */
    if (t_neg(5) != 1) return 3;
    if (t_not(5) != 1) return 4;                /* ~5 = -6 */
    if (t_short(5) != 1) return 5;
    if (t_cmp(5) != 1) return 6;                /* 5 > -1, signed */
    if (t_shift(0xC1) != -63) return 7;         /* sign-extended through the signed shift */
    if (t_uint(0) != 0) return 8;               /* unsigned int stays unsigned: 0xFFFFFFFF < 0 is false */
    if (t_loop(x, 9) != 7) return 9;
    return 0;
}
