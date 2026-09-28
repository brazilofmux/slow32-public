/* The C side of tests/free/calleesaved.cbl (cobol ISSUES-57): enough
 * values live across a call into a COBOL program that the compiler keeps
 * some in r12 and r13, which the C ABI says a callee preserves.  The
 * COBOL program makes a dynamic CALL, whose code uses r12 and r13. */
extern int csvsub(void);

static volatile int seed = 3;

int csvdrive(void)
{
    int a = seed, b = a * 5, c = b + 7, d = c * 3, e = d - 11, f = e * 2, g = f + 13,
        h = g * 3, i = h - 17, j = i * 2, k = j + 19, l = k * 3, m = l - 23, n = m * 2;
    csvsub();
    long s = (long)a + b + c + d + e + f + g + h + i + j + k + l + m + n;
    int expect = 0;
    {   int a2 = 3, b2 = a2 * 5, c2 = b2 + 7, d2 = c2 * 3, e2 = d2 - 11, f2 = e2 * 2, g2 = f2 + 13,
            h2 = g2 * 3, i2 = h2 - 17, j2 = i2 * 2, k2 = j2 + 19, l2 = k2 * 3, m2 = l2 - 23, n2 = m2 * 2;
        expect = a2 + b2 + c2 + d2 + e2 + f2 + g2 + h2 + i2 + j2 + k2 + l2 + m2 + n2; }
    return s == expect;
}
