/* GitHub issue 48: argument-region exemption so a macro used inside
 * its own argument still expands.  The 9th parameter and __VA_ARGS__
 * were unrecorded, so a self-name there stayed painted disabled.
 * Keep these in statements: a file-scope initializer sits against a
 * short prefix, and backwards splicing can eat the name. */
#define R(x) ((x) + 1)
#define S(n) R(n)
#define NINE(a, b, c, d, e, f, g, h, i) i
#define VA(a, ...) __VA_ARGS__

int main(void) {
    if (R(S(1)) != 3) return 1;
    if (NINE(0, 0, 0, 0, 0, 0, 0, 0, NINE(1, 1, 1, 1, 1, 1, 1, 1, 2)) != 2) return 2;
    if (VA(0, VA(1, 3)) != 3) return 3;
    if (R(S(2)) != 4) return 4;
    if (NINE(9, 9, 9, 9, 9, 9, 9, 9, R(4)) != 5) return 5;
    if (VA(0, R(S(1))) != 3) return 6;
    return 0;
}
