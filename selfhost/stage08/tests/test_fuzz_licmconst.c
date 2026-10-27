/* cfuzz seed 39992 (wide profile), reduced (2026-10-27).  LICM strength
 * reduction put `b * 2654435761` (the low word of a 64-bit product) in
 * the loop preheader, reading the constant's register; the constant was
 * pinned as a loop constant and loaded only inside the loop, so the
 * preheader multiplied by whatever r7 held.  In every kit to batch 48.
 * Must return 0. */
struct S { unsigned a; unsigned short b; unsigned char c; unsigned long long d; };
static unsigned g0 = 11u, g1 = 0x9e3779b9u, g2, g3 = 7u;
static unsigned ga[8] = { 3u, 1u, 4u, 1u, 5u, 9u, 2u, 6u };
static unsigned fcv(double d) {
}
static unsigned sum4(const unsigned *q) { return q[0] + q[1] * 3u + q[2] * 5u + q[3] * 7u; }
static struct S smix(struct S x, unsigned k) {
}
static unsigned f2(unsigned p0, unsigned p1) {
    int i0 = 0;
    int i1 = 0;
    struct S s0;
    struct S s1;
    for (i0 = 3; i0 < 15; i0 += 1) {
        g3 -= (unsigned)(unsigned)p1;
        g3 |= (unsigned)(unsigned)(((((unsigned long long)27u << 17) | ((unsigned)p0 + (unsigned)i0)) + ((unsigned long long)((unsigned)p0 + (unsigned)i0) * 2654435761u + 1u)) >> 17);
        if ((unsigned)i0) return (0u + ((unsigned)ga[(((unsigned)(i0 < 5) ? (unsigned)i1 : (unsigned)i0)) & 7u] + 100u));
    }
    return 0u;
}
int main(void) {
    if (f2(1541546147u, 2255938393u) != 103u) return 1;
    if (g3 != 4257062591u) return 2;
    return 0;
}
