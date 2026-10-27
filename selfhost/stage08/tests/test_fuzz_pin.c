/* cfuzz seed 16026 (wide profile), reduced (2026-10-27).  A ?: whose
 * test is always true folds to the constant 4096, left in the arm's
 * block.  hcg_mark_loop_consts pinned it (one load at its definition,
 * for the loop), but LICM had hoisted `i2 - 4096` to the outer loop,
 * ahead of that block: the subtraction read the register before the
 * constant was loaded, the early return was never taken, and g2 was
 * decremented 60 times.  Older than the batch-47 work (HEAD of
 * 2026-10-27 morning had it too).  Must return 0. */
struct S { unsigned a; unsigned short b; unsigned char c; unsigned long long d; };
static unsigned g0 = 11u, g1 = 0x9e3779b9u, g2, g3 = 7u;
static unsigned ga[8] = { 3u, 1u, 4u, 1u, 5u, 9u, 2u, 6u };
static unsigned fcv(double d) {
}
static unsigned sum4(const unsigned *q) { return q[0] + q[1] * 3u + q[2] * 5u + q[3] * 7u; }
static struct S smix(struct S x, unsigned k) {
}
static unsigned f0(unsigned p0) {
    unsigned v1 = 0;
    unsigned v2 = 0;
    int v3 = 0;
    int i1 = 0;
    int i2 = 0;
    int i3 = 0;
    int i4 = 0;
    v2 = (unsigned)(unsigned)(((((unsigned long long)0u << 17) | 0u) % (((unsigned long long)0u * 2654435761u + 1u) | 1u)) >> 10);
    v3 = (int)(unsigned)(((int)((unsigned)v3 & 0xffffu) - 32768) * ((int)(1u & 0x7fffu)));
    for (i2 = 1; i2 < 7; i2 += 2) {
        for (i4 = 14; i4 >= 0; i4 -= 1) {
            if ((unsigned)(((unsigned)i2 - ((unsigned)((unsigned)i1 <= 4294967295u) ? 4096u : (unsigned)g0)) > ((unsigned)g3 ^ ((unsigned)v1 >> 3)))) return ((unsigned)i3 - (unsigned)p0);
            v2 |= (unsigned)(unsigned)(sum4(ga) > (((unsigned)v3 - (unsigned)i2) << 23));
            g2 -= (unsigned)(unsigned)p0;
        }
    }
    return 0u;
}
static unsigned f1(unsigned p0) {
    unsigned char v3 = 0;
    int i1 = 0;
    v3 = (unsigned char)1u;
    for (i1 = 3; i1 < 10; i1 += 2) {
        if (f0((unsigned)v3)) continue;
    }
    return 0u;
}
int main(void) {
    if (f1(20u) != 0u || g2 != 0u) return 1;
    return 0;
}