/* cfuzz seed 5406 (wide profile), reduced (2026-10-27).  clang -O2 for
 * SLOW-32 prints a different g0 from stage08 and the host compiler, which
 * agree.  Doubles converted through fcv() feed a shift; not yet diagnosed,
 * may share a cause with llvm-o2-seed5262.c: see docs/IMPROVEMENTS.md. */
#include <stdio.h>

struct S { unsigned a; unsigned short b; unsigned char c; unsigned long long d; };
static unsigned g0 = 11u, g1 = 0x9e3779b9u, g2, g3 = 7u;
static unsigned ga[8] = { 3u, 1u, 4u, 1u, 5u, 9u, 2u, 6u };
static unsigned fcv(double d) {
    if (d != d) return 1u;
    if (d > 1e15) return 2u;
    if (d < -1e15) return 3u;
    return (unsigned)(long long)d;
}
static void bump(unsigned *q, unsigned k) { q[0] += k; q[0] ^= k >> 3; }
static unsigned sum4(const unsigned *q) { return q[0] + q[1] * 3u + q[2] * 5u + q[3] * 7u; }
static struct S smix(struct S x, unsigned k) {
    x.a += k; x.b ^= (unsigned short)k; x.c += 1u; x.d = x.d * 3u + k;
    return x;
}
static unsigned shash(struct S x) { return x.a ^ x.b ^ ((unsigned)x.c << 8) ^ (unsigned)(x.d >> 7); }

static unsigned f0(unsigned p0, unsigned p1, unsigned p2);

static unsigned f0(unsigned p0, unsigned p1, unsigned p2) {
    unsigned char v0 = 0;
    unsigned char v1 = 0;
    int i1 = 0;
    int i2 = 0;
    int i3 = 0;
    v0 = (unsigned char)0u;
    v1 = (unsigned char)0u;
    for (i1 = 1; i1 < 11; i1 += 1) {
        if ((unsigned)((int)(fcv((((double)(((unsigned)p2 * 128u) & 0xffffu) * (double)(65535u & 0xffffu)) * ((double)(((unsigned)p2 - (unsigned)ga[((unsigned)i1) & 7u]) & 0xffffu) * 0.5))) << 18) > (int)(unsigned)i1)) return 0u;
        for (i2 = 5; i2 >= 2; i2 -= 1) {
            if ((unsigned)((unsigned)v0 <= 27u)) {
                v0 |= (unsigned char)((unsigned)i1 + ((unsigned)i1 % ((unsigned)ga[(((unsigned)g0 - (unsigned)g1)) & 7u] | 1u)));
                g1 = (unsigned)((unsigned)v1 * (unsigned)v1);
            }
            g0 += (unsigned)((1u - (unsigned)v0) << 17);
        }
        v1 |= (unsigned char)((unsigned)g2 - ((unsigned)g1 / ((unsigned)p2 | 1u)));
        switch ((((unsigned)i1 + (unsigned)p2) ^ (unsigned)i3) % 6u) {
        case 0:
            g0 ^= (unsigned)((unsigned)i1 << 6);
            break;
        case 2:
            for (i3 = 1; i3 < 9; i3 += 1) {
                g2 = (unsigned)(unsigned)i1;
            }
            break;
        case 5:
            break;
        }
    }
done:
    return (unsigned)i3;
}

int main(void) {
    printf("%s %u\n", "f0", f0(5u, 3908055128u, 2613455268u));
    printf("g %u %u %u %u %u\n", g0, g1, g2, g3, sum4(ga) ^ sum4(ga + 4));
    return 0;
}
