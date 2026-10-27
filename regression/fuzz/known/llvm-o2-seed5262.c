/* cfuzz seed 5262, reduced (2026-10-27).  The SLOW-32 LLVM backend at -O2
 * prints "f1 65472"; clang -O0 for SLOW-32, the host compiler and stage08
 * all print "f1 65470".  Not yet diagnosed: see docs/IMPROVEMENTS.md. */
#include <stdio.h>

static unsigned f0(unsigned p0, unsigned p1, unsigned p2);
static unsigned f1(unsigned p0, unsigned p1, unsigned p2);

static unsigned f0(unsigned p0, unsigned p1, unsigned p2) {
    unsigned v1 = 0;
    unsigned long long v3 = 0;
    int i0 = 0;
    int i1 = 0;
    int i3 = 0;
    unsigned short a0[4];
    unsigned char a1[4];
    { int z; for (z = 0; z < 4; z++) a0[z] = (unsigned short)(z * 37u + 4u); }
    { int z; for (z = 0; z < 4; z++) a1[z] = (unsigned char)(z * 37u + 4u); }
    v1 = (unsigned)0u;
    v3 = (unsigned long long)0u;
    for (i0 = 3; i0 < 10; i0 += 3) {
        v1 = (unsigned)((unsigned)p1 | (unsigned)i0);
        for (i1 = 0; i1 < 7; i1 += 1) {
            v3 -= (unsigned long long)((((unsigned)i1 % ((unsigned)i0 | 1u)) & (unsigned)a1[(((unsigned)(i0 == 5) ? 65535u : ((unsigned)v1 % ((unsigned)v1 | 1u)))) & 3u]) ^ (unsigned)p2);
        }
    }
    a0[((unsigned)i1) & 3u] = (unsigned)(unsigned)((int)(unsigned)v3 >> 5);
    for (i3 = 1; i3 < 12; i3 += 3) {
        if ((unsigned)i3) return (unsigned)a0[((0u - 5u)) & 3u];
    }
    return 0u;
}

static unsigned f1(unsigned p0, unsigned p1, unsigned p2) {
    int v0 = 0;
    unsigned short v1 = 0;
    unsigned short v2 = 0;
    v0 = (int)0u;
    v1 = (unsigned short)f0(0u, 256u, 100u);
    v2 = (unsigned short)0u;
    v2 += (unsigned short)f0((unsigned)v0, 0u, 0u);
    return (unsigned)v1;
}

int main(void) {
    printf("%s %u\n", "f1", f1(20u, 2612959600u, 1295292914u));
    return 0;
}
