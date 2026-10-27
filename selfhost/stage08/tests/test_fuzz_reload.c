/* cfuzz seed 8710, reduced (2026-10-27): a read-modify-write of a
 * short array element in a function that spills nearly everything.  The
 * allocator inserted a block reload of the element's address, could not
 * place it, and deleted it, pointing the load and the store back at the
 * address -- whose interval still ended at the reload.  Spilled, the
 * address shared its slot with the XOR result, and the store went to
 * address 0x35.  Must run to completion and return 0. */

static unsigned f0(unsigned p0);
static unsigned f1(unsigned p0, unsigned p1, unsigned p2);

static unsigned f0(unsigned p0) {
    unsigned v0 = 0;
    int v1 = 0;
    unsigned char v2 = 0;
    int i0 = 0;
    int i1 = 0;
    int i2 = 0;
    int i4 = 0;
    int i5 = 0;
    int i6 = 0;
    int i7 = 0;
    unsigned short a0[16];
    { int z; for (z = 0; z < 16; z++) a0[z] = (unsigned short)(z * 37u + 16u); }
    v0 = (unsigned)0u;
    v1 = (int)(unsigned)(4096u > ((unsigned)a0[((unsigned)a0[((((unsigned)p0 - 65535u) - 1u)) & 15u]) & 15u] + 100u));
    v2 = (unsigned char)0u;
    a0[((unsigned)v1) & 15u] ^= (unsigned)((unsigned)p0 + 1u);
    for (i0 = 0; i0 < 4; i0 += 1) {
        v1 = (int)((unsigned)v1 | (((unsigned)a0[(2u) & 15u] - (unsigned)i0) * (0u / (2147483647u | 1u))));
        for (i1 = 3; i1 < 14; i1 += 1) {
            a0[((unsigned)i1) & 15u] -= (unsigned)(1u ? ((unsigned)v1 + 1u) : (unsigned)v0);
        }
    }
    for (i2 = 3; i2 < 14; i2 += 1) {
        for (i5 = 1; i5 < 2; i5 += 2) {
        }
        if ((unsigned)(i2 >= 11)) {
            for (i7 = 1; i7 < 5; i7 += 1) {
                a0[((unsigned)(((unsigned long long)(unsigned)a0[(0u) & 15u] * (unsigned)i1) >> 32)) & 15u] = (unsigned)(unsigned)v1;
                if ((unsigned)((((unsigned)i4 >> 8) - (unsigned)v1) != ((31u | (unsigned)v1) % (((unsigned)i5 >> 21) | 1u)))) {
                    v0 = (unsigned)((unsigned)((100u - (unsigned)i2) >= ((unsigned)i7 * (unsigned)i0)) ^ (unsigned)(((unsigned long long)(unsigned)i5 * (unsigned)i7) >> 32));
                    if ((unsigned)(i2 < 10)) {
                        v2 |= (unsigned char)(unsigned)a0[(((unsigned)i7 - 2048u)) & 15u];
                    } else {
                        v1 = (int)((unsigned)p0 % ((unsigned)v2 | 1u));
                        v2 ^= (unsigned char)(unsigned)i0;
                    }
                }
                a0[((unsigned)p0) & 15u] += (unsigned)(((unsigned)i5 + 1000u) - (unsigned)((unsigned)(((unsigned long long)5u * 4294967295u) >> 32) != ((unsigned)p0 + 65535u)));
                v0 = (unsigned)((((unsigned)a0[(256u) & 15u] | (unsigned)i4) - ((unsigned)i2 & (unsigned)a0[((((unsigned)i6 ^ (unsigned)p0) | ((unsigned)p0 + 4u))) & 15u])) * ((unsigned)i0 + 2047u));
            }
        }
    }
    v0 = (unsigned)((unsigned)(((unsigned)i7 - 5u) >= ((unsigned)v2 + (unsigned)v1)) ? 0u : (unsigned)(((unsigned long long)5u * 13u) >> 32));
    return 0u;
}

static unsigned f1(unsigned p0, unsigned p1, unsigned p2) {
    unsigned v2 = 0;
    v2 = (unsigned)0u;
    v2 = (unsigned)f0(4294967295u);
    return 0u;
}

int main(void) {
    if (f1(1567832222u, 9u, 63u) != 0u) return 1;
    return 0;
}
