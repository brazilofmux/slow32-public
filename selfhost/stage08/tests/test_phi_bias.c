/* Phi copies the coalescer froze are biased toward one register at
 * selection, never when that would close a cycle on an edge.  Swaps,
 * rotations and permutations through loop phis under high register
 * pressure must still deliver each value to the right place; a
 * 128-byte local of 64-bit words (ssa_sroa_late's widest) is split. */
#include <stdint.h>

/* Two values exchanged every trip: the copies on the back edge form a
 * cycle by construction. */
int swapper(int a, int b, int n) {
    int t;
    while (n-- > 0) {
        t = a;
        a = b + 1;
        b = t * 2;
    }
    return a * 1000 + b;
}

/* Eight-way rotation, sha512's shape, with a value computed fresh. */
unsigned rot8(unsigned seed, int n) {
    unsigned a = seed, b = 2, c = 3, d = 4, e = 5, f = 6, g = 7, h = 8;
    unsigned t;
    int i;
    for (i = 0; i < n; i++) {
        t = h + (e ^ f) + (unsigned)i;
        h = g; g = f; f = e; e = d + t;
        d = c; c = b; b = a; a = t ^ (b << 3);
    }
    return a ^ (b * 3) ^ (c * 5) ^ (d * 7) ^ (e * 11) ^ (f * 13) ^ (g * 17) ^ (h * 19);
}

/* A byte state permuted and substituted each round, AES's shape. */
static const unsigned char box[16] = { 9, 4, 10, 11, 13, 1, 8, 5, 6, 2, 0, 3, 12, 14, 15, 7 };

unsigned perm16(unsigned seed, int rounds) {
    unsigned char s[16], t[16];
    int r, i;
    unsigned out;
    for (i = 0; i < 16; i++) s[i] = (unsigned char)((seed >> (i & 7)) + i);
    for (r = 0; r < rounds; r++) {
        for (i = 0; i < 16; i++) t[i] = (unsigned char)(box[s[i] & 15] ^ (s[i] & 0xf0));
        for (i = 0; i < 16; i++) s[i] = t[(i * 5 + 3) % 16];
        if (r & 1) {
            for (i = 0; i < 16; i += 4) {
                unsigned char x = s[i];
                s[i] = (unsigned char)(s[i] ^ s[i + 1]);
                s[i + 1] = (unsigned char)(s[i + 1] + s[i + 2]);
                s[i + 2] = (unsigned char)(s[i + 2] ^ s[i + 3]);
                s[i + 3] = (unsigned char)(s[i + 3] + x);
            }
        }
        for (i = 0; i < 16; i++) s[i] = (unsigned char)(s[i] ^ (r * 7 + i));
    }
    out = 0;
    for (i = 0; i < 16; i++) out = out * 31 + s[i];
    return out;
}

/* Sixteen 64-bit words, mixed with constant indexes in a loop. */
uint64_t wide16(uint64_t seed, int rounds) {
    uint64_t v[16];
    int i, r;
    for (i = 0; i < 16; i++) v[i] = seed * (uint64_t)(i + 1) + (uint64_t)i;
    for (r = 0; r < rounds; r++) {
        for (i = 0; i < 8; i++) {
            v[i] = v[i] + v[i + 8];
            v[i + 8] = (v[i + 8] << 13 | v[i + 8] >> 51) ^ v[i];
        }
        v[0] ^= (uint64_t)r;
    }
    for (i = 1; i < 16; i++) v[0] ^= v[i] + (uint64_t)i;
    return v[0];
}

int main(void) {
    if (swapper(3, 5, 7) != 55062) return 1;
    if (rot8(0x9e3779b9u, 40) != 1163704409u) return 2;
    if (perm16(12345u, 9) != 943940786u) return 3;
    if (wide16(0x0123456789abcdefULL, 6) != 0x1d1e3233923b13c8ULL) return 4;
    return 0;
}
