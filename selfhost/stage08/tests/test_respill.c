/* When colouring fails, the allocator respills: the failed value or a
 * neighbour.  A neighbour is charged its uses once per failure it
 * clears, credited for its span, so a long-lived value is spilled
 * rather than temporaries -- a temporary spilled is a store and an
 * immediate reload that free nothing.  Sixteen 64-bit words mixed
 * BLAKE2b-style hold more than the registers; run-tests.sh checks that
 * round16 has few slots stored only to be reloaded at once, and every
 * result must still be right.
 *
 * Memory model step 5b.1: the in[k] addresses round16 spills are ADDIs
 * of the register-resident `in`; as respill victims they are recomputed
 * at their uses (or fold into the load's displacement) instead of being
 * stored to the frame -- fifteen `addi r1, r3, N; stw r29, r1` pairs
 * before.  run-tests.sh requires a RESPILL ... remat line and no such
 * pair in round16. */
#include <stdint.h>

static uint64_t rotr(uint64_t x, int n) { return (x >> n) | (x << (64 - n)); }

#define G(a, b, c, d, x) do { \
    a = a + b + (x); d = rotr(d ^ a, 32); \
    c = c + d;       b = rotr(b ^ c, 24); \
    a = a + b;       d = rotr(d ^ a, 16); \
    c = c + d;       b = rotr(b ^ c, 63); } while (0)

uint64_t round16(const uint64_t *in, int rounds) {
    uint64_t v0 = in[0], v1 = in[1], v2 = in[2], v3 = in[3];
    uint64_t v4 = in[4], v5 = in[5], v6 = in[6], v7 = in[7];
    uint64_t v8 = in[8], v9 = in[9], v10 = in[10], v11 = in[11];
    uint64_t v12 = in[12], v13 = in[13], v14 = in[14], v15 = in[15];
    int r;
    for (r = 0; r < rounds; r++) {
        G(v0, v4, v8, v12, (uint64_t)r);
        G(v1, v5, v9, v13, (uint64_t)r * 3);
        G(v2, v6, v10, v14, (uint64_t)r * 5);
        G(v3, v7, v11, v15, (uint64_t)r * 7);
        G(v0, v5, v10, v15, (uint64_t)r * 11);
        G(v1, v6, v11, v12, (uint64_t)r * 13);
        G(v2, v7, v8, v13, (uint64_t)r * 17);
        G(v3, v4, v9, v14, (uint64_t)r * 19);
    }
    return v0 ^ v1 ^ v2 ^ v3 ^ v4 ^ v5 ^ v6 ^ v7 ^
           (v8 + v9) ^ (v10 + v11) ^ (v12 + v13) ^ (v14 + v15);
}

/* BlaMka's multiply over a block in memory, with pointers live
 * through the whole loop: spilling those would load them at every use. */
uint64_t blam(uint64_t *w, const uint64_t *k, int n) {
    int i;
    uint64_t acc = 0;
    for (i = 0; i + 3 < n; i += 4) {
        uint64_t a = w[i], b = w[i + 1], c = w[i + 2], d = w[i + 3];
        a = a + b + 2 * ((uint64_t)(uint32_t)a * (uint32_t)b);
        d = rotr(d ^ a, 32);
        c = c + d + 2 * ((uint64_t)(uint32_t)c * (uint32_t)d);
        b = rotr(b ^ c, 24) ^ k[i & 7];
        w[i] = a; w[i + 1] = b; w[i + 2] = c; w[i + 3] = d;
        acc = rotr(acc, 7) ^ a ^ d;
    }
    return acc;
}

int main(void) {
    uint64_t in[16], w[32], k[8];
    int i;
    for (i = 0; i < 16; i++) in[i] = 0x9e3779b97f4a7c15ULL * (uint64_t)(i + 1) ^ (uint64_t)(i << 20);
    for (i = 0; i < 32; i++) w[i] = (uint64_t)i * 0x0123456789abcdefULL + 77;
    for (i = 0; i < 8; i++) k[i] = (uint64_t)(i + 1) << (i * 7);
    if (round16(in, 12) != 0xd44bd7177576155eULL) return 1;
    if (blam(w, k, 32) != 0x1767eddc0cf31031ULL) return 2;
    if (w[5] != 0xf669043effe032fdULL) return 3;
    return 0;
}
