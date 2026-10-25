/* 64-bit shifts by a count known only at run time are lowered inline
 * (hl_ll_shift_var), not as __ashldi3/__lshrdi3/__ashrdi3 calls, and a
 * count that becomes constant once a tiny static is inlined must fold
 * to the right constant shift.  8-byte parameters now get 8-byte slots
 * so their word pairs promote; one here has its address taken and one
 * is assigned, which must still behave.  The reference shifts a pair
 * of 32-bit words one bit at a time, so it never uses the code under
 * test. */
typedef unsigned long long u64;
typedef long long s64;

static unsigned ref_lo, ref_hi;

static void ref_shift(unsigned lo, unsigned hi, int n, int kind) {
    int i;
    for (i = 0; i < n; i++) {
        if (kind == 0) {                       /* << */
            hi = (hi << 1) | (lo >> 31);
            lo = lo << 1;
        } else {                               /* >> logical (1) or arithmetic (2) */
            lo = (lo >> 1) | (hi << 31);
            hi = (kind == 2) ? (unsigned)((int)hi >> 1) : hi >> 1;
        }
    }
    ref_lo = lo;
    ref_hi = hi;
}

static int same(u64 v) {
    return (unsigned)v == ref_lo && (unsigned)(v >> 32) == ref_hi;
}

static u64 rotr(u64 x, int c) { return (x >> c) | (x << (64 - c)); }
static u64 rotl(u64 x, int c) { return (x << c) | (x >> (64 - c)); }

static int counts[5] = { 3, 29, 32, 45, 63 };

static u64 through_addr(u64 x, int c) {
    u64 *p;
    p = &x;
    *p = *p >> c;
    return x;
}

static s64 reassign(s64 x, int c) {
    x = x >> c;
    x = x + 7;
    return x;
}

int main(void) {
    unsigned pats[4][2];
    int p;
    int n;
    u64 v;
    s64 sv;
    u64 a;

    pats[0][0] = 0x7d1c39e5u; pats[0][1] = 0x9b04f6a2u;
    pats[1][0] = 0x00000001u; pats[1][1] = 0x00000000u;
    pats[2][0] = 0xffffffffu; pats[2][1] = 0x80000000u;
    pats[3][0] = 0x2468ace1u; pats[3][1] = 0x13579bdfu;

    for (p = 0; p < 4; p++) {
        v = ((u64)pats[p][1] << 32) | pats[p][0];
        for (n = 0; n < 64; n++) {
            ref_shift(pats[p][0], pats[p][1], n, 0);
            if (!same(v << n)) return 1;
            ref_shift(pats[p][0], pats[p][1], n, 1);
            if (!same(v >> n)) return 2;
            ref_shift(pats[p][0], pats[p][1], n, 2);
            sv = (s64)v;
            if (!same((u64)(sv >> n))) return 3;
            a = v;
            a <<= n;
            ref_shift(pats[p][0], pats[p][1], n, 0);
            if (!same(a)) return 4;
            sv = (s64)v;
            sv >>= n;
            ref_shift(pats[p][0], pats[p][1], n, 2);
            if (!same((u64)sv)) return 5;
        }
    }

    /* Constant counts after inlining: each side of the 32 boundary,
     * and 32 itself. */
    v = 0x0fedcba987654321ull;
    if (rotr(v, 8) != 0x210fedcba9876543ull) return 6;
    if (rotr(v, 32) != 0x876543210fedcba9ull) return 7;
    if (rotr(v, 44) != 0xba9876543210fedcull) return 8;
    if (rotl(v, 4) != 0xfedcba9876543210ull) return 9;
    if (rotl(v, 60) != 0x10fedcba98765432ull) return 10;
    if (rotr(v, 1) != 0x87f6e5d4c3b2a190ull) return 11;
    if (rotr(v, 63) != 0x1fdb97530eca8642ull) return 12;

    /* Same rotates with the count from memory. */
    for (p = 0; p < 5; p++) {
        n = counts[p];
        if (rotl(rotr(v, n), n) != v) return 13;
    }

    if (through_addr(0xc000000000000000ull, 61) != 6) return 14;
    if (reassign(-0x100000000ll, 4) != -0x10000000ll + 7) return 15;
    return 0;
}
