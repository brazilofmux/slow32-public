/* 64-bit division through the runtime's __udivdi3/__umoddi3/__divdi3/__moddi3
 * at the shapes its fast paths take: a 32-bit divisor with a 64-bit dividend
 * (the hardware-stepped path), both narrow, both wide, and signed. */
void debug_char(char c);
static void put(const char *s) { while (*s) debug_char(*s++); }
static void puthex(unsigned long long v) { for (int i = 60; i >= 0; i -= 4) debug_char("0123456789abcdef"[(v >> i) & 15]); }

static volatile struct { unsigned long long n, d, q, r; } U[] = {
    { 0xffffffffffffffffULL, 0x3ULL, 0x5555555555555555ULL, 0x0ULL },
    { 0x123456789abcdef0ULL, 0xaULL, 0x1d208a5a912e318ULL, 0x0ULL },
    { 0x123456789abcdef0ULL, 0x3b9aca00ULL, 0x4e2fff93ULL, 0x1ba4e0f0ULL },
    { 0x8000000000000000ULL, 0x7ULL, 0x1249249249249249ULL, 0x1ULL },
    { 0xffffffffffffffffULL, 0xffffffffULL, 0x100000001ULL, 0x0ULL },
    { 0x100000000ULL, 0xffffffffULL, 0x1ULL, 0x1ULL },
    { 0xffffffff00000000ULL, 0x80000001ULL, 0x1fffffffaULL, 0x6ULL },
    { 0xde0b6b3a7640000ULL, 0x3b9aca00ULL, 0x3b9aca00ULL, 0x0ULL },
    { 0x123456789abcdef0ULL, 0x100000000ULL, 0x12345678ULL, 0x9abcdef0ULL },
    { 0x123456789abcdef0ULL, 0x123456789ULL, 0x10000000ULL, 0xabcdef0ULL },
    { 0x3039ULL, 0x2a6ULL, 0x12ULL, 0x8dULL },
    { 0xffffffffULL, 0xffffffffULL, 0x1ULL, 0x0ULL },
    { 0x5ULL, 0x7ULL, 0x0ULL, 0x5ULL },
    { 0x10000000000ULL, 0x100000ULL, 0x100000ULL, 0x0ULL },
    { 0xfedcba9876543210ULL, 0xffffULL, 0xfeddb9762fcaULL, 0x61daULL },
    { 0xfedcba9876543210ULL, 0x10001ULL, 0xfedbbbbcba97ULL, 0x7779ULL },
    { 0xf2a74de452e6b438ULL, 0x269e0d37ULL, 0x648957000ULL, 0x41ba438ULL },
    { 0xa6a3a4506513270eULL, 0xc5c7fd0ULL, 0xd7b0eab8aULL, 0x47650eeULL },
    { 0xd23f0824128b2f33ULL, 0x892f902bULL, 0x1885636dfULL, 0x434187beULL },
    { 0x5d9dc9f81818e811ULL, 0x9531985dULL, 0xa0a2a70eULL, 0xa23e7fbULL },
    { 0xe8e25d940ed90475ULL, 0x81e74ef5ULL, 0x1caf17d43ULL, 0x771cb956ULL },
    { 0x99950d836f675ccULL, 0x1600a35aULL, 0x6faefb1dULL, 0xb8eb69aULL },
    { 0x6b0d549b6f03675aULL, 0x11e20b8fULL, 0x5fc7afd8bULL, 0xdb4cdb5ULL },
    { 0x1738f7d93d9c1724ULL, 0x8d116eceULL, 0x2a247d9cULL, 0x62e9fb9cULL },
    { 0xf21ddb66cad4a26ULL, 0x90d3ac94afULL, 0x1abf8dULL, 0x7cf9ead4c3ULL },
    { 0xf28c105d1fb17c23ULL, 0xa139263059ULL, 0x1812187ULL, 0x8b5a798434ULL },
    { 0x953f48f1a09f76b5ULL, 0xff29d0da9ULL, 0x95bc902ULL, 0x59f98aa63ULL },
    { 0x95e60af593bd04cfULL, 0xc658cda14ULL, 0xc178271ULL, 0xab20699fbULL },
};
static volatile struct { long long a, b, q, r; } S[] = {
    { -1000000000000000000LL, 1000000000LL, -1000000000LL, 0LL },
    { -1311768467463790320LL, 10LL, -131176846746379032LL, 0LL },
    { 1311768467463790320LL, -1000LL, -1311768467463790LL, 320LL },
    { -7LL, 2LL, -3LL, -1LL },
    { 7LL, -2LL, -3LL, 1LL },
    { -9223372036854775808LL, 1LL, -9223372036854775808LL, 0LL },
    { -4611686018427387904LL, -2147483648LL, 2147483648LL, 0LL },
};

int main(void) {
    int bad = 0;
    for (unsigned i = 0; i < sizeof U / sizeof U[0]; i++) {
        unsigned long long n = U[i].n, d = U[i].d;
        if (n / d != U[i].q || n % d != U[i].r) {
            bad++; put("U"); puthex(n); put("/"); puthex(d); put(" got "); puthex(n / d); put(" r "); puthex(n % d); put("\n");
        }
    }
    for (unsigned i = 0; i < sizeof S / sizeof S[0]; i++) {
        long long a = S[i].a, b = S[i].b;
        if (a / b != S[i].q || a % b != S[i].r) {
            bad++; put("S"); puthex((unsigned long long)a); put("/"); puthex((unsigned long long)b); put(" got "); puthex((unsigned long long)(a / b)); put("\n");
        }
    }
    put(bad ? "FAIL\n" : "PASS\n");
    return bad != 0;
}
