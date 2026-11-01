/* Decimal floating constants rounded once, to the nearest (ties to even):
 * the lexer's conversion corrected against the halfway points in big
 * integers (c_lexer.rl lex_round_exact).  1e23 came out one ulp high, a
 * float literal rounded twice (through double), and a subnormal float in
 * a static initializer came out 0 -- stage07, which builds this compiler,
 * flushes subnormals in its float conversions (ps_d2f_bits, sema_float_bits). */
#include <string.h>

static unsigned hi_of(double d) { unsigned w[2]; memcpy(w, &d, 8); return w[1]; }
static unsigned lo_of(double d) { unsigned w[2]; memcpy(w, &d, 8); return w[0]; }
static unsigned bits_of(float f) { unsigned u; memcpy(&u, &f, 4); return u; }

static const double sd[] = { 1e23, 8.98846567431158e307, 2.4703282292062328e-324,
                             2.4703282292062327e-324, 1.7976931348623158e308,
                             9007199254740993.0, 0.1000000000000000055511151231257827021181583404541015625,
                             2.2250738585072012e-308 };   /* the 2011 one: PHP and Java looped on it */
static const float sf[] = { 1e-45f, 1.00000005960464477539062501f, 3.4028235e38f, 1e23f, 7.1e-46f, 7e-46f };
static const float sz[] = { 2.5f, 0.0f, 1.0f };
static float s_sub = 1e-38f / 100.0f;
static double s_negz = -(0.0);

int main(void) {
    volatile double d;
    volatile float f;
    if (hi_of(sd[0]) != 0x44b52d02u || lo_of(sd[0]) != 0xc7e14af6u) return 1;     /* 1e23 */
    if (hi_of(sd[1]) != 0x7fe00000u || lo_of(sd[1]) != 0x00000000u) return 2;
    if (hi_of(sd[2]) != 0u || lo_of(sd[2]) != 1u) return 3;          /* just above half: the least */
    if (hi_of(sd[3]) != 0u || lo_of(sd[3]) != 0u) return 4;          /* just below: zero */
    if (hi_of(sd[4]) != 0x7fefffffu || lo_of(sd[4]) != 0xffffffffu) return 5;
    if (lo_of(sd[5]) != 0u || hi_of(sd[5]) != 0x43400000u) return 6; /* 2^53 + 1: a tie, to even */
    if (hi_of(sd[6]) != 0x3fb99999u || lo_of(sd[6]) != 0x9999999au) return 7;
    if (hi_of(sd[7]) != 0x00100000u || lo_of(sd[7]) != 0u) return 18;    /* DBL_MIN */
    if (bits_of(sf[0]) != 1u) return 8;
    if (bits_of(sf[1]) != 0x3f800001u) return 9;    /* above the tie: up, not to even */
    if (bits_of(sf[2]) != 0x7f7fffffu) return 10;
    if (bits_of(sf[3]) != 0x65a96816u) return 11;
    if (bits_of(sf[4]) != 1u) return 12;            /* 7.1e-46: above half the least */
    if (bits_of(sf[5]) != 0u) return 19;            /* 7e-46: below it */
    if (bits_of(s_sub) != 0x000116c2u) return 13;
    if (hi_of(s_negz) != 0x80000000u) return 14;
    d = 1e23;
    if (hi_of(d) != 0x44b52d02u || lo_of(d) != 0xc7e14af6u) return 15;
    f = 1.00000005960464477539062501f;
    if (bits_of(f) != 0x3f800001u) return 16;
    f = 1e-45f;
    if (bits_of(f) != 1u) return 17;
    f = 2.5f;
    f = 0.0f;                       /* a zero after another float: its own bits */
    if (bits_of(f) != 0u) return 20;
    if (bits_of(sz[1]) != 0u || bits_of(sz[2]) != 0x3f800000u) return 21;
    return 0;
}
