/* Regression test for issue #49 / 887414ea bug 3:
 * cc-x64's SIB+ALU fold used to emit `add reg, [base+idx*scale]` for any
 * fold-eligible LOAD, including narrow ones (uint8_t / int8_t / uint16_t /
 * int16_t).  That's a 32-bit memory `add` reading a 4-byte dword, which
 * for narrow-element arrays picks up adjacent elements as garbage high
 * bits.  The fix in hir_codegen_x64.h refuses to fuse loads whose element
 * type is TY_CHAR or TY_SHORT; they must go via a separate movzx/movsx
 * load + register `add`.
 *
 * This test exercises the buggy shape (`int + (int32_t)narrow_array[i]`
 * inline, vs. factored through a local int32_t).  Before the fix, the
 * inline form returned wrong answers on uint16_t arrays whose element
 * values had non-zero high bytes that aliased adjacent entries when
 * read as a dword.  After the fix the two forms agree.
 *
 * Returns 0 on success, non-zero on any mismatch.
 */

#include <stdint.h>

static uint16_t u16[64];
static int16_t  s16[64];
static uint8_t  u8[64];
static int8_t   s8[64];

static int inline_add_u16(int x, int i) { return x + (int32_t)u16[i]; }
static int factor_add_u16(int x, int i) { int32_t t = (int32_t)u16[i]; return x + t; }

static int inline_sub_u16(int x, int i) { return x - (int32_t)u16[i]; }
static int factor_sub_u16(int x, int i) { int32_t t = (int32_t)u16[i]; return x - t; }

static int inline_and_u16(int x, int i) { return x & (int32_t)u16[i]; }
static int factor_and_u16(int x, int i) { int32_t t = (int32_t)u16[i]; return x & t; }

static int inline_or_u16(int x, int i)  { return x | (int32_t)u16[i]; }
static int factor_or_u16(int x, int i)  { int32_t t = (int32_t)u16[i]; return x | t; }

static int inline_xor_u16(int x, int i) { return x ^ (int32_t)u16[i]; }
static int factor_xor_u16(int x, int i) { int32_t t = (int32_t)u16[i]; return x ^ t; }

static int inline_add_s16(int x, int i) { return x + (int32_t)s16[i]; }
static int factor_add_s16(int x, int i) { int32_t t = (int32_t)s16[i]; return x + t; }

static int inline_add_u8(int x, int i)  { return x + (int32_t)u8[i]; }
static int factor_add_u8(int x, int i)  { int32_t t = (int32_t)u8[i]; return x + t; }

static int inline_add_s8(int x, int i)  { return x + (int32_t)s8[i]; }
static int factor_add_s8(int x, int i)  { int32_t t = (int32_t)s8[i]; return x + t; }

int main(void) {
    int i;
    for (i = 0; i < 64; i++) {
        /* 0x0101 * i fills both halves of each uint16 cell so a
         * 4-byte dword load straddling [i] and [i+1] picks up
         * a distinctly-different high half. */
        u16[i] = (uint16_t)(i * 257);
        s16[i] = (int16_t)(-(i * 257));
        u8[i]  = (uint8_t)(0xC0 + i);
        s8[i]  = (int8_t)(-(i + 1));
    }
    int idx = 7;
    int x = 1000;
    int fail = 0;

    if (inline_add_u16(x, idx) != factor_add_u16(x, idx)) fail = fail | 0x01;
    if (inline_sub_u16(x, idx) != factor_sub_u16(x, idx)) fail = fail | 0x02;
    if (inline_and_u16(x, idx) != factor_and_u16(x, idx)) fail = fail | 0x04;
    if (inline_or_u16(x, idx)  != factor_or_u16(x, idx))  fail = fail | 0x08;
    if (inline_xor_u16(x, idx) != factor_xor_u16(x, idx)) fail = fail | 0x10;
    if (inline_add_s16(x, idx) != factor_add_s16(x, idx)) fail = fail | 0x20;
    if (inline_add_u8(x, idx)  != factor_add_u8(x, idx))  fail = fail | 0x40;
    if (inline_add_s8(x, idx)  != factor_add_s8(x, idx))  fail = fail | 0x80;

    return fail;
}
