/*
 * Diagnostic: 64-bit shifts and bitwise operations
 *
 * dtoa uses these patterns extensively:
 *   carry = y >> 32;           (extract high word)
 *   lo = (ULong)(y & 0xFFFF); (extract low 16 bits)
 *   borrow = y >> 32 & 1;     (extract borrow bit)
 *   word0(x) >> Exp_shift      (exponent extraction via shift)
 *
 * If 64-bit shifts are wrong, carry/borrow propagation
 * and exponent extraction both fail.
 */
#include <stdio.h>

int main() {
    unsigned long long v;

    /* Test 1: Right shift by 32 (extract high word) */
    v = 0x123456789ABCDEF0ULL;
    printf("t1: %llx\n", v >> 32);

    /* Test 2: Right shift by 32 of a value with high bit set */
    v = 0xFEDCBA9876543210ULL;
    printf("t2: %llx\n", v >> 32);

    /* Test 3: Right shift by 16 */
    v = 0x00000000FFFF0000ULL;
    printf("t3: %llx\n", v >> 16);

    /* Test 4: Right shift by 1 */
    v = 0x8000000000000000ULL;
    printf("t4: %llx\n", v >> 1);

    /* Test 5: Left shift by 32 */
    v = 0x12345678ULL;
    printf("t5: %llx\n", v << 32);

    /* Test 6: Left shift by 1 across the 32-bit boundary */
    v = 0x80000000ULL;
    printf("t6: %llx\n", v << 1);

    /* Test 7: Borrow extraction pattern (dtoa diff()) */
    /* borrow = y >> 32 & (ULong)1; */
    v = 0x1FFFFFFFFULL;  /* bit 32 set */
    unsigned int borrow = (unsigned int)(v >> 32) & 1;
    printf("t7a: %x\n", borrow);

    v = 0x0FFFFFFFFULL;  /* bit 32 clear */
    borrow = (unsigned int)(v >> 32) & 1;
    printf("t7b: %x\n", borrow);

    /* Test 8: Mask low 32 bits (dtoa result word extraction) */
    v = 0xAAAABBBBCCCCDDDDULL;
    unsigned int lo = (unsigned int)v;
    unsigned int hi = (unsigned int)(v >> 32);
    printf("t8lo: %x\n", lo);
    printf("t8hi: %x\n", hi);

    /* Test 9: Signed right shift (arithmetic shift) */
    long long sv = (long long)0xFEDCBA9876543210ULL;
    printf("t9: %llx\n", (unsigned long long)(sv >> 32));

    /* Test 10: Variable shift amounts (compiler may handle differently) */
    v = 0xFFFFFFFF00000000ULL;
    int shift = 32;
    printf("t10: %llx\n", v >> shift);

    /* Test 11: Combined shift pattern from dtoa b2d():
     * d0 = word0(d) >> 16 | word0(d) << 16;
     * This is a 32-bit byteswap-of-halves, but let's test 64-bit version
     */
    v = 0x0000000012345678ULL;
    unsigned long long swapped = (v >> 16) | (v << 48);
    printf("t11: %llx\n", swapped & 0xFFFFFFFFFFFFFFFFULL);

    return 0;
}
