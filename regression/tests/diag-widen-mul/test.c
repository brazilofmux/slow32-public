/*
 * Diagnostic: 32x32 -> 64-bit widening multiply + 64-bit add
 *
 * dtoa's inner loops do:
 *   y = *x * (ULLong)m + carry;
 *   z = *x++ * (ULLong)y + *xc + carry;
 *
 * These are 32-bit values zero-extended to 64-bit, multiplied,
 * then added to a 64-bit carry. If the widening multiply or
 * the subsequent 64-bit add is wrong, all Bigint arithmetic fails.
 *
 * At -O0, these may lower to __muldi3 calls rather than UMUL_LOHI.
 */
#include <stdio.h>

int main() {
    unsigned long long r;
    unsigned int a, b;

    /* Test 1: Small multiply (fits in 32 bits) */
    a = 100; b = 200;
    r = (unsigned long long)a * (unsigned long long)b;
    printf("t1: %llx\n", r);

    /* Test 2: Multiply producing >32 bit result */
    a = 0x10000; b = 0x10000;
    r = (unsigned long long)a * (unsigned long long)b;
    printf("t2: %llx\n", r);

    /* Test 3: Large operands, full 64-bit result */
    a = 0xFFFFFFFF; b = 0xFFFFFFFF;
    r = (unsigned long long)a * (unsigned long long)b;
    printf("t3: %llx\n", r);

    /* Test 4: Asymmetric operands */
    a = 0xFFFFFFFF; b = 2;
    r = (unsigned long long)a * (unsigned long long)b;
    printf("t4: %llx\n", r);

    /* Test 5: Multiply + add (dtoa pattern: y = *x * (ULLong)m + carry) */
    a = 0xFFFF; b = 0xFFFF;
    r = (unsigned long long)a * (unsigned long long)b + 0xFFFFFFFFULL;
    printf("t5: %llx\n", r);

    /* Test 6: Multiply + add with large carry */
    a = 0xFFFFFFFF; b = 0xFFFFFFFF;
    r = (unsigned long long)a * (unsigned long long)b + 0xFFFFFFFFULL;
    printf("t6: %llx\n", r);

    /* Test 7: Extract high and low 32-bit halves (dtoa carry extraction) */
    r = 0x123456789ABCDEF0ULL;
    unsigned int lo = (unsigned int)r;
    unsigned int hi = (unsigned int)(r >> 32);
    printf("t7lo: %08x\n", lo);
    printf("t7hi: %08x\n", hi);

    /* Test 8: dtoa-realistic: Bigint single-word multiply chain
     * y = x[i] * m + carry; result[i] = (uint32)y; carry = y >> 32;
     */
    unsigned int x0 = 0x12345678;
    unsigned int x1 = 0x9ABCDEF0;
    unsigned int m = 10;
    unsigned long long carry = 0;
    unsigned long long y;

    y = (unsigned long long)x0 * m + carry;
    printf("t8a: %llx\n", y);
    carry = y >> 32;

    y = (unsigned long long)x1 * m + carry;
    printf("t8b: %llx\n", y);

    return 0;
}
