/*
 * Diagnostic: Reproduce dtoa's USE_BF96 entry path
 *
 * dtoa_r does:
 *   U u;
 *   u.d = dd;
 *   dbits = (u.LL & 0xfffffffffffffULL) << 11;
 *   be = u.LL >> 52;
 *
 * If be == 0, dtoa thinks the number is denormalized and takes
 * a completely wrong code path. For 1.25, be should be 1023.
 *
 * This test reproduces exactly that pattern.
 */
#include <stdio.h>

typedef union {
    double d;
    unsigned int L[2];
    unsigned long long LL;
} U;

/* Test the extraction inline */
void __attribute__((noinline)) test_extract(double dd) {
    U u;
    u.d = dd;

    /* Print raw union members */
    printf("L0=%08x L1=%08x LL=%llx\n", u.L[0], u.L[1], u.LL);

    /* Reproduce dtoa's BF96 extraction */
    unsigned long long dbits = (u.LL & 0xfffffffffffffULL) << 11;
    unsigned long long be_val = u.LL >> 52;
    printf("be=%lld dbits=%llx\n", (long long)be_val, dbits);

    /* What dtoa does next for normal numbers (be != 0) */
    if (be_val) {
        dbits |= 0x8000000000000000ULL;
        printf("normal: dbits=%llx\n", dbits);
    } else {
        printf("DENORM (this is wrong for 1.25!)\n");
    }
}

/* Test with receiving double through a chain (like dtoa -> dtoa_r) */
void __attribute__((noinline)) inner(double dd, int mode, int ndigits,
                                      int *decpt, int *sign) {
    U u;
    u.d = dd;
    unsigned long long be_val = u.LL >> 52;
    unsigned long long dbits = (u.LL & 0xfffffffffffffULL) << 11;
    printf("inner: LL=%llx be=%lld\n", u.LL, (long long)be_val);
    *decpt = (int)be_val;
    *sign = mode + ndigits;  /* just to use the args */
}

void __attribute__((noinline)) outer(double dd, int mode, int ndigits,
                                      int *decpt, int *sign) {
    inner(dd, mode, ndigits, decpt, sign);
}

int main() {
    /* Test 1: Direct extraction */
    printf("--- 1.25 ---\n");
    test_extract(1.25);

    printf("--- 1.5 ---\n");
    test_extract(1.5);

    printf("--- 12345.0 ---\n");
    test_extract(12345.0);

    /* Test 2: Through chain (simulates dtoa -> dtoa_r) */
    int decpt = 0, sign = 0;
    outer(1.25, 2, 7, &decpt, &sign);
    printf("chain: decpt=%d sign=%d\n", decpt, sign);

    /* Test 3: Construct known 64-bit value via union, verify LL */
    U manual;
    manual.L[0] = 0x00000000;
    manual.L[1] = 0x3FF40000;
    printf("manual: LL=%llx be=%lld\n", manual.LL,
           (long long)(manual.LL >> 52));

    return 0;
}
