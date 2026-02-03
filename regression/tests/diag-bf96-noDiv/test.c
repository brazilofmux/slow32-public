/*
 * Diagnostic: Reproduce dtoa's BF96 use_exact/no_div path for 1.25
 *
 * For 1.25 (mode=2, ndigits=7):
 *   be=1023, dbits=0xA000000000000000, k=0, j=52, ilim=7
 *   Code goes: use_exact -> no_div (k<=0)
 *
 * no_div loop:
 *   res = dbits >> 11 = 0x0014000000000000
 *   loop:
 *     dig = res >> j
 *     *s++ = '0' + dig
 *     res -= dig << j
 *     if (res == 0) -> done
 *     --j; ++i;
 *     res *= 5;
 *
 * Expected digits: "125"
 */
#include <stdio.h>

int main() {
    /* Exactly replicate the BF96 no_div loop */
    unsigned long long dbits = 0xA000000000000000ULL;
    unsigned long long res = dbits >> 11;
    int j = 52;
    int ilim = 7;
    int i = 1;
    unsigned long long ulp = 1;
    char buf[32];
    char *s = buf;

    printf("res_init: %llx\n", res);

    /* no_div loop - exact reproduction of lines 5447-5503 */
    for (;;) {
        unsigned long long den = res >> j;
        int dig = (int)den;
        *s++ = '0' + dig;
        printf("i=%d j=%d dig=%d res=%llx den=%llx\n", i, j, dig, res, den);

        if (!(res -= den << j))
            break;

        --j;
        if (i == ilim) {
            /* rounding check would go here; for exact 1.25, we won't reach this
               with only 3 digits generated */
            printf("REACHED_ILIM\n");
            break;
        }
        ++i;
        res *= 5;
        ulp *= 5;
    }

    *s = '\0';
    printf("digits: %s\n", buf);

    /* Also test the multiply by 5 in isolation */
    unsigned long long v = 0x0004000000000000ULL;
    printf("mul5: %llx\n", v * 5);

    /* And shift by variable */
    v = 0x0014000000000000ULL;
    int shift = 52;
    printf("shr52: %llx\n", v >> shift);
    shift = 51;
    printf("shr51: %llx\n", v >> shift);

    return 0;
}
