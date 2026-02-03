/*
 * Diagnostic: 16-bit store/load via pointer cast
 *
 * dtoa's Storeinc macro does:
 *   ((unsigned short *)a)[1] = (unsigned short)b;
 *   ((unsigned short *)a)[0] = (unsigned short)c;
 *
 * This packs two 16-bit values into a 32-bit word.
 * If 16-bit stores (STH) or byte ordering within a word are wrong,
 * all of dtoa's Bigint arithmetic is corrupted.
 */
#include <stdio.h>

/* Reproduce exactly what Storeinc does (IEEE_8087 version) */
static void storeinc_8087(unsigned int *a, unsigned int b, unsigned int c) {
    ((unsigned short *)a)[1] = (unsigned short)b;
    ((unsigned short *)a)[0] = (unsigned short)c;
}

int main() {
    unsigned int word;
    unsigned short *hp;

    /* Test 1: Basic 16-bit store and read back */
    word = 0;
    hp = (unsigned short *)&word;
    hp[0] = 0x1234;
    printf("t1a: %08x\n", word);  /* expect low half set */

    word = 0;
    hp[1] = 0xABCD;
    printf("t1b: %08x\n", word);  /* expect high half set */

    /* Test 2: Both halves */
    word = 0;
    hp[0] = 0x5678;
    hp[1] = 0x1234;
    printf("t2: %08x\n", word);   /* expect 0x12345678 on LE */

    /* Test 3: Storeinc pattern exactly as dtoa uses it */
    word = 0xFFFFFFFF;
    storeinc_8087(&word, 0x0042, 0x0037);
    printf("t3: %08x\n", word);   /* expect 0x00420037 on LE */

    /* Test 4: Read back via unsigned short pointer */
    word = 0xDEADBEEF;
    hp = (unsigned short *)&word;
    printf("t4lo: %04x\n", (unsigned int)hp[0]);  /* expect BEEF on LE */
    printf("t4hi: %04x\n", (unsigned int)hp[1]);  /* expect DEAD on LE */

    /* Test 5: Storeinc with realistic dtoa multiply values
     * In dtoa mult(): z = x * y; Storeinc(xc, z>>16, z&0xffff)
     * Simulating: 0xFFFF * 0xFFFF = 0xFFFE0001
     * high16 = 0xFFFE, low16 = 0x0001
     */
    word = 0;
    storeinc_8087(&word, 0xFFFE, 0x0001);
    printf("t5: %08x\n", word);   /* expect FFFE0001 on LE */

    /* Test 6: Chained Storeinc (two consecutive words) */
    unsigned int arr[2] = {0, 0};
    unsigned int *p = arr;
    /* Simulate: Storeinc(p, 0x0011, 0x0022) then Storeinc(p, 0x0033, 0x0044) */
    storeinc_8087(p, 0x0011, 0x0022); p++;
    storeinc_8087(p, 0x0033, 0x0044);
    printf("t6a: %08x\n", arr[0]);  /* expect 00110022 */
    printf("t6b: %08x\n", arr[1]);  /* expect 00330044 */

    return 0;
}
