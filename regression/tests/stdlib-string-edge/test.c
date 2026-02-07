#include <stdio.h>
#include <string.h>

int main() {
    char src[64];
    char dst[64];

    /* memcpy size 0 */
    memset(dst, 'X', 8);
    memcpy(dst, "ABC", 0);
    printf("%s: memcpy size 0\n", dst[0] == 'X' ? "PASS" : "FAIL");

    /* memcpy size 1 */
    memset(dst, 'X', 8);
    memcpy(dst, "A", 1);
    printf("%s: memcpy size 1\n", dst[0] == 'A' && dst[1] == 'X' ? "PASS" : "FAIL");

    /* memcpy odd sizes */
    const char *pat = "abcdefghijklmnopqrstuvwxyz01234";
    memset(dst, 0, 64);
    memcpy(dst, pat, 3);
    printf("%s: memcpy size 3\n", memcmp(dst, "abc", 3) == 0 ? "PASS" : "FAIL");

    memset(dst, 0, 64);
    memcpy(dst, pat, 7);
    printf("%s: memcpy size 7\n", memcmp(dst, "abcdefg", 7) == 0 ? "PASS" : "FAIL");

    memset(dst, 0, 64);
    memcpy(dst, pat, 15);
    printf("%s: memcpy size 15\n", memcmp(dst, "abcdefghijklmno", 15) == 0 ? "PASS" : "FAIL");

    memset(dst, 0, 64);
    memcpy(dst, pat, 31);
    printf("%s: memcpy size 31\n", memcmp(dst, pat, 31) == 0 ? "PASS" : "FAIL");

    /* memset size 0 */
    memset(dst, 'X', 8);
    memset(dst, 'A', 0);
    printf("%s: memset size 0\n", dst[0] == 'X' ? "PASS" : "FAIL");

    /* memset size 1 */
    memset(dst, 'X', 8);
    memset(dst, 'A', 1);
    printf("%s: memset size 1\n", dst[0] == 'A' && dst[1] == 'X' ? "PASS" : "FAIL");

    /* memset odd sizes */
    memset(dst, 0, 64);
    memset(dst, 'B', 3);
    printf("%s: memset size 3\n", dst[0] == 'B' && dst[1] == 'B' && dst[2] == 'B' && dst[3] == 0 ? "PASS" : "FAIL");

    memset(dst, 0, 64);
    memset(dst, 'C', 7);
    int ok = 1;
    for (int i = 0; i < 7; i++) if (dst[i] != 'C') ok = 0;
    printf("%s: memset size 7\n", ok && dst[7] == 0 ? "PASS" : "FAIL");

    memset(dst, 0, 64);
    memset(dst, 'D', 15);
    ok = 1;
    for (int i = 0; i < 15; i++) if (dst[i] != 'D') ok = 0;
    printf("%s: memset size 15\n", ok && dst[15] == 0 ? "PASS" : "FAIL");

    memset(dst, 0, 64);
    memset(dst, 'E', 31);
    ok = 1;
    for (int i = 0; i < 31; i++) if (dst[i] != 'E') ok = 0;
    printf("%s: memset size 31\n", ok && dst[31] == 0 ? "PASS" : "FAIL");

    /* memmove overlapping forward (src < dst, regions overlap) */
    memcpy(dst, "ABCDEFGHIJ", 10);
    memmove(dst + 3, dst, 7);  /* ABCABCDEFG */
    printf("%s: memmove overlap fwd\n",
           memcmp(dst, "ABCABCDEFG", 10) == 0 ? "PASS" : "FAIL");

    /* memmove overlapping backward (dst < src, regions overlap) */
    memcpy(dst, "ABCDEFGHIJ", 10);
    memmove(dst, dst + 3, 7);  /* DEFGHIJHIJ */
    printf("%s: memmove overlap bwd\n",
           memcmp(dst, "DEFGHIJHIJ", 10) == 0 ? "PASS" : "FAIL");

    /* memmove size 0 */
    memcpy(dst, "XY", 2);
    memmove(dst, dst + 1, 0);
    printf("%s: memmove size 0\n", dst[0] == 'X' ? "PASS" : "FAIL");

    /* memcmp size 0 */
    printf("%s: memcmp size 0\n", memcmp("abc", "xyz", 0) == 0 ? "PASS" : "FAIL");

    /* strlen empty string */
    printf("%s: strlen empty\n", strlen("") == 0 ? "PASS" : "FAIL");

    /* strcpy empty string */
    strcpy(dst, "");
    printf("%s: strcpy empty\n", dst[0] == '\0' ? "PASS" : "FAIL");

    /* strcpy then strlen */
    strcpy(dst, "");
    printf("%s: strlen after strcpy empty\n", strlen(dst) == 0 ? "PASS" : "FAIL");

    return 0;
}
