/* 7.24.4.1 memcmp: bytes compare as unsigned char, nulls do not stop it, n == 0 is equal, every misalignment */
#include <stdio.h>
#include <string.h>

static int sgn(int v) { return (v > 0) - (v < 0); }

int main(void) {
    unsigned char a[64], b[64];
    int bad = 0, x, y, n, k, i;
    printf("%d %d %d ", sgn(memcmp("\x80", "\x7f", 1)), sgn(memcmp("\xff", "\x01", 1)), sgn(memcmp("a\0b", "a\0c", 3)));
    printf("%d %d\n", memcmp("xyz", "abc", 0), sgn(memcmp("\x01\x02", "\x01\xfe", 2)));
    /* first difference at every position, for lengths 1..24 and every pair of offsets */
    for (x = 0; x < 4; x++)
        for (y = 0; y < 4; y++)
            for (n = 1; n <= 24; n++)
                for (k = 0; k < n; k++) {
                    for (i = 0; i < 64; i++) a[i] = b[i] = (unsigned char)(0x70 + i);
                    a[x + k] = 0x90; b[y + k] = 0x10;
                    for (i = 0; i < n; i++) if (i != k) a[x + i] = b[y + i] = (unsigned char)(i * 9);
                    if (sgn(memcmp(a + x, b + y, n)) != 1 || sgn(memcmp(b + y, a + x, n)) != -1) bad++;
                    if (memcmp(a + x, b + y, k) != 0) bad++;
                }
    printf("wrong %d\n", bad);
    return 0;
}
