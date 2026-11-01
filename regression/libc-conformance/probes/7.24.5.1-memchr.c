/* 7.24.5.1 memchr: c and the bytes compare as unsigned char (c of -23, 0xE9, 0x1E9), nulls do not stop it, n bounds it */
#include <stdio.h>
#include <string.h>

static long at(const void *p, const unsigned char *base) {
    return p ? (long)((const unsigned char *)p - base) : -1L;
}

int main(void) {
    static const unsigned char s[] = { 'a', 0, 'b', 0xE9, 'c', 0xFF, 'd', 0x80 };
    unsigned char big[100];
    int i, bad = 0, k;
    printf("%ld %ld %ld ", at(memchr(s, 0xE9, 8), s), at(memchr(s, -23, 8), s), at(memchr(s, 0x1E9, 8), s));
    printf("%ld %ld %ld ", at(memchr(s, 'b', 8), s), at(memchr(s, 0, 8), s), at(memchr(s + 2, 0, 6), s));
    printf("%ld %ld %ld ", at(memchr(s, -1, 8), s), at(memchr(s, 0x80, 8), s), at(memchr(s, 'd', 6), s));
    printf("%ld\n", at(memchr(s, 'a', 0), s));
    /* every position and offset in a longer block */
    for (k = 0; k < 4; k++)
        for (i = k; i < 100; i++) {
            memset(big, 0x55, sizeof big);
            big[i] = 0xC3;
            if (memchr(big + k, 0xC3, 100 - k) != big + i) bad++;
            if (memchr(big + k, 0xC3, i - k) != NULL) bad++;
        }
    printf("wrong %d\n", bad);
    return 0;
}
