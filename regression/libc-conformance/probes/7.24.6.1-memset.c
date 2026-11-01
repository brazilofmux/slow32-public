/* 7.24.6.1 memset: c converted to unsigned char (0x1AB, -1, 256), every length 0..70 and misalignment, returns s */
#include <stdio.h>
#include <string.h>

static unsigned char b[96];

int main(void) {
    int bad = 0, badret = 0, d, n, i;
    memset(b, 0, sizeof b);
    memset(b, 0x1AB, 3);
    memset(b + 3, -1, 2);
    memset(b + 5, 256, 1);
    memset(b + 6, 0x7F, 1);
    printf("%02x %02x %02x %02x %02x %02x %02x\n", b[0], b[2], b[3], b[4], b[5], b[6], b[7]);
    for (d = 0; d < 8; d++)
        for (n = 0; n <= 70; n++) {
            memset(b, 0x11, sizeof b);
            if (memset(b + d, 0x3C5, n) != b + d) badret++;
            for (i = 0; i < 96; i++)
                if (b[i] != ((i >= d && i < d + n) ? 0xC5 : 0x11)) { bad++; break; }
        }
    printf("wrong fills %d, wrong return %d\n", bad, badret);
    return 0;
}
