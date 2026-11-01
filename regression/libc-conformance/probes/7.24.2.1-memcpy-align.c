/* 7.24.2.1 memcpy: copies exactly n bytes for every length 0..70 and every source/destination misalignment, returns s1 */
#include <stdio.h>
#include <string.h>

static unsigned char src[128], dst[128];

int main(void) {
    int bad = 0, badret = 0, so, d, n, i;
    for (i = 0; i < 128; i++) src[i] = (unsigned char)(i * 37 + 11);
    for (so = 0; so < 8; so++)
        for (d = 0; d < 8; d++)
            for (n = 0; n <= 70; n++) {
                memset(dst, 0xEE, sizeof dst);
                if (memcpy(dst + d, src + so, n) != dst + d) badret++;
                for (i = 0; i < 128; i++) {
                    unsigned char want = (i >= d && i < d + n) ? src[so + i - d] : 0xEE;
                    if (dst[i] != want) { bad++; break; }
                }
            }
    printf("wrong copies %d, wrong return %d\n", bad, badret);
    return 0;
}
