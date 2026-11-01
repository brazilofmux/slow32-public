/* 7.24.2.3 strcpy: copies through the terminating null and no further, at any alignment, returns s1 */
#include <stdio.h>
#include <string.h>

int main(void) {
    char src[48], dst[64];
    int bad = 0, badret = 0, so, d, n, i;
    for (so = 0; so < 4; so++)
        for (d = 0; d < 4; d++)
            for (n = 0; n <= 20; n++) {
                for (i = 0; i < n; i++) src[so + i] = (char)('A' + i);
                src[so + n] = '\0';
                memset(dst, '#', sizeof dst);
                if (strcpy(dst + d, src + so) != dst + d) badret++;
                for (i = 0; i < 64; i++) {
                    char want = (i < d || i > d + n) ? '#' : (i == d + n ? '\0' : (char)('A' + i - d));
                    if (dst[i] != want) { bad++; break; }
                }
            }
    printf("wrong copies %d, wrong return %d\n", bad, badret);
    return 0;
}
