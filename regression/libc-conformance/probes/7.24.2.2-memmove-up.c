/* 7.24.2.2 memmove: overlapping move to a higher address (s1 > s2), every distance 1..9, length 0..40, start 0..3 */
#include <stdio.h>
#include <string.h>

static unsigned char buf[96], ref[96];

int main(void) {
    int bad = 0, badret = 0, dist, n, st, i;
    for (dist = 1; dist <= 9; dist++)
        for (n = 0; n <= 40; n++)
            for (st = 0; st < 4; st++) {
                for (i = 0; i < 96; i++) buf[i] = ref[i] = (unsigned char)(i + 1);
                for (i = 0; i < n; i++) ref[st + dist + i] = (unsigned char)(st + i + 1);
                if (memmove(buf + st + dist, buf + st, n) != buf + st + dist) badret++;
                if (memcmp(buf, ref, sizeof buf) != 0) {
                    if (!bad) printf("first wrong: distance %d length %d start %d\n", dist, n, st);
                    bad++;
                }
            }
    printf("wrong moves %d, wrong return %d\n", bad, badret);
    return 0;
}
