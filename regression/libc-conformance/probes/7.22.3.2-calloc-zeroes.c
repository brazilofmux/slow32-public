/* 7.22.3.2 calloc: the space is all bits zero, even when it reuses memory freed dirty */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

int main(void) {
    int round, ok = 1;
    for (round = 0; round < 20; round++) {
        size_t n = 16 + round * 37;
        unsigned char *p = malloc(n), *q;
        size_t i;
        if (!p) return 2;
        memset(p, 0xA5, n);
        free(p);
        q = calloc(n, 1);
        if (!q) return 3;
        for (i = 0; i < n; i++) if (q[i]) ok = 0;
        free(q);
        q = calloc(round + 1, 24);
        for (i = 0; i < (size_t)(round + 1) * 24; i++) if (q[i]) ok = 0;
        memset(q, 0xff, (size_t)(round + 1) * 24);
        free(q);
    }
    printf("zeroed: %d\n", ok);
    return 0;
}
