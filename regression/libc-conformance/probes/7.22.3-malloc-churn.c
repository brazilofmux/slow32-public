/* 7.22.3 memory management: many allocations of mixed sizes stay disjoint and keep their contents */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define N 300
static unsigned char *blk[N];
static size_t len[N];
static unsigned seed = 12345;
static unsigned next(void) { seed = seed * 1103515245u + 12345u; return (seed >> 8) & 0xffff; }
int main(void) {
    int round, i, ok = 1;
    for (round = 0; round < 6; round++) {
        for (i = 0; i < N; i++) {
            if (blk[i] && (next() & 1)) {
                size_t k;
                for (k = 0; k < len[i]; k++) if (blk[i][k] != (unsigned char)i) ok = 0;
                if (next() & 1) { free(blk[i]); blk[i] = 0; }
                else {
                    size_t nl = next() % 700 + 1;
                    unsigned char *p = realloc(blk[i], nl);
                    if (!p) return 2;
                    for (k = 0; k < (nl < len[i] ? nl : len[i]); k++) if (p[k] != (unsigned char)i) ok = 0;
                    memset(p, i, nl);
                    blk[i] = p; len[i] = nl;
                }
            } else if (!blk[i]) {
                len[i] = next() % 500 + 1;
                blk[i] = (next() & 1) ? malloc(len[i]) : calloc(len[i], 1);
                if (!blk[i]) return 3;
                memset(blk[i], i, len[i]);
            }
        }
    }
    for (i = 0; i < N; i++) {
        size_t k;
        if (blk[i]) for (k = 0; k < len[i]; k++) if (blk[i][k] != (unsigned char)i) ok = 0;
        free(blk[i]);
    }
    printf("intact: %d\n", ok);
    return 0;
}
