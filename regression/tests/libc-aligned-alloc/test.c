/* aligned_alloc and posix_memalign: alignment, the whole block usable,
 * free() taking the result, and the heap intact afterwards. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

int main(void) {
    int bad = 0;
    void *keep[64];
    for (int round = 0; round < 3; round++) {
        for (int i = 0; i < 64; i++) {
            size_t al = (size_t)8 << (i % 6);        /* 8 .. 256 */
            size_t sz = 1 + (size_t)i * 37;
            if (sz % al) sz += al - sz % al;         /* C11: a multiple */
            char *p = aligned_alloc(al, sz);
            if (!p || ((uintptr_t)p & (al - 1))) { bad++; keep[i] = 0; continue; }
            memset(p, i, sz);
            keep[i] = p;
            if (i % 3 == 0) free(malloc(24 + i));    /* interleave plain blocks */
        }
        for (int i = 0; i < 64; i += 2) free(keep[i]);
        for (int i = 1; i < 64; i += 2) {
            if (!keep[i]) continue;
            size_t sz = 1 + (size_t)i * 37;
            for (size_t k = 0; k < sz; k++)
                if (((unsigned char *)keep[i])[k] != (unsigned char)i) { bad++; break; }
            free(keep[i]);
        }
    }
    void *pm = 0;
    int rc = posix_memalign(&pm, 64, 100);
    if (rc != 0 || ((uintptr_t)pm & 63)) bad++;
    free(pm);
    if (posix_memalign(&pm, 3, 8) == 0) bad++;      /* not a power of two */
    /* the heap still hands out and takes back large blocks */
    char *big = malloc(200000);
    if (!big) bad++; else { memset(big, 7, 200000); free(big); }
    printf("aligned_alloc: %s\n", bad ? "FAIL" : "ok");
    return bad;
}
