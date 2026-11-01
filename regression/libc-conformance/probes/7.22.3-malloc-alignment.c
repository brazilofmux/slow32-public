/* 7.22.3p1 malloc, calloc, realloc: each returned pointer is suitably aligned for max_align_t */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stddef.h>
#include <stdint.h>

int main(void) {
    size_t sizes[] = {1, 2, 3, 5, 7, 8, 9, 15, 16, 17, 24, 31, 33, 100, 1000, 4097, 65536};
    size_t al = _Alignof(max_align_t), i;
    int ok = 1;
    void *keep[17];
    for (i = 0; i < sizeof sizes / sizeof sizes[0]; i++) {
        void *p = malloc(sizes[i]), *q = calloc(sizes[i], 1);
        if (!p || !q || (uintptr_t)p % al || (uintptr_t)q % al) ok = 0;
        p = realloc(p, sizes[i] * 3);
        if (!p || (uintptr_t)p % al) ok = 0;
        keep[i] = p;
        free(q);
    }
    for (i = 0; i < 17; i++) free(keep[i]);
    printf("aligned: %d\nalign >= double: %d\n", ok, al >= _Alignof(double) && al >= _Alignof(long long));
    return 0;
}
