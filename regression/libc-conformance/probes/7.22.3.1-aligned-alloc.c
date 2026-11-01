/* 7.22.3.1 aligned_alloc: the object is aligned as requested for supported alignments */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdint.h>

int main(void) {
    size_t al[] = {8, 16, 32, 64, 128, 256, 1024, 4096};
    size_t i;
    for (i = 0; i < sizeof al / sizeof al[0]; i++) {
        char *p = aligned_alloc(al[i], al[i] * 3);
        int ok = p != NULL && (uintptr_t)p % al[i] == 0;
        if (p) memset(p, 1, al[i] * 3);
        printf("%4u: %d\n", (unsigned)al[i], ok);
        free(p);
    }
    return 0;
}
