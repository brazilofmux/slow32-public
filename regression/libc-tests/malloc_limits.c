/* malloc, calloc and realloc asked for more than a 256 MB address space
 * can hold: each must say NULL, leave the block realloc was given alone,
 * and leave the heap working.  Sizes just under the top of size_t and of
 * int are the ones whose rounding-up used to wrap to a small block.  The
 * sizes go through a volatile so the compiler cannot reason about them,
 * and every pointer is printed as null or not, never used blind. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static volatile size_t sizes[] = {
    (size_t)-1, (size_t)-4, (size_t)-60, 0xFFFFFF00u,
    0x7FFFFFFFu, 0x7FFFFFFCu, 0x7FFFFFF9u, 0x40000000u, 0x10000000u,
};
#define NSIZES (sizeof sizes / sizeof sizes[0])

static void * volatile sink;

int main(void)
{
    unsigned i;
    char *keep, *grown, *after;

    for (i = 0; i < NSIZES; i++) {
        sink = malloc(sizes[i]);
        printf("malloc(%#lx) %s\n", (unsigned long)sizes[i], sink ? "BLOCK" : "null");
        if (sink) free(sink);
    }

    sink = calloc(sizes[7], 4);
    printf("calloc(%#lx, 4) %s\n", (unsigned long)sizes[7], sink ? "BLOCK" : "null");
    sink = calloc(1, sizes[2]);
    printf("calloc(1, %#lx) %s\n", (unsigned long)sizes[2], sink ? "BLOCK" : "null");

    keep = malloc(40);
    if (!keep) { printf("malloc(40) null\n"); return 1; }
    strcpy(keep, "pumice and basalt");
    for (i = 0; i < NSIZES; i++) {
        grown = realloc(keep, sizes[i]);
        printf("realloc(keep, %#lx) %s\n", (unsigned long)sizes[i], grown ? "BLOCK" : "null");
        if (grown) keep = grown;
    }
    printf("keep still \"%s\"\n", keep);
    free(keep);

    after = malloc(3000);
    if (after) {
        memset(after, 'q', 2999);
        after[2999] = 0;
        printf("heap afterwards: %u bytes, last '%c'\n", (unsigned)strlen(after), after[2998]);
        free(after);
    } else {
        printf("heap afterwards: null\n");
    }
    printf("malloc_limits done\n");
    return 0;
}
