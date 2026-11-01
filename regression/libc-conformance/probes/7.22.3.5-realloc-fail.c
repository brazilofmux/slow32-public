/* 7.22.3.5 realloc: when the new size cannot be had, a null pointer comes back and the old object is unchanged */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

static void *(*volatile rea)(void *, size_t) = realloc;
int main(void) {
    char *p = malloc(32), *q;
    if (!p) return 2;
    strcpy(p, "still here");
    q = rea(p, (size_t)-1 - 16);
    printf("null: %d\n", q == NULL);
    if (q == NULL) printf("old: %s\n", p), free(p);
    return 0;
}
