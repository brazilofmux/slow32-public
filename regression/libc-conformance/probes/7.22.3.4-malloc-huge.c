/* 7.22.3.4 malloc: a request no object can satisfy returns a null pointer */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

static void *(*volatile mal)(size_t) = malloc;   /* keeps the compiler from eliding the calls */
int main(void) {
    size_t big = (size_t)-1;
    void *p = mal(big), *q = mal(big - 8), *r = mal(big / 2 + 1);
    printf("%d %d %d\n", p == NULL, q == NULL, r == NULL);
    return 0;
}
