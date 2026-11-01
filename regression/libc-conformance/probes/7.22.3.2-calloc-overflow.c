/* 7.22.3.2 calloc: when nmemb times size cannot be represented, no object fits and a null pointer comes back */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

static void *(*volatile cal)(size_t, size_t) = calloc;   /* keeps the compiler from eliding the calls */
int main(void) {
    size_t big = (size_t)-1;
    void *p = cal(big / 16 + 1, 32);
    void *q = cal(big, big);
    void *r = cal((big >> 1) + 2, 2);   /* the product wraps to 2 */
    printf("%d %d %d\n", p == NULL, q == NULL, r == NULL);
    free(cal(0, 8));
    printf("calloc(0, 8) returned\n");
    return 0;
}
