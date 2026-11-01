/* 7.22.3p1 malloc(0): either a null pointer or a pointer distinct from every other live allocation */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

int main(void) {
    void *a = malloc(0), *b = malloc(0), *c = malloc(1);
    int ok = (a == NULL || (a != b && a != c)) && (b == NULL || b != c);
    void *d = realloc(NULL, 0);
    printf("null-or-unique: %d\n", ok);
    free(a); free(b); free(c); free(d);
    free(NULL);
    printf("free(NULL) returned\n");
    return 0;
}
