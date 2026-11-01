/* 7.8.1p2-p3: PRIdPTR, PRIuPTR and PRIxPTR print intptr_t and uintptr_t values, and the arguments after them */
#include <inttypes.h>
#include <stdio.h>

int main(void) {
    intptr_t a = -5;
    uintptr_t b = 0xbeef;
    printf("%" PRIdPTR " %d\n", a, 11);
    printf("%" PRIuPTR " %d\n", b, 22);
    printf("%" PRIxPTR " %s\n", b, "end");
    return 0;
}
