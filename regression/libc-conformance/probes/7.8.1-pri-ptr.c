/* 7.8.1p2-p3: the PRI macros for intptr_t and uintptr_t print values of those types */
#include <inttypes.h>
#include <stdio.h>

int main(void) {
    intptr_t a = -5;
    uintptr_t b = 4000000000u % ((uintptr_t)-1);
    intptr_t c = INT32_MAX;
    printf("%" PRIdPTR " %" PRIiPTR " %" PRIdPTR " end\n", a, a, c);
    printf("%" PRIuPTR " %" PRIxPTR " %" PRIXPTR " %" PRIoPTR " end\n", b, (uintptr_t)0xbeef, (uintptr_t)0xbeef, (uintptr_t)8);
    return 0;
}
