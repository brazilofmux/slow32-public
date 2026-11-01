/* 7.20.2.4: INTPTR_MIN, INTPTR_MAX and UINTPTR_MAX meet their minima and describe intptr_t and uintptr_t */
#include <stdint.h>
#include <stdio.h>

int main(void) {
    printf("%d %d %d\n", INTPTR_MIN <= -32767, INTPTR_MAX >= 32767, UINTPTR_MAX >= 65535);
    printf("%d %d\n", (uintptr_t)-1 == UINTPTR_MAX, (intptr_t)INTPTR_MAX == INTPTR_MAX && (intptr_t)INTPTR_MIN == INTPTR_MIN);
    return 0;
}
