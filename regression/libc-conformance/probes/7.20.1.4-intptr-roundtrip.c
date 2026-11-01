/* 7.20.1.4: a pointer to void converted to intptr_t or uintptr_t and back compares equal to the original */
#include <stdint.h>
#include <stdio.h>

static int g;

int main(void) {
    int local;
    void *ps[3] = { &g, &local, (void *)0 };
    int i, ok = 1;
    for (i = 0; i < 3; i++) {
        intptr_t s = (intptr_t)ps[i];
        uintptr_t u = (uintptr_t)ps[i];
        ok &= (void *)s == ps[i] && (void *)u == ps[i];
    }
    printf("%d %d\n", ok, sizeof(intptr_t) == sizeof(uintptr_t));
    return 0;
}
