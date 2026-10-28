// 7.15/6.7.5  <stdalign.h> alignas and alignof macros
#include <stdio.h>
#include <stdalign.h>
#include <stdint.h>
alignas(8) static char b[4];
int main(void) {
    printf("%u %d %d\n", (unsigned)alignof(int), __alignas_is_defined, (int)((uintptr_t)b % 8));
    return 0;
}
