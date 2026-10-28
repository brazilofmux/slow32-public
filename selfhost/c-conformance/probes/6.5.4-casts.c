// 6.5.4p5  explicit conversions: truncation, sign change, float<->int, pointer<->integer, void cast
#include <stdio.h>
#include <stdint.h>
int main(void) {
    int big = 0x12345;
    double d = -3.75;
    int x = 0; int *p = &x;
    uintptr_t up = (uintptr_t)p;
    printf("%d %d %d %u\n", (signed char)0x1ff, (unsigned char)-1, (short)big, (unsigned)(signed char)-2);
    printf("%d %.1f %lld %d\n", (int)d, (double)7 / 2, (long long)1e10, (int *)up == p);
    (void)x;
    float f = (float)0.1; printf("%d\n", (double)f != 0.1);
    return 0;
}
