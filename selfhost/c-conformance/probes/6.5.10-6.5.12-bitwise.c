// 6.5.10p4 6.5.11p4 6.5.12p4  & ^ | after usual arithmetic conversions, including long long
#include <stdio.h>
int main(void) {
    volatile int a = 0x0F0F, b = 0x00FF;
    volatile long long la = 0x0F0F0F0F00000000LL, lb = 0x00FF00FF000000FFLL;
    printf("%x %x %x\n", a & b, a ^ b, a | b);
    printf("%llx %llx %llx\n", la & lb, la ^ lb, la | lb);
    printf("%x\n", (unsigned)(-1 & 0xFFFFu));
    return 0;
}
