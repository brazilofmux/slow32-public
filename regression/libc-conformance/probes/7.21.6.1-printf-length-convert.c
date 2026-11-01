/* 7.21.6.1 fprintf: hh and h convert the promoted argument to the narrower type before printing */
#include <stdio.h>

int main(void) {
    printf("%hhd %hhu %hhx %hd %hu %hx\n", 300, 300, 0x1ff, 70000, 70000, 0x12345);
    printf("%hhd %hhu %hd %hu\n", 200, -1, 40000, -1);
    printf("%hhi %hi %hho %ho\n", 0x180, 0x18000, 0x1ff, 0x1ffff);
    return 0;
}
