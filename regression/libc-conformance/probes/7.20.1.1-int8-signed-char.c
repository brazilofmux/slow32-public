/* 7.20.1.1p1, 6.2.5p4: int8_t is a signed integer type, so with 8-bit char it is signed char, never plain char */
#include <stdint.h>
#include <stdio.h>

int main(void) {
    int8_t x = 0;
    printf("%s\n", _Generic(x, signed char: "signed char", char: "char", default: "other"));
    return 0;
}
