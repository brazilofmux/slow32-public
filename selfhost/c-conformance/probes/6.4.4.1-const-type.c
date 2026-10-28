// 6.4.4.1p5  type of an unsuffixed constant: decimal -> int, long, long long; hex/octal may be unsigned
#include <stdio.h>
#define T(x) _Generic((x), int: "int", unsigned: "uint", long: "long", unsigned long: "ulong", \
                         long long: "ll", unsigned long long: "ull", default: "?")
int main(void) {
    printf("%s %s %s %s %s %s %s %s\n", T(2147483647), T(2147483648), T(0x7fffffff), T(0x80000000),
           T(4294967296), T(0xffffffffffffffff), T(2147483648u), T(037777777777));
    return 0;
}
