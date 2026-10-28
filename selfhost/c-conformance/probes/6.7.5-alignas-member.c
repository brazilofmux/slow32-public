// 6.7.5p2  _Alignas on a struct member changes offset and struct size
#include <stdio.h>
#include <stddef.h>
struct s { char c; _Alignas(16) int i; };
int main(void) {
    printf("%u %u\n", (unsigned)offsetof(struct s, i), (unsigned)sizeof(struct s));
    return 0;
}
