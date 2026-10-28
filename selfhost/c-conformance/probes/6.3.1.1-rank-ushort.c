// 6.3.1.1p2  unsigned short promotes to (signed) int, so ~us and us - 70000 are negative
#include <stdio.h>
int main(void) {
    unsigned short us = 5;
    unsigned char uc = 0;
    printf("%d %d %d\n", ~us < 0, us - 70000 < 0, ~uc);
    return 0;
}
