// 6.5.3.3p2-4  unary + - ~ apply the integer promotions: the result of +uc has type int
#include <stdio.h>
int main(void) {
    unsigned char uc = 1; short s = 2;
    printf("%d %d %d\n", (int)sizeof(+uc), (int)sizeof(-uc), (int)sizeof(~s));
    return 0;
}
