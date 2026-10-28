// 6.3.1.1p2  unary + - ~ promote their operand: sizeof(+c) is sizeof(int)
#include <stdio.h>
int main(void) {
    char c = 1; short s = 2; unsigned char uc = 3;
    printf("%d %d %d %d %d\n", (int)sizeof(+c), (int)sizeof(-s), (int)sizeof(~uc), (int)sizeof(c + c),
           (int)sizeof(c << 1));
    return 0;
}
