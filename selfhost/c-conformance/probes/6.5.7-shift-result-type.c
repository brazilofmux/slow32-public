// 6.5.7p3  a shift's type is that of the promoted LEFT operand; the right operand does not convert it
#include <stdio.h>
int main(void) {
    long long ll = 1; unsigned char uc = 1;
    printf("%d %d %d\n", (int)sizeof(1 << ll), (int)sizeof(1 >> ll), (int)sizeof(uc << ll));
    return 0;
}
