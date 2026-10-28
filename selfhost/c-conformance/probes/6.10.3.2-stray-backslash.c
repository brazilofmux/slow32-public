// 6.10.3.2p2  a \ outside any literal is copied into the string unescaped
#include <stdio.h>
#define S(x) #x
int main(void) {
    fputs(S(a\n), stdout);
    fputs(S(:\t:) "\n", stdout);
    return 0;
}
