// 6.7.2.2p3  enumerator in scope right after its declarator; block-scope enum shadows
#include <stdio.h>
enum { X = 1, Y = X * 5, Z = Y + X };
int main(void) {
    printf("%d %d %d\n", X, Y, Z);
    { enum { X = 100 }; printf("%d\n", X); }
    printf("%d\n", X);
    return 0;
}
