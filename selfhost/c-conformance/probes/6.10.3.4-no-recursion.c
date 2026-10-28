// 6.10.3.4p2  a macro's own name inside its expansion is not replaced again
#include <stdio.h>
int foo = 5;
#define foo (foo + 1)
int ping = 100, pong = 200;
#define ping pong
#define pong ping
#define f(x) (x + f)
int (f) = 1000;
int main(void) {
    printf("%d %d %d %d\n", foo, ping, pong, f(2));
    return 0;
}
