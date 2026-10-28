// 6.6p9  &function is an address constant
#include <stdio.h>
static int f(void) { return 42; }
static int (*fp)(void) = &f;
int main(void) { printf("%d\n", fp()); return 0; }
