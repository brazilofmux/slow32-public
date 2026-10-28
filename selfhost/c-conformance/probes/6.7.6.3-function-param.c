// 6.7.6.3p8  a parameter declared with function type is adjusted to pointer to function
#include <stdio.h>
static int twice(int x) { return 2 * x; }
static int apply(int f(int), int x) { return f(x); }
int main(void) { printf("%d\n", apply(twice, 21)); return 0; }
