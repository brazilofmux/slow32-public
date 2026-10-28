// 6.9.1p7 / 6.7.6.3p8  a parameter declared with function type is adjusted to a pointer to function
#include <stdio.h>
static int sq(int x) { return x * x; }
static int apply(int f(int), int v) { return f(v) + (*f)(v + 1); }
int main(void) {
    printf("%d\n", apply(sq, 3));
    return 0;
}
