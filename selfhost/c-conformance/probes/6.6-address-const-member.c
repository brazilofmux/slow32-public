// 6.6p9  & of a struct member (and of an array element within one) is an address constant
#include <stdio.h>
struct S { int a; int b[4]; };
static struct S gs = {1, {2, 3, 4, 5}};
static int *p5 = &gs.a;
static int *p3 = &gs.b[2];
int main(void) { printf("%d %d\n", *p5, *p3); return 0; }
