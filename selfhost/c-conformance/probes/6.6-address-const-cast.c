// 6.6p9  casts of address constants: (char *)&obj + n in a static initializer
#include <stdio.h>
struct S { int a; int b; };
static struct S gs = {1, 2};
static char *cp = (char *)&gs + sizeof(int);
int main(void) { printf("%d\n", *(int *)cp); return 0; }
