// 6.4.1p1  register is a storage class allowed on parameters (6.7.6.3p2)
#include <stdio.h>
static int sq(register int x, register const char *s) { return x * x + s[0]; }
int main(void) { printf("%d\n", sq(3, "\1")); return 0; }
