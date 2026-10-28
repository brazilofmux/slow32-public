// 6.4.1p1  _Noreturn function specifier (6.7.4) accepted
#include <stdio.h>
#include <stdlib.h>
_Noreturn static void die(int c) { printf("bye %d\n", c); exit(0); }
int main(void) { die(3); }
