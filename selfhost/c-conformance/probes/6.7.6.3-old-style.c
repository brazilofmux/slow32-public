// 6.9.1p13/6.7.6.3p14  old-style (identifier-list) function definition
#include <stdio.h>
int add(a, b) int a; int b; { return a + b; }
int main(void) { printf("%d\n", add(3, 4)); return 0; }
