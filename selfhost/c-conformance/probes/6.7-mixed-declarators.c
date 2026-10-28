// 6.7p1  one declaration mixing object, pointer-to-function and function declarators
#include <stdio.h>
int x = 4, (*fp)(void), f(void);
int f(void) { return 12; }
int main(void) { fp = f; printf("%d %d\n", x, fp()); return 0; }
