// 6.10.3p8  an identifier right after a directive-starting # is never macro-replaced
#include <stdio.h>
#define undef define
#define ifdef ifndef
#define FOO 1
#undef FOO
#ifdef FOO
int r = 1;
#else
int r = 2;
#endif
int main(void) { printf("%d\n", r); return 0; }
