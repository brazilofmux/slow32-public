// 6.6p6,8 (6.3.1.2)  a cast to _Bool inside a constant expression yields 0 or 1
#include <stdio.h>
static int b = (_Bool)2 + 1;
enum { E = (_Bool)256 };
int main(void) { printf("%d %d\n", b, E); return 0; }
