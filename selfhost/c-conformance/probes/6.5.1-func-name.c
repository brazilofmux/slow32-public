// 6.5.1p2 (6.4.2.2)  __func__ is usable as a primary expression inside a function
#include <stdio.h>
#include <string.h>
static const char *who(void) { return __func__; }
int main(void) { printf("%s %s %d\n", __func__, who(), (int)sizeof(__func__)); return 0; }
