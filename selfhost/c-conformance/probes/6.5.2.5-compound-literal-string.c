// 6.5.2.5p3, 6.7.9p14  a compound literal of character array type may be initialized by a string literal
#include <stdio.h>
int main(void){ char *p = (char[]){"abc"}; char *q = (char[]){ "xy" }; char *r = (char[6]){"hi"}; printf("%s %s %s %d %d\n", p, q, r, (int)sizeof((char[]){"abc"}), r[5]); return 0; }
