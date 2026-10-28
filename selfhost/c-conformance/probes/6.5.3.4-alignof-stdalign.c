// 6.5.3.4p3 (7.15)  alignof from <stdalign.h> is _Alignof
#include <stdio.h>
#include <stdalign.h>
int main(void) { printf("%d %d\n", (int)alignof(short), (int)alignof(long long)); return 0; }
