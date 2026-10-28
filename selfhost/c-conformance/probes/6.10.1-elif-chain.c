// 6.10.1p6  only the first true group is processed; nesting inside skipped groups
#include <stdio.h>
#define V 2
int main(void) {
    int r = 0;
#if V == 1
    r = 10;
#elif V == 2
    r = 20;
#elif V == 2
    r = 30;
#else
    r = 40;
#endif
#if 0
#  if 1
     r = 99;
#  elif 1
     r = 98;
#  else
     r = 97;
#  endif
#elif 0
    r = 50;
#else
    r += 1;
#endif
#if 0
#endif
    printf("%d\n", r);
    return 0;
}
