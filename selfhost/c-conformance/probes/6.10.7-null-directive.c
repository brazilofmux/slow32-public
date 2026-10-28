// 6.10.7p1  a # alone on a line has no effect
#include <stdio.h>
#
   #   
#if 1
#
#endif
int main(void) {
#
    puts("ok");
    return 0;
}
