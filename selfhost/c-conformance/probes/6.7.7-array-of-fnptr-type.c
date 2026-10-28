// 6.7.7p2  type name for an array of pointers to functions
#include <stdio.h>
int main(void) {
    printf("%u %u\n", (unsigned)sizeof(int (*[2])(void)), (unsigned)sizeof(char (*[3])(int, ...)));
    return 0;
}
