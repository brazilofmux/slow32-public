// 6.7.7p2  abstract declarators in casts, sizeof and compound literals
#include <stdio.h>
static int one(void) { return 1; }
int main(void) {
    printf("%u %u %u %u\n", (unsigned)sizeof(int *[3]), (unsigned)sizeof(int (*)[3]),
           (unsigned)sizeof(int (*)(void)), (unsigned)sizeof(char (*)(int, ...)));
    int (*fp)(void) = (int (*)(void))one;
    int *q = (int[]){ 7, 8, 9 };
    printf("%d %d %u\n", fp(), q[2], (unsigned)sizeof(const volatile unsigned long long));
    return 0;
}
