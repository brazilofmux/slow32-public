// 6.4.5p3,p6  u8 prefix: a UTF-8 char array, same as a plain literal here
#include <stdio.h>
int main(void) {
    const char *s = u8"hi";
    printf("%s %d\n", s, (int)sizeof u8"hi");
    return 0;
}
