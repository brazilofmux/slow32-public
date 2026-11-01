/* 7.10, 5.2.4.2.1p1: MB_LEN_MAX is defined, usable in #if, and at least 1 */
#include <limits.h>
#include <stdio.h>

int main(void) {
#if MB_LEN_MAX >= 1
    puts("ok");
#endif
    return 0;
}
