/* 7.18p4: a program may undefine and redefine bool, true and false */
#include <stdbool.h>
#include <stdio.h>
#undef bool
#undef true
#undef false
typedef int bool;
enum { false = 10, true = 20 };

int main(void) {
    bool x = 5;
    printf("%d %d %d\n", x, false, true);
    return 0;
}
