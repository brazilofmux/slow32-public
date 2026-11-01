/* 7.1.4p1: #undef of any library function name leaves the real function, which still works */
#include <ctype.h>
#include <stdlib.h>
#include <setjmp.h>
#include <stdio.h>
#undef isalpha
#undef toupper
#undef abs
#undef longjmp
#undef putchar

int main(void) {
    printf("%d %c %d\n", isalpha('k') != 0, toupper('k'), abs(-9));
    putchar('!');
    putchar('\n');
    return 0;
}
