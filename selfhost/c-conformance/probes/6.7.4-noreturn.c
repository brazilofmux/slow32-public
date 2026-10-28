// 6.7.4p8  _Noreturn function specifier
#include <stdio.h>
#include <stdlib.h>
_Noreturn static void die(int code) { printf("dying %d\n", code); exit(code == 3 ? 0 : 1); }
int main(void) {
    printf("start\n");
    die(3);
}
