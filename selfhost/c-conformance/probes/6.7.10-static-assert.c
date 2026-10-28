// 6.7.10p2-3  _Static_assert at file and block scope
#include <stdio.h>
_Static_assert(sizeof(int) == 4, "int is 4 bytes");
struct s { int a; int b; };
int main(void) {
    _Static_assert(sizeof(struct s) == 8, "two ints");
    for (int i = 0; i < 1; i++) { _Static_assert(1 + 1 == 2, "math"); }
    printf("ok %u\n", (unsigned)sizeof(struct s));
    return 0;
}
