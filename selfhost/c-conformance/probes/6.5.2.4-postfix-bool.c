// 6.5.2.4p2 (6.3.1.2)  postfix ++/-- on _Bool: result converted back to _Bool (0 or 1)
#include <stdio.h>
int main(void) {
    _Bool b = 1; b++;
    _Bool c = 0; c--;
    printf("%d %d\n", b, c);
    return 0;
}
