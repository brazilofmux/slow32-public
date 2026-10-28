// 6.8.1p4  labels live in their own name space; a label may precede any statement
#include <stdio.h>
int main(void) {
    int x = 0;
    goto x;
x:  x++;
    if (x < 3) goto x;
    { int n = 0; goto end; n = 99; end: ; printf("n=%d\n", n); }
    {
    inner:
        x += 10;
        if (x < 30) goto inner;
    }
    printf("x=%d\n", x);
    return 0;
}
