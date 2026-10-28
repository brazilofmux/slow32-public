// 6.5.16p3  an assignment's value is the left operand's value after the store, with its type; right-assoc
#include <stdio.h>
int main(void) {
    int a, b, c;
    unsigned char uc;
    a = b = c = 7;
    int r = (uc = 300);
    double d; int i = (int)(d = 2.5) ; 
    printf("%d %d %d %d %d %d\n", a, b, c, r, i, (int)sizeof(uc = 1000));
    return 0;
}
