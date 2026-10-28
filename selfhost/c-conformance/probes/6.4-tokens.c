// 6.4p4  longest-match tokenization: x+++++y is invalid; a+++b means a++ + b; a- -b
#include <stdio.h>
int main(void) {
    int a = 5, b = 2;
    int c = a+++b;          /* (a++) + b */
    int d = a - -b;
    int e = a---b;          /* (a--) - b */
    printf("%d %d %d %d\n", c, d, e, a);
    return 0;
}
