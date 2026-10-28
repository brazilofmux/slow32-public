// 6.5.1p2-5  identifiers, constants, string literals as lvalues, parenthesized expressions
#include <stdio.h>
int main(void) {
    int a = 5;
    (a) = 7;                 /* parenthesized lvalue */
    char c = "xyz"[1];       /* string literal is an array lvalue */
    printf("%d %c %d %d\n", a, c, (int)sizeof("hello"), (int)sizeof(("ab")));
    return 0;
}
