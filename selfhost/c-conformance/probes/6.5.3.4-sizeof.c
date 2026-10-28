// 6.5.3.4p2,4-5  sizeof of types, expressions and arrays; operand not evaluated
#include <stdio.h>
#include <stddef.h>
struct S { char c; double d; };
int main(void) {
    int i = 0; char a[10]; int m[3][5];
    size_t z = sizeof(i++);
    printf("%d %d %d %d %d\n", (int)z, i, (int)sizeof a, (int)sizeof m[0], (int)(sizeof m / sizeof m[0]));
    printf("%d %d %d %d\n", (int)sizeof(char), (int)sizeof(struct S), (int)sizeof 'x', (int)sizeof(long long));
    return 0;
}
