// 6.8.5.3p1  a for clause-1 declaration whose type is a struct defined in place
#include <stdio.h>
int main(void) {
    int sum = 0;
    for (struct { int x, y; } s = { 1, 2 }; s.x < 5; s.x += s.y) sum += s.x;
    printf("%d\n", sum);
    return 0;
}
