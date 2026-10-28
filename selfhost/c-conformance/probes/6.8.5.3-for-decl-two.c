// 6.8.5.3p1  a for clause-1 declaration with two declarators
#include <stdio.h>
int main(void) {
    int sum = 0;
    for (int i = 0, j = 10; i < j; i++, j--) sum += i * j;
    printf("%d\n", sum);
    return 0;
}
