// 6.8.5.3p1  a for clause-1 declaration of an initialized array and a pointer into it
#include <stdio.h>
int main(void) {
    int sum = 0;
    for (int a[3] = { 1, 2, 3 }, *p = a; p < a + 3; p++) sum += *p;
    printf("%d\n", sum);
    return 0;
}
