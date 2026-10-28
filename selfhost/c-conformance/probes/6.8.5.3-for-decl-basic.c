// 6.8.5.3p1  a for clause-1 declaration is scoped to the loop; the body may shadow it
#include <stdio.h>
int i = 1000;
int main(void) {
    int sum = 0;
    for (int i = 0; i < 5; i++) sum += i;
    for (int i = 0; i < 3; i++) { int i = 50; sum += i; }
    printf("%d %d\n", sum, i);
    return 0;
}
