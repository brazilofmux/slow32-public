// 6.2.4p3,p6  static locals keep value across calls; automatic re-initialized each entry
#include <stdio.h>
static int counter(void) { static int n = 10; int a = 1; a++; return n++ * a; }
static int *addr(void) { static int z; return &z; }
int main(void) {
    int a = counter(), b = counter(), c = counter();
    int total = 0;
    for (int i = 0; i < 3; i++) { int fresh = 1; fresh += i; total += fresh; }
    printf("%d %d %d %d %d\n", a, b, c, total, addr() == addr());
    return 0;
}
