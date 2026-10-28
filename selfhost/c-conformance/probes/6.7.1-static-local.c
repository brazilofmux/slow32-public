// 6.7.1p3  static local keeps its value across calls; extern refers to file-scope object
#include <stdio.h>
static int g = 5;
static int counter(void) { static int n = 10; return n++; }
int main(void) {
    int a = counter(), b = counter(), c = counter();
    { extern int g; g += 1; }
    register int r = a + b + c;
    printf("%d %d %d %d %d\n", a, b, c, g, r);
    return 0;
}
