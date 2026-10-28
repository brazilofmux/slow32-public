// 6.7.9p13  automatic aggregates may use non-constant initializers; eval of full struct copy
#include <stdio.h>
struct s { int a, b; };
static int f(int x) { return x * 3; }
int main(void) {
    int n = 4;
    int arr[3] = { n, f(n), n + f(1) };
    struct s v = { f(2), arr[2] }, w = v;
    printf("%d %d %d %d %d\n", arr[0], arr[1], arr[2], w.a, w.b);
    return 0;
}
