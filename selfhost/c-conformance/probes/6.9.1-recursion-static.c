// 6.9.1p2-4  static and extern function definitions, mutual recursion via a prior declaration
#include <stdio.h>
static int odd(unsigned);
extern int even(unsigned n) { return n == 0 ? 1 : odd(n - 1); }
static int odd(unsigned n) { return n == 0 ? 0 : even(n - 1); }
int main(void) {
    printf("%d %d %d\n", even(10), odd(7), even(33));
    return 0;
}
