// 6.7.3p5-9  const/volatile objects, pointers to const, qualified struct members, repeated qualifiers
#include <stdio.h>
struct s { const int k; volatile int v; };
static const int table[3] = { 1, 2, 3 };
int main(void) {
    const const int c = 4;          /* duplicate qualifier allowed, 6.7.3p5 */
    volatile int v = 0;
    for (int i = 0; i < 5; i++) v += i;
    const int *pc = &c; int x = 9; int *const cp = &x; *cp = 10;
    const volatile int cv = 7;
    struct s a = { 11, 12 }; a.v++;
    printf("%d %d %d %d %d %d %d\n", c, v, *pc, x, cv, a.k, a.v);
    printf("%d\n", table[0] + table[2]);
    return 0;
}
