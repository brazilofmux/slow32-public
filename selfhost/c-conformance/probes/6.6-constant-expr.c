// 6.6p6-9  integer constant expressions in array sizes, case labels, enum, bit-field widths, static init
#include <stdio.h>
#include <stddef.h>
enum { A = 3 * 4 - 2, B = A << 2, C = (A > 5) ? 100 : 200, D = sizeof(int) * 2, E = (int)3.9 };
struct S { int x; char c[3]; int y; };
static int arr[A + (int)sizeof(double)];
struct BF { unsigned f : D + 1; };
static int sw(int v) { switch (v) { case A: return 1; case B + 1: return 2; case 'a' - 'A': return 3; default: return 0; } }
static const int k = 7;   /* not an ICE, but an arithmetic constant initializer */
static int off = offsetof(struct S, y);
int main(void) {
    printf("%d %d %d %d %d %d\n", A, B, C, D, E, (int)(sizeof arr / sizeof arr[0]));
    printf("%d %d %d %d\n", sw(10), sw(41), sw(32), k);
    printf("%d\n", off);
    _Static_assert(A == 10 && -1 < 0 && ~0 == -1, "ice");
    return 0;
}
