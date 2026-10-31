// 6.7.9p17,p20  after a designator, initialization continues with the next subobject; an elided aggregate ends at a designator
#include <stdio.h>
int a[3][3] = { 1, 2, [1][1] = 7, 8, 9, 10 };
int b[2][2][2] = { [0][1][1] = 5, { 6, 7 } };
int c[2][2][2] = { [0][0] = { 1, 2 }, { 3, 4 }, 5 };
struct S { int arr[2]; int b; int c; };
struct S s = { 1, .b = 3, 4 };
int main(void) {
    int la[3][3] = { 1, 2, [1][1] = 7, 8, 9, 10 };
    int lc[2][2][2] = { [0][0] = { 1, 2 }, { 3, 4 }, 5 };
    printf("%d %d %d %d %d %d\n", a[0][1], a[1][1], a[1][2], a[2][0], a[2][1], a[2][2]);
    printf("%d %d %d %d %d %d\n", b[0][1][1], b[1][0][0], b[1][0][1], c[0][1][0], c[0][1][1], c[1][0][0]);
    printf("%d %d %d %d\n", s.arr[0], s.arr[1], s.b, s.c);
    printf("%d %d %d %d %d\n", la[1][2], la[2][0], lc[0][1][1], lc[1][0][0], lc[1][1][1]);
    return 0;
}
