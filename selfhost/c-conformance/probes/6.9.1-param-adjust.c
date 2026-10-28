// 6.9.1p7-10  array params adjust to pointers and are modifiable; struct params are copies
#include <stdio.h>
static int walk(int a[], int n, int m[][3]) {
    int s = 0;
    a++;
    n--;
    while (n-- > 0) s += *a++;
    s += m[1][2];
    return s;
}
struct big { int v[8]; };
static int touch(struct big b) { b.v[0] = 999; return b.v[0] + b.v[1]; }
int main(void) {
    int arr[4] = { 1, 2, 3, 4 };
    int m[2][3] = { { 0 }, { 0, 0, 50 } };
    struct big b = { { 1, 2 } };
    int r = walk(arr, 4, m);
    int t = touch(b);
    printf("%d %d %d %d\n", r, arr[0], t, b.v[0]);
    return 0;
}
