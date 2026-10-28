// 6.5.3.2p3-4  &*E is E, &E[i] is E+i, * on function designator, address of struct member
#include <stdio.h>
static int f(int x) { return x * 2; }
struct S { int a; int b[3]; };
int main(void) {
    int arr[4] = {1, 2, 3, 4};
    int *p = arr;
    struct S s = {1, {2, 3, 4}};
    int (*fp)(int) = f;
    printf("%d %d %d\n", &*p == p, &arr[2] == arr + 2, (**fp)(4));
    printf("%d %d\n", *&s.b[1], &s.b[2] - &s.b[0]);
    int *np = 0; printf("%d\n", &*np == np); /* &*null is not an evaluation of * */
    return 0;
}
