/* 7.19p3: offsetof's member designator may name a nested member (a.b) or an array element (a[i]) */
#include <stddef.h>
#include <stdio.h>

struct in { char a; double b; };
struct out { char c; struct in i; int arr[5]; };

int main(void) {
    struct out o;
    char *base = (char *)&o;
    printf("%d\n", (char *)&o.i.b - base == (ptrdiff_t)offsetof(struct out, i.b));
    printf("%d\n", (char *)&o.arr[3] - base == (ptrdiff_t)offsetof(struct out, arr[3]));
    return 0;
}
