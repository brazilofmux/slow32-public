/* 7.19p2: ptrdiff_t is the type of a pointer difference and size_t the type of sizeof */
#include <stddef.h>
#include <stdio.h>

int main(void) {
    int a[4];
    int *p = &a[3], *q = &a[0];
    printf("%d %d %d\n", _Generic(p - q, ptrdiff_t: 1, default: 0), _Generic(sizeof a, size_t: 1, default: 0),
           _Generic(_Alignof(int), size_t: 1, default: 0));
    printf("%d %d\n", (int)(p - q), (size_t)-1 > 0);
    return 0;
}
