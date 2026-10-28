// 6.5.1.1p2  _Generic over result types: ptrdiff_t, size_t, ?: composite pointer types
#include <stdio.h>
#include <stddef.h>
int main(void) {
    int a[2]; int *p = a; const int *cp = a; void *vp = a; volatile int t = 1;
    printf("%d %d %d %d\n", _Generic(p - p, ptrdiff_t: 1, default: 0), _Generic(sizeof(int), size_t: 1, default: 0),
           _Generic(t ? cp : p, const int *: 1, default: 0), _Generic(t ? vp : p, void *: 1, default: 0));
    return 0;
}
