/* 7.19p2, 6.2.8p2: max_align_t's alignment is at least that of every scalar type */
#include <stddef.h>
#include <stdio.h>

int main(void) {
    size_t m = _Alignof(max_align_t);
    printf("%d %d %d %d %d %d %d %d\n", m >= _Alignof(char), m >= _Alignof(short), m >= _Alignof(int),
           m >= _Alignof(long), m >= _Alignof(long long), m >= _Alignof(float), m >= _Alignof(double),
           m >= _Alignof(long double));
    printf("%d %d\n", m >= _Alignof(void *), m >= _Alignof(void (*)(void)));
    return 0;
}
