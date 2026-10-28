// 6.5.3.1p2-3  prefix ++E is E+=1; value is the new value; pointers step by element
#include <stdio.h>
int main(void) {
    int i = 5; int j = ++i; int k = --i;
    short a[4] = {1, 2, 3, 4}; short *p = a;
    short v = *++p;
    float f = 0.25f; ++f;
    printf("%d %d %d %d %.2f %d\n", i, j, k, v, f, (int)(++p - a));
    return 0;
}
