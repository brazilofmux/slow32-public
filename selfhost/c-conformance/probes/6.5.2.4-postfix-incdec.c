// 6.5.2.4p2-3  postfix ++/-- yields the old value; on int, pointers, long long, unsigned char wrap
#include <stdio.h>
int main(void) {
    int a[3] = {1, 2, 3}, *p = a;
    int x = *p++;
    long long ll = 0xFFFFFFFFLL; long long oll = ll++;
    unsigned char uc = 255; uc++;
    printf("%d %d %lld %lld %d\n", x, *p, oll, ll, uc);
    return 0;
}
