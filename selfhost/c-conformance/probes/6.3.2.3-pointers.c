// 6.3.2.3p1-p7  void* round trip; null pointer constants; qualification; char* to object bytes
#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
int main(void) {
    int x = 42;
    void *vp = &x;
    int *ip = vp;
    const int *cip = ip;
    int *np = 0;
    char *cp = (void *)0;
    void (*fnp)(void) = 0;
    unsigned char *bp = (unsigned char *)&x;
    uintptr_t u = (uintptr_t)ip;
    int *ip2 = (int *)u;
    int eq = (np == NULL) + (cp == 0) + (fnp == 0) + (vp == ip) + (cip == ip) + (ip2 == &x);
    printf("%d %d %d\n", *ip, eq, bp[0]);
    return 0;
}
