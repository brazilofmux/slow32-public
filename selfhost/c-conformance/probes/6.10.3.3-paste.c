// 6.10.3.3p2,p3  ## concatenates identifiers and numbers; the result is rescanned
#include <stdio.h>
#define CAT(a, b) a ## b
#define CAT3(a, b, c) a ## b ## c
#define PREFIX(n) var_ ## n
#define fortytwo 42
int main(void) {
    int var_1 = 11, var_xy = 22;
    int n = CAT(12, 34);
    int h = CAT(0x, 1F);
    printf("%d %d %d %d\n", PREFIX(1), PREFIX(xy), n, h);
    printf("%d %d %ld\n", CAT(forty, two), CAT3(4, 5, 6), CAT(7, L));
    return 0;
}
