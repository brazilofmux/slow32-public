// 6.3.1.1p2  (with 6.7.9) a struct with bit-field members brace-initialized
#include <stdio.h>
struct bf { unsigned u3 : 3; int s3 : 3; unsigned rest : 10; };
static struct bf g = { 5, -2, 1000 };
int main(void) {
    struct bf l = { 7, -1, 3 };
    printf("%d %d %d %d %d %d\n", g.u3, g.s3, g.rest, l.u3, l.s3, l.rest);
    return 0;
}
