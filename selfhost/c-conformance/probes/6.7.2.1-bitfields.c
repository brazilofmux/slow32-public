// 6.7.2.1p10-12  bit-fields: widths, signedness, wrap, unnamed and zero-width (assigned, not initialized)
#include <stdio.h>
#include <string.h>
struct b { unsigned a : 3; signed s : 4; unsigned : 5; unsigned c : 7; unsigned : 0; unsigned d : 1; _Bool f : 1; };
int main(void) {
    struct b x; memset(&x, 0, sizeof x);
    unsigned nine = 9;
    x.a = nine;     /* wraps to 1 */
    x.s = -5;
    x.c = 127; x.d = 1; x.f = 1;
    x.c++;          /* wraps to 0 */
    printf("%u %d %u %u %d\n", x.a, x.s, x.c, x.d, x.f);
    return 0;
}
