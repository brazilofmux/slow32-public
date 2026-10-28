// 6.3.1.1p2  a bit-field promotes by its width: below int's, to int; as wide, unsigned stays unsigned
#include <stdio.h>
struct w {
    unsigned n5 : 5; unsigned n32 : 32; int s7 : 7;
    unsigned short h9 : 9; _Bool b : 1;
};
int main(void) {
    struct w v;
    int one = 1;
    v.n5 = 3; v.n32 = 3; v.s7 = -2; v.h9 = 5; v.b = 1;
    printf("%d %d %d\n", v.n5 - 4 < 0, v.n32 - 4 < 0, v.s7 < 0);
    printf("%d %d %d\n", -v.n5 < 0, ~v.n5 < 0, v.b - 2 < 0);
    printf("%d %d\n", (one ? v.n5 : -1) < 0, (one ? -1 : v.n5) < 0);
    printf("%d %d\n", v.h9 - 6 < 0, -v.n32 < 0);
    printf("%u %u\n", (unsigned)sizeof(+v.n5), (unsigned)sizeof(+v.n32));
    printf("%d %d\n", +v.n5 - 4 < 0, v.n5 * -1 < 0);
    return 0;
}
