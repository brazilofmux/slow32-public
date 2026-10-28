// 6.7.2.1p11  bit-field allocation units (implementation-defined; compared with clang's SLOW-32 ABI)
#include <stdio.h>
struct a { unsigned x : 3; unsigned : 0; unsigned d : 1; };
struct b { unsigned x : 3; _Bool f : 1; };
struct c { char c; unsigned x : 3; };
struct d { char c; unsigned short h : 4; char e; };
int main(void) {
    printf("%u %u %u %u\n", (unsigned)sizeof(struct a), (unsigned)sizeof(struct b),
           (unsigned)sizeof(struct c), (unsigned)sizeof(struct d));
    return 0;
}
