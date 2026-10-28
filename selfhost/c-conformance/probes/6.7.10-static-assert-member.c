// 6.7.2.1p1/6.7.10  a static assertion as a struct-declaration (inside a struct)
#include <stdio.h>
struct s { int a; _Static_assert(1, "in struct"); int b; };
int main(void) { printf("%u\n", (unsigned)sizeof(struct s)); return 0; }
