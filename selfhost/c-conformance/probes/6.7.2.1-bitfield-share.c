// 6.7.2.1p11  bit-fields share bytes with their neighbours; :0 and unnamed fields as clang lays them out
#include <stdio.h>
#include <stddef.h>
struct s1 { char c; int : 0; char d; };
struct s2 { char c; int : 4; char d; };
struct s3 { char c; unsigned x : 3; };
struct s4 { unsigned short a : 12; unsigned short b : 6; };
struct s5 { char a; unsigned b : 30; };
struct s6 { unsigned x : 3; char c; unsigned y : 5; };
struct s7 { char a : 3; char b : 6; };
struct s8 { short a; int : 0; };
struct s9 { char c; unsigned short : 0; char d; unsigned x : 20; };
int main(void) {
    struct s6 v;
    struct s9 w;
    printf("%u %u %u %u %u %u %u %u %u\n", (unsigned)sizeof(struct s1), (unsigned)sizeof(struct s2),
           (unsigned)sizeof(struct s3), (unsigned)sizeof(struct s4), (unsigned)sizeof(struct s5),
           (unsigned)sizeof(struct s6), (unsigned)sizeof(struct s7), (unsigned)sizeof(struct s8),
           (unsigned)sizeof(struct s9));
    printf("%u %u %u %u\n", (unsigned)offsetof(struct s1, d), (unsigned)offsetof(struct s2, d),
           (unsigned)offsetof(struct s6, c), (unsigned)offsetof(struct s9, d));
    v.c = 'q'; v.x = 5; v.y = 17; v.c = (char)(v.c + 1); v.x = v.x + 1;
    printf("%c %u %u\n", v.c, (unsigned)v.x, (unsigned)v.y);
    w.c = 1; w.d = 2; w.x = 0xABCDE; w.d = 3;
    printf("%d %d %x\n", w.c, w.d, (unsigned)w.x);
    return 0;
}
