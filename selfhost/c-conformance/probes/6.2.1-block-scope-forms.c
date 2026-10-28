// 6.2.1p4/6.7.9p14  block-scope tags and enumerators, braced string initializers, pointer-to-array type names
#include <stdio.h>
#include <string.h>
struct node { int v; };
enum { K = 2 };
char g1[] = { "glob" };
char g2[8] = { "ab", };
char gm[2][4] = { "xy", "z" };
const char *gp[] = { "p", "q" };
int main(void) {
    static char s1[] = { "stat" };
    char l1[6] = { "loc" };
    char lm[2][3] = { "a", "bc" };
    printf("%u %s %s %s %s %s %s\n", (unsigned)sizeof g1, g1, g2, gm[1], gp[1], s1, l1);
    printf("%s %s %u %u\n", lm[0], lm[1], (unsigned)sizeof(int (*)[3]),
           (unsigned)sizeof(char (*)[2][5]));
    {
        struct inner *p;
        struct inner { double d; int w; } n2;
        struct node { char c; } n3;
        enum { K = 9 };
        p = &n2; p->w = K;
        printf("%u %d %u", (unsigned)sizeof n2, p->w, (unsigned)sizeof n3);
        { enum { K = 11 }; printf(" %d", K); }
        printf(" %d\n", K);
    }
    { struct node o = { 5 }; printf("%u %d %d\n", (unsigned)sizeof o, o.v, K); }
    return 0;
}
