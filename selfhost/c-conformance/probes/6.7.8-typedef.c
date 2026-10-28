// 6.7.8p3-8  typedef names: synonyms, function-pointer typedef, C11 same-type redeclaration, shadowing
#include <stdio.h>
typedef int I;
typedef I I;
typedef int (*Op)(int, int);
typedef struct { int x, y; } Pt;
typedef Pt *PtP;
typedef char Name[8];
static int addf(int a, int b) { return a + b; }
int main(void) {
    Op o = addf; Pt p = { 1, 2 }; PtP pp = &p; Name n = "hi";
    I I2 = 3;
    { int I = 40; printf("%d\n", I + 2); }
    printf("%d %d %u %s %d\n", o(2, 3), pp->y, (unsigned)sizeof(Name), n, I2);
    unsigned U = 5; typedef unsigned T; T t = U; printf("%u\n", t);
    return 0;
}
