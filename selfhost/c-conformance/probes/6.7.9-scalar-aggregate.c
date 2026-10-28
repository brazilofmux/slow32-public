// 6.7.9p10-21  zero-filled statics, brace elision, partial init, string init, scalar in braces
#include <stdio.h>
static int zs[4]; static double zd; static int *zp;
struct in { int a, b; }; struct out { struct in i[2]; char c; };
int main(void) {
    struct out o = { 1, 2, 3, 4, 'x' };
    int m[2][3] = { 1, 2, 3, 4 };
    int part[5] = { 9 };
    char s1[4] = "abc", s2[3] = "abc";
    int sc = { 17 };
    printf("%d %d %d %d\n", zs[3], (int)zd, zp == 0, o.i[1].b + o.c);
    printf("%d %d %d %d %d\n", m[1][0], m[1][2], part[0], part[4], sc);
    printf("%s %c%c%c\n", s1, s2[0], s2[1], s2[2]);
    return 0;
}
