/* 7.21.6.2 fscanf: a field width caps the characters consumed; %c reads exactly width characters with no NUL */
#include <stdio.h>
#include <string.h>

int main(void) {
    int a = 0, b = 0;
    char s[16], t[16], c[8];
    int r = sscanf("12345", "%3d%d", &a, &b);
    printf("%d: %d %d\n", r, a, b);
    r = sscanf("abcdefgh xy", "%3s%s %s", s, t, c);
    printf("%d: %s %s %s\n", r, s, t, c);
    memset(c, '#', sizeof c);
    c[7] = 0;
    r = sscanf(" xyz", "%3c", c);
    printf("%d: [%s]\n", r, c);
    double f = 0;
    r = sscanf("3.14159", "%4lf%d", &f, &a);
    printf("%d: %g %d\n", r, f, a);
    r = sscanf("-12", "%1d", &a);
    printf("%d\n", r);
    return 0;
}
