// 6.9.1p6,p10  old-style definitions: declaration list in any order, char/float/short params promoted at the call
#include <stdio.h>
int add3(a, b, c)
    double c;
    int a;
    char b;
{
    return a + b + (int)c;
}
float half(f) float f; { return f / 2; }
long mix(s, p, n)
    short s; const char *p; register long n;
{
    return s * 100 + p[1] + n;
}
int main(void) {
    printf("%d %d %.2f %ld\n", add3(1, 2, 3.75), add3(10, 'A', 2.0), half(5.0f), mix(3, "xyz", 4L));
    return 0;
}
