// 6.10.8.1p1  __LINE__ is the current line (an int constant); __FILE__ a string literal
#include <stdio.h>
#include <string.h>
#define HERE __LINE__
static const char *base(const char *p) { const char *s = strrchr(p, '/'); return s ? s + 1 : p; }
int main(void) {
    int a = __LINE__;
    int b = HERE;
    int c =
        __LINE__;
    char f[] = __FILE__;           /* must be a string literal to initialize an array */
    printf("%d %d %d %s %d\n", a, b, c, base(f), (int)sizeof(__LINE__));
    return 0;
}
