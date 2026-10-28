// 6.5.1.1p2-3  _Generic selects by controlling type (after lvalue conversion), default
#include <stdio.h>
#define T(x) _Generic((x), int: "int", unsigned: "unsigned", long: "long", \
    long long: "llong", char: "char", double: "double", float: "float", \
    char *: "char*", const char *: "cchar*", default: "other")
int main(void) {
    char c = 0; const int ci = 1; short s = 0; float f = 0;
    char buf[3];
    printf("%s %s %s %s\n", T(1), T(1u), T(1L), T(1LL));
    printf("%s %s %s %s\n", T(c), T(ci), T(s), T(f));
    printf("%s %s %s\n", T(1.0), T(buf), T("lit"));
    printf("%d\n", _Generic(1, default: 3, int: 4));
    return 0;
}
