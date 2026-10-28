// 6.7.6.1p1, 6.5.3.4p2  each * in a function-pointer declarator is a pointer level: sizeof of an element of a pointer to function pointers is a pointer's size
#include <stdio.h>
static void hello(void) { }
static void (*one[3])(void) = { hello, hello, hello };
struct Tab { void (**aExt)(void); int n; };
int main(void) {
    void (**pp)(void) = one;
    struct Tab t = { one, 3 };
    printf("%d %d %d %d\n", (int)sizeof(pp[0]), (int)sizeof(*t.aExt), (int)sizeof(t.aExt[1]),
           (int)(sizeof(one) / sizeof(one[0])));
    return 0;
}
