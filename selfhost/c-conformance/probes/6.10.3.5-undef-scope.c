// 6.10.3.5p1,p2  #undef ends a definition; scope ignores blocks; #undef of nothing is fine
#include <stdio.h>
#undef NEVER_DEFINED
static int f(void) {
#define LOCAL 3
    return LOCAL;
}
int main(void) {
    int a = LOCAL;          /* still defined: macros ignore block scope */
#undef LOCAL
    int LOCAL = 9;          /* now an ordinary identifier */
#define LOCAL 4
    printf("%d %d %d\n", f(), a, LOCAL);
    return 0;
}
