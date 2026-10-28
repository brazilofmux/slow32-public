// 6.4.5p6  a literal is a static char array: sizeof, indexing and identical-content addresses
#include <stdio.h>
static const char *f(void) { return "persist"; }
int main(void) {
    const char *p = f();
    char c = "xyz"[1];
    char a5[5] = "abcde";              /* no room for NUL: allowed in C */
    printf("%s %c %d %c\n", p, c, (int)sizeof a5, a5[4]);
    return 0;
}
