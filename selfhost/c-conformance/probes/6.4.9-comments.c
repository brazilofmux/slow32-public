// 6.4.9p1-p2  /* */ and // comments; comment-like text inside literals; a comment is one space
#include <stdio.h>
#include <string.h>
#define TWO 1/**/+/**/1
int main(void) {
    const char *s = "/* not a comment */ // nor this";
    int a = 10 /* inner // slashes */ + 2; // trailing
    int b = 8 //* c99: line comment wins, so this is 8 / not 8 */ 2
        ;
    char c = '/'; /* "unterminated quote inside comment ' */
    int/**/d = 3;
    printf("%d %d %d %c %d %d\n", (int)strlen(s), a, b, c, d, TWO);
    return 0;
}
