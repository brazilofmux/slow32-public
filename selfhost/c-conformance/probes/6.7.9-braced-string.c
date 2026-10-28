// 6.7.9p14  a character array may be initialized by a string literal enclosed in braces
#include <stdio.h>
int main(void) {
    char a[] = { "hi" };
    char b[5] = { "abc" };
    printf("%u %s %s\n", (unsigned)sizeof a, a, b);
    return 0;
}
