/* 7.24.4.4 strncmp: at most n characters, stops at a null, unsigned char order, n == 0 is equal */
#include <stdio.h>
#include <string.h>

static int sgn(int v) { return (v > 0) - (v < 0); }

int main(void) {
    printf("%d ", sgn(strncmp("abcX", "abcY", 3)));
    printf("%d ", sgn(strncmp("abcX", "abcY", 4)));
    printf("%d ", sgn(strncmp("ab\0X", "ab\0Y", 4)));
    printf("%d ", sgn(strncmp("ab", "abc", 5)));
    printf("%d ", sgn(strncmp("x", "y", 0)));
    printf("%d ", sgn(strncmp("\xe9", "e", 1)));
    printf("%d\n", sgn(strncmp("a\x80", "a\x7f", 2)));
    return 0;
}
