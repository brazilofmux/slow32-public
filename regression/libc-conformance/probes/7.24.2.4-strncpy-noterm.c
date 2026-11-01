/* 7.24.2.4 strncpy: a source of n or more characters leaves s1 unterminated, n == 0 writes nothing */
#include <stdio.h>
#include <string.h>

static void show(const char *b, int len) {
    int i;
    for (i = 0; i < len; i++) putchar(b[i] ? b[i] : '0');
    putchar('\n');
}

int main(void) {
    char b[16];
    memset(b, '#', sizeof b);
    strncpy(b, "abcdef", 4);
    show(b, 8);
    memset(b, '#', sizeof b);
    strncpy(b, "abcd", 4);
    show(b, 8);
    memset(b, '#', sizeof b);
    printf("%d ", strncpy(b, "abcd", 0) == b);
    show(b, 8);
    return 0;
}
