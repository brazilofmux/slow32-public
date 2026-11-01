/* 7.24.2.4 strncpy: a source shorter than n is followed by null characters up to n in all, nothing beyond n is written */
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
    printf("%d ", strncpy(b, "ab", 7) == b);
    show(b, 10);
    memset(b, '#', sizeof b);
    strncpy(b + 1, "", 5);
    show(b, 10);
    memset(b, '#', sizeof b);
    strncpy(b, "abc\0xyz", 6);
    show(b, 10);
    return 0;
}
