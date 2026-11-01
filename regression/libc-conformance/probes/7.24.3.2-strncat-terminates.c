/* 7.24.3.2 strncat: appends at most n characters, stops at s2's null, always writes a terminator; n == 0 */
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
    strcpy(b, "ab");
    printf("%d ", strncat(b, "cdefg", 3) == b);
    show(b, 10);
    memset(b, '#', sizeof b);
    strcpy(b, "ab");
    strncat(b, "cd", 9);
    show(b, 10);
    memset(b, '#', sizeof b);
    strcpy(b, "ab");
    strncat(b, "cd", 2);
    show(b, 10);
    memset(b, '#', sizeof b);
    strcpy(b, "ab");
    strncat(b, "cd", 0);
    show(b, 10);
    /* s2 need not be terminated when n bytes suffice */
    {
        char raw[3] = { 'x', 'y', 'z' };
        memset(b, '#', sizeof b);
        b[0] = '\0';
        strncat(b, raw, 3);
        show(b, 6);
    }
    return 0;
}
