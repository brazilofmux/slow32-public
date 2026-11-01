/* 7.21.6.5 snprintf: returns the untruncated length, writes at most n-1 characters plus a NUL, and n == 0 writes nothing */
#include <stdio.h>
#include <string.h>

int main(void) {
    char b[16];
    memset(b, 'Z', sizeof b);
    int r = snprintf(b, 5, "hello world");
    printf("%d [%s] %c\n", r, b, b[5]);
    memset(b, 'Z', sizeof b);
    r = snprintf(b, 0, "abc%d", 12345);
    printf("%d %c\n", r, b[0]);
    r = snprintf(NULL, 0, "%s-%d", "xyz", -7);
    printf("%d\n", r);
    memset(b, 'Z', sizeof b);
    r = snprintf(b, 1, "abc");
    printf("%d [%s] %c\n", r, b, b[1]);
    memset(b, 'Z', sizeof b);
    r = snprintf(b, 4, "abc");
    printf("%d [%s]\n", r, b);
    r = snprintf(b, 3, "abc");
    printf("%d [%s]\n", r, b);
    r = snprintf(b, sizeof b, "");
    printf("%d [%s]\n", r, b);
    r = snprintf(b, sizeof b, "a%cb", 0);
    printf("%d %d %d %d\n", r, b[0], b[1], b[2]);
    return 0;
}
