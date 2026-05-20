#include "libc_x64.h"

int main(int argc, char **argv) {
    char buf[64];

    /* Test write */
    write(1, "Hello from libc!\n", 17);

    /* Test printf */
    fprintf(stderr, "argc=%d\n", argc, 0, 0, 0, 0, 0, 0, 0);

    /* Test string functions */
    if (strlen("hello") != 5) return 1;
    if (strcmp("abc", "abc") != 0) return 2;
    if (strcmp("abc", "abd") >= 0) return 3;

    /* Test memcpy */
    memcpy(buf, "test123", 8);
    if (strcmp(buf, "test123") != 0) return 4;

    /* Test memset */
    memset(buf, 'A', 5);
    buf[5] = 0;
    if (strcmp(buf, "AAAAA") != 0) return 5;

    /* Test malloc */
    {
        char *p;
        p = malloc(100);
        if (!p) return 6;
        memcpy(p, "dynamic!", 9);
        if (strcmp(p, "dynamic!") != 0) return 7;
    }

    /* Test getenv */
    {
        char *home;
        home = getenv("HOME");
        if (!home) return 8;
    }

    /* Test fprintf to stderr */
    fprintf(stderr, "test: %s %d 0x%08X\n", "ok", 42, 255, 0, 0, 0, 0, 0);

    write(1, "All libc tests passed!\n", 23);
    return 0;
}
