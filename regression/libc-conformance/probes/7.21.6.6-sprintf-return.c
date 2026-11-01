/* 7.21.6.6 sprintf: returns the number of characters written, not counting the terminating null */
#include <stdio.h>

int main(void) {
    char b[64];
    int r = sprintf(b, "%d|%s|%c", -12, "ab", 'q');
    printf("%d [%s]\n", r, b);
    r = sprintf(b, "%s", "");
    printf("%d [%s]\n", r, b);
    r = sprintf(b, "x%cy", 0);
    printf("%d %d %d\n", r, b[1], b[2]);
    r = printf("four");
    printf(" %d\n", r);
    r = printf("%s", "");
    printf("%d\n", r);
    r = printf("%%|%%|\n");
    printf("%d\n", r);
    return 0;
}
