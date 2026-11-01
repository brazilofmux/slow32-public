/* 7.24.3.1 strcat: appends s2 over s1's terminator, terminates, returns s1; empty strings either side */
#include <stdio.h>
#include <string.h>

int main(void) {
    char b[32];
    memset(b, '#', sizeof b);
    b[0] = '\0';
    printf("%d ", strcat(b, "") == b);
    strcat(b, "abc");
    strcat(b, "");
    strcat(b, "de");
    printf("[%s] %d %c\n", b, (int)strlen(b), b[6]);
    return 0;
}
