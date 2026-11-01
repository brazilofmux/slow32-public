/* 7.24.3.2 strncat: n is only an upper bound -- with n of SIZE_MAX or above INT_MAX, s2's null still ends the copy */
#include <stdio.h>
#include <string.h>

int main(void) {
    char b[16];
    strcpy(b, "ab");
    strncat(b, "cd", (size_t)-1);
    printf("[%s] ", b);
    strcpy(b, "ab");
    strncat(b, "ef", (size_t)0x80000000u);
    printf("[%s] ", b);
    strcpy(b, "ab");
    strncat(b, "gh", (size_t)0x7fffffffu);
    printf("[%s]\n", b);
    return 0;
}
