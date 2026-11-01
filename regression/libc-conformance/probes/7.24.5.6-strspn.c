/* 7.24.5.6 strspn: length of the initial segment made only of s2's characters; empty s2, empty s1, high bytes */
#include <stdio.h>
#include <string.h>

int main(void) {
    printf("%d %d %d ", (int)strspn("aabbcx", "ba"), (int)strspn("abc", "abc"), (int)strspn("abc", ""));
    printf("%d %d %d\n", (int)strspn("", "abc"), (int)strspn("\xe9\xe9z", "\xe9"), (int)strspn("xyz", "abc"));
    return 0;
}
