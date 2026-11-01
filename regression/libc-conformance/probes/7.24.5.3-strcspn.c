/* 7.24.5.3 strcspn: length of the initial segment free of s2's characters; empty s2, empty s1, high bytes */
#include <stdio.h>
#include <string.h>

int main(void) {
    printf("%d %d %d ", (int)strcspn("abcdef", "fd"), (int)strcspn("abcdef", "xyz"), (int)strcspn("abcdef", ""));
    printf("%d %d %d\n", (int)strcspn("", "abc"), (int)strcspn("ab\xe9z", "\xe9"), (int)strcspn("abc", "a"));
    return 0;
}
