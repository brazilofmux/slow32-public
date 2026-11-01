/* 7.24.4.2 strcmp: the sign follows the first differing characters, compared as unsigned char */
#include <stdio.h>
#include <string.h>

int main(void) {
    const char a[] = "\x80x", b[] = "\x01x";
    int r = strcmp(a, b);
    printf("%d %d %d\n", r > 0, strcmp("abc", "abd") < 0, strcmp("", "") == 0);
    return 0;
}
