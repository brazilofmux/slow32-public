// 6.2.5p15  char has the same range as signed char or unsigned char (implementation choice must match clang's ABI)
#include <stdio.h>
#include <limits.h>
int main(void) {
    char c = (char)200;
    printf("%d %d %d\n", (int)c, CHAR_MIN, CHAR_MAX);
    return 0;
}
