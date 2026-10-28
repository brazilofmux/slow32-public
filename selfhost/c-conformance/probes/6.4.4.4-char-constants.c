// 6.4.4.4p2-p10  character constants: escapes, octal/hex escapes, type int, signedness of plain char
#include <stdio.h>
int main(void) {
    printf("%d %d %d %d %d %d %d %d %d %d %d\n", 'a', '\n', '\t', '\\', '\'', '"', '\"', '\?', '\0', '\a', '\v');
    printf("%d %d %d %d %d %d %d\n", '\b', '\f', '\r', '\101', '\x41', '\377', '\xff');
    printf("%d %d\n", (int)sizeof 'a', '\7');
    return 0;
}
