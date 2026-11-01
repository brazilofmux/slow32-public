/* 7.4.1.8 isprint: in the "C" locale true for exactly the printing characters including space, over every unsigned char value and EOF */
#include <ctype.h>
#include <stdio.h>

int main(void) {
    int c, start = -1, count = 0;
    printf("EOF %d\n", isprint(EOF) != 0);
    for (c = 0; c <= 256; c++) {
        int t = c < 256 && isprint(c) != 0;
        if (t && start < 0)
            start = c;
        if (!t && start >= 0) {
            printf("%02x-%02x\n", start, c - 1);
            count += c - start;
            start = -1;
        }
    }
    printf("count %d\n", count);
    return 0;
}
