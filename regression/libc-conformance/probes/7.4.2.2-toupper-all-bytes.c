/* 7.4.2.2 toupper: maps exactly the lowercase letters to their uppercase counterparts and returns every other unsigned char value and EOF unchanged */
#include <ctype.h>
#include <stdio.h>

int main(void) {
    int c, changed = 0;
    printf("EOF %d\n", toupper(EOF) == EOF);
    for (c = 0; c < 256; c++) {
        int r = toupper(c);
        if (r != c) {
            printf("%02x>%02x%s", c, r, ++changed % 8 ? " " : "\n");
        }
    }
    printf("changed %d\n", changed);
    return 0;
}
