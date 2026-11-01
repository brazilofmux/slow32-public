/* 7.4.2.1 tolower: maps exactly the uppercase letters to their lowercase counterparts and returns every other unsigned char value and EOF unchanged */
#include <ctype.h>
#include <stdio.h>

int main(void) {
    int c, changed = 0;
    printf("EOF %d\n", tolower(EOF) == EOF);
    for (c = 0; c < 256; c++) {
        int r = tolower(c);
        if (r != c) {
            printf("%02x>%02x%s", c, r, ++changed % 8 ? " " : "\n");
        }
    }
    printf("changed %d\n", changed);
    return 0;
}
