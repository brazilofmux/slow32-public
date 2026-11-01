/* 7.24.6.3 strlen: characters before the terminator, any start alignment, bytes above 127 count */
#include <stdio.h>
#include <string.h>

int main(void) {
    char b[80];
    int bad = 0, k, n;
    printf("%d %d %d\n", (int)strlen(""), (int)strlen("\xff\x80\x01"), (int)strlen("ab\0cd"));
    for (k = 0; k < 8; k++)
        for (n = 0; n < 64; n++) {
            memset(b, '\x81', sizeof b);
            b[k + n] = '\0';
            if (strlen(b + k) != (size_t)n) bad++;
        }
    printf("wrong %d\n", bad);
    return 0;
}
