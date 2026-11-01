/* 7.24.5.2 strchr: first occurrence, the terminator counts as part of the string, c converted to char, bytes above 127 */
#include <stdio.h>
#include <string.h>

static long at(const char *p, const char *base) { return p ? (long)(p - base) : -1L; }

int main(void) {
    const char s[] = "abca\xe9z";
    char big[80];
    int i, k, bad = 0;
    printf("%ld %ld %ld ", at(strchr(s, 'a'), s), at(strchr(s, 'c'), s), at(strchr(s, 'q'), s));
    printf("%ld %ld ", at(strchr(s, '\0'), s), at(strchr("", '\0'), ""));
    printf("%ld %ld\n", at(strchr(s, (char)0xe9), s), at(strchr(s, 'z' + 256), s));
    for (k = 0; k < 4; k++)
        for (i = k; i < 70; i++) {
            memset(big, 'x', sizeof big);
            big[70] = '\0';
            big[i] = 'Q';
            if (strchr(big + k, 'Q') != big + i) bad++;
            if (strchr(big + k, '\0') != big + 70) bad++;
            big[i] = '\0';
            if (strchr(big + k, 'x') != (i == k ? NULL : big + k)) bad++;
        }
    printf("wrong %d\n", bad);
    return 0;
}
