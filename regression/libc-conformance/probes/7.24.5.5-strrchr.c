/* 7.24.5.5 strrchr: last occurrence, the terminator counts, c converted to char, absent gives null */
#include <stdio.h>
#include <string.h>

static long at(const char *p, const char *base) { return p ? (long)(p - base) : -1L; }

int main(void) {
    const char s[] = "abcabc\xe9x\xe9";
    printf("%ld %ld %ld ", at(strrchr(s, 'a'), s), at(strrchr(s, 'c'), s), at(strrchr(s, 'q'), s));
    printf("%ld %ld ", at(strrchr(s, '\0'), s), at(strrchr("", '\0'), ""));
    printf("%ld %ld %ld\n", at(strrchr(s, (char)0xe9), s), at(strrchr(s, 'b' + 256), s), at(strrchr("", 'a'), ""));
    return 0;
}
