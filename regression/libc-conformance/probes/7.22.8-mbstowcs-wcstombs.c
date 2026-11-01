/* 7.22.8.1 mbstowcs, 7.22.8.2 wcstombs: string conversion in the C locale, count limits, null destination */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

int main(void) {
    wchar_t w[16];
    char c[16];
    size_t n;
    n = mbstowcs(w, "hello", 16);
    printf("%u %d %d\n", (unsigned)n, w[0] == L'h' && w[4] == L'o', w[5] == 0);
    n = mbstowcs(w, "hello", 3);
    printf("%u %d\n", (unsigned)n, w[2] == L'l');
    printf("%u\n", (unsigned)mbstowcs(NULL, "count me", 0));
    n = wcstombs(c, L"world", sizeof c);
    printf("%u %s\n", (unsigned)n, c);
    memset(c, '#', sizeof c);
    n = wcstombs(c, L"world", 2);
    printf("%u %c%c%c\n", (unsigned)n, c[0], c[1], c[2]);
    printf("%u\n", (unsigned)wcstombs(NULL, L"abcdef", 0));
    return 0;
}
