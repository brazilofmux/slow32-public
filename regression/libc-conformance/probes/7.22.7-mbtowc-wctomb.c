/* 7.22.7.1-7.22.7.3 mblen, mbtowc, wctomb: single-byte characters in the C locale, null and empty cases */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

int main(void) {
    wchar_t wc = 0;
    char buf[16];
    int n;
    mblen(NULL, 0); mbtowc(NULL, NULL, 0); wctomb(NULL, 0);    /* reset the states */
    printf("mblen: %d %d %d\n", mblen("abc", 3), mblen("", 1), mblen("a", 0) == -1 || mblen("a", 0) == 0);
    n = mbtowc(&wc, "Q", 1);
    printf("mbtowc: %d %d\n", n, wc == L'Q');
    n = mbtowc(&wc, "", 1);
    printf("mbtowc nul: %d %d\n", n, wc == 0);
    printf("mbtowc no pwc: %d\n", mbtowc(NULL, "z", 1));
    n = wctomb(buf, L'Z');
    printf("wctomb: %d %d\n", n, n == 1 && buf[0] == 'Z');
    n = wctomb(buf, L'\0');
    printf("wctomb nul: %d %d\n", n, buf[0] == 0);
    return 0;
}
