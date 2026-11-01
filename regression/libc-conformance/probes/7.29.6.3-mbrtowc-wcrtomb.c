/* 7.29.6 mbsinit, mbrlen, mbrtowc, wcrtomb, mbsrtowcs, wcsrtombs on ASCII in the "C" locale */
#include <stdio.h>
#include <string.h>
#include <wchar.h>

int main(void) {
    mbstate_t st;
    wchar_t wc = 0, wbuf[16];
    char buf[16];
    const char *src = "abc";
    const wchar_t *wsrc = L"xyz";
    size_t r;
    memset(&st, 0, sizeof st);
    printf("mbsinit %d\n", mbsinit(&st) != 0);
    r = mbrtowc(&wc, "A", 1, &st);
    printf("mbrtowc A -> %u, %d\n", (unsigned)r, (int)wc);
    r = mbrtowc(&wc, "", 1, &st);
    printf("mbrtowc nul -> %u, %d\n", (unsigned)r, (int)wc);
    printf("mbrlen %u\n", (unsigned)mbrlen("q", 1, &st));
    r = wcrtomb(buf, L'B', &st);
    printf("wcrtomb B -> %u [%c]\n", (unsigned)r, buf[0]);
    r = mbsrtowcs(wbuf, &src, 16, &st);
    printf("mbsrtowcs -> %u, src null %d, %d %d %d\n", (unsigned)r, src == NULL, (int)wbuf[0], (int)wbuf[1], (int)wbuf[2]);
    r = wcsrtombs(buf, &wsrc, sizeof buf, &st);
    printf("wcsrtombs -> %u [%s]\n", (unsigned)r, buf);
    printf("btowc %d wctob %d btowc(EOF) is WEOF %d\n", (int)btowc('m'), wctob(L'n'), btowc(EOF) == WEOF);
    return 0;
}
