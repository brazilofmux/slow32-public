/* 7.29.6, 7.28.1, 7.22.7 in a UTF-8 locale: a string of one-, two-, three- and four-byte characters both ways, a character split across calls, malformed sequences, surrogate pairs through char16_t */
#include <errno.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <uchar.h>
#include <wchar.h>

static void bytes(const char *s, size_t n) {
    size_t i;
    for (i = 0; i < n; i++) printf("%02X", (unsigned char)s[i]);
}

int main(void) {
    static const char text[] = "q\xC3\xB1\xE2\x88\x9E\xF0\x9D\x84\x9E.";   /* q, n-tilde, infinity, G clef, . */
    static const char *const broken[] = {
        "\xC1\xBF", "\xE0\x80\xAF", "\xED\xB0\x80", "\xBF", "\xF8\x88\x80\x80\x80", "\xE2\x28\xA1"
    };
    wchar_t w[16], wc;
    char out[32];
    const char *src;
    const wchar_t *wsrc;
    mbstate_t st;
    size_t r, i;
    char16_t u;

    if (!setlocale(LC_ALL, "C.UTF-8") && !setlocale(LC_ALL, "en_US.UTF-8")) {
        printf("no UTF-8 locale\n");
        return 0;
    }
    printf("MB_CUR_MAX at least 4: %d\n", MB_CUR_MAX >= 4);
    memset(&st, 0, sizeof st);
    src = text;
    r = mbsrtowcs(w, &src, 16, &st);
    printf("to wide: %u, done %d:", (unsigned)r, src == NULL);
    for (i = 0; i < r; i++) printf(" U+%04X", (unsigned)w[i]);
    printf("\n");
    memset(&st, 0, sizeof st);
    wsrc = w;
    r = wcsrtombs(out, &wsrc, sizeof out, &st);
    printf("back: %u same %d\n", (unsigned)r, strcmp(out, text) == 0);
    memset(&st, 0, sizeof st);
    wsrc = w;
    r = wcsrtombs(out, &wsrc, 5, &st);
    printf("five bytes hold %u, stopped at element %d\n", (unsigned)r, (int)(wsrc - w));
    printf("length only: %u\n", (unsigned)wcstombs(NULL, w, 0));

    memset(&st, 0, sizeof st);
    r = mbrtowc(&wc, text + 6, 2, &st);
    printf("split: %d initial %d", (int)r, mbsinit(&st) != 0);
    r = mbrtowc(&wc, text + 8, 2, &st);
    printf(", then %d U+%X initial %d\n", (int)r, (unsigned)wc, mbsinit(&st) != 0);
    for (i = 0; i < sizeof broken / sizeof broken[0]; i++) {
        memset(&st, 0, sizeof st);
        errno = 0;
        r = mbrtowc(&wc, broken[i], strlen(broken[i]), &st);
        printf("malformed %u: %d EILSEQ %d\n", (unsigned)i, (int)r, errno == EILSEQ);
    }
    errno = 0;
    printf("lone surrogate out: %d EILSEQ %d\n", (int)wcrtomb(out, 0xDFFF, NULL), errno == EILSEQ);
    printf("mblen %d %d, mbtowc %d, wctomb %d\n", mblen(text + 3, 3), mblen(text + 3, 2),
           mbtowc(&wc, text + 6, 4), wctomb(out, L'\x20AC'));
    printf("btowc of a lead byte is WEOF: %d, wctob of U+00F1 is EOF: %d\n", btowc(0xC3) == WEOF, wctob(0xF1) == EOF);

    memset(&st, 0, sizeof st);
    r = mbrtoc16(&u, text + 6, 4, &st);
    printf("mbrtoc16: %d %04X", (int)r, (unsigned)u);
    r = mbrtoc16(&u, text + 10, 1, &st);
    printf(" then %d %04X", (int)r, (unsigned)u);
    r = mbrtoc16(&u, text + 10, 1, &st);
    printf(" then %d %04X\n", (int)r, (unsigned)u);
    memset(&st, 0, sizeof st);
    r = c16rtomb(out, 0xD834, &st);
    printf("c16rtomb high %d", (int)r);
    r = c16rtomb(out, 0xDD1E, &st);
    printf(", low %d [", (int)r);
    bytes(out, r);
    printf("]\n");
    r = c32rtomb(out, 0x1D11E, NULL);
    printf("c32rtomb %d, mbrtoc32 ", (int)r);
    {
        char32_t c32;
        printf("%d U+%X\n", (int)mbrtoc32(&c32, out, r, NULL), (unsigned)c32);
    }
    return 0;
}
