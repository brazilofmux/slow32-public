/* 7.29.2 and 7.21.6 in a UTF-8 locale: the wide family's widths and precisions count characters, the narrow family's %ls counts bytes and writes whole characters, %lc, %n, and the wide scanf's %ls, %s, %c, %[ and white space */
#include <locale.h>
#include <stdio.h>
#include <string.h>
#include <wchar.h>

static void wide(const wchar_t *s) {
    printf("[");
    for (; *s; s++) printf(*s < 0x80 ? "%c" : "<%X>", (unsigned)*s);
    printf("]\n");
}

static void narrow(const char *s) {
    printf("[");
    for (; *s; s++) printf((unsigned char)*s < 0x80 ? "%c" : "<%02X>", (unsigned char)*s);
    printf("]\n");
}

int main(void) {
    wchar_t w[80], ws[16];
    char b[80], mb[16];
    int r, n = -1, i;
    wchar_t c[3];
    if (!setlocale(LC_ALL, "C.UTF-8") && !setlocale(LC_ALL, "en_US.UTF-8")) {
        printf("no UTF-8 locale\n");
        return 0;
    }
    r = swprintf(w, 80, L"%6ls|%-6ls|%.2ls|%4s|%.1s|%3lc|%c%n", L"\x00e9t\x00e9", L"\x00e9t\x00e9", L"\x00e9t\x00e9",
                 "\xC3\xA9", "\xC3\xA9\xC3\xA9", (wint_t)0x2192, 'k', &n);
    printf("swprintf %d n %d ", r, n);
    wide(w);
    r = swprintf(w, 80, L"%d %5.1f %x \x00fc", 42, 2.25, 255u);
    printf("swprintf %d ", r);
    wide(w);
    printf("too small: %d\n", swprintf(w, 5, L"%ls", L"abcdef") < 0);
    r = snprintf(b, sizeof b, "%6ls|%.3ls|%.4ls|%lc|%n", L"\x00e9t\x00e9", L"\x00e9t\x00e9", L"\x00e9t\x00e9", (wint_t)0x2192, &n);
    printf("snprintf %d n %d ", r, n);
    narrow(b);

    r = swscanf(L"  17\x3000\x00e9t\x00e9 caf\x00e9 \x2192xy\x00e9\x00e8z", L"%d%ls%s %3lc%[\x00e8\x00e9xyz]%n",
                &i, ws, mb, c, b, &n);
    printf("swscanf %d %d n %d ", r, i, n);
    wide(ws);
    narrow(mb);
    printf("chars %X %X %X ", (unsigned)c[0], (unsigned)c[1], (unsigned)c[2]);
    narrow(b);
    r = sscanf("\xC3\xA9t\xC3\xA9 \xE2\x86\x92", "%ls %lc", ws, c);
    printf("sscanf %d %X ", r, (unsigned)c[0]);
    wide(ws);
    return 0;
}
