/* 7.29.3 in a UTF-8 locale: a file written with fputwc, fputws and fwprintf holds the multibyte forms; fgetwc, ungetwc, fgetws and fwscanf read them back; fwide reports the orientation each stream took */
#include <locale.h>
#include <stdio.h>
#include <wchar.h>

int main(void) {
    FILE *f;
    wchar_t line[32];
    wint_t c;
    int ch, n = 0, k;
    unsigned v = 0;
    if (!setlocale(LC_ALL, "C.UTF-8") && !setlocale(LC_ALL, "en_US.UTF-8")) {
        printf("no UTF-8 locale\n");
        return 0;
    }
    f = tmpfile();
    if (!f) return 1;
    printf("new stream: %d\n", fwide(f, 0));
    fputwc(0x00C6, f);
    fputws(L"\x0393\x03B5\x03B9\x03AC\n", f);
    fwprintf(f, L"%ls=%d\n", L"\x20AC", 7);
    printf("written wide: %d\n", fwide(f, 0) > 0);
    rewind(f);
    printf("after rewind still wide: %d\n", fwide(f, 0) > 0);
    c = fgetwc(f);
    printf("first U+%04X\n", (unsigned)c);
    printf("ungetwc U+%04X\n", (unsigned)ungetwc(0x00E6, f));
    printf("read back U+%04X\n", (unsigned)fgetwc(f));
    if (fgetws(line, 32, f)) {
        printf("line:");
        for (k = 0; line[k]; k++) printf(" %X", (unsigned)line[k]);
        printf("\n");
    }
    k = fwscanf(f, L"\x20AC=%u", &v);
    printf("fwscanf %d %u\n", k, v);
    fgetwc(f);
    printf("end: %d feof %d\n", fgetwc(f) == WEOF, feof(f) != 0);
    fclose(f);

    f = tmpfile();
    if (!f) return 1;
    fputs("\xC3\xA9!", f);
    printf("byte stream: %d", fwide(f, 0) < 0);
    printf(", asking for wide stays byte: %d\n", fwide(f, 1) < 0);
    rewind(f);
    while ((ch = fgetc(f)) != EOF) n++;
    printf("bytes read %d\n", n);
    fclose(f);
    return 0;
}
