/* 7.29.2.11 wprintf / 7.29.3 fputws: a wide-oriented stdout writes wide text as multibyte characters */
#include <stdio.h>
#include <wchar.h>

int main(void) {
    /* stdout is used only wide from here on */
    wprintf(L"wide %d %ls\n", 7, L"text");
    fputws(L"fputws line\n", stdout);
    fputwc(L'Z', stdout);
    putwchar(L'\n');
    wprintf(L"oriented wide: %d\n", fwide(stdout, 0) > 0);
    return 0;
}
