/* 7.29.4.1 wcstol, wcstoul, wcstod: wide numeric conversion, with the end pointer */
#include <stdio.h>
#include <wchar.h>

int main(void) {
    const wchar_t *s = L"  -1234xyz";
    wchar_t *end;
    long v = wcstol(s, &end, 10);
    printf("wcstol %ld, stopped at %d\n", v, (int)(end - s));
    printf("wcstoul hex %lu\n", wcstoul(L"0x1F", NULL, 0));
    printf("wcstod %.3f\n", wcstod(L"2.5e1", NULL));
    return 0;
}
