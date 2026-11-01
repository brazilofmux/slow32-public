/* 7.29.4 wide string functions: wcslen, wcscpy, wcscat, wcscmp, wcsncmp, wcschr, wcsrchr, wcsstr, wcsspn, wmemset, wmemcpy, wmemcmp */
#include <stdio.h>
#include <wchar.h>

int main(void) {
    wchar_t a[32], b[32];
    wcscpy(a, L"hello");
    wcscat(a, L", world");
    printf("wcslen %u\n", (unsigned)wcslen(a));
    printf("wcscmp %d %d %d\n", wcscmp(a, L"hello, world") == 0, wcscmp(L"abc", L"abd") < 0, wcscmp(L"b", L"a") > 0);
    printf("wcsncmp %d\n", wcsncmp(L"hello", L"help", 3) == 0);
    printf("wcschr %d wcsrchr %d\n", (int)(wcschr(a, L'o') - a), (int)(wcsrchr(a, L'o') - a));
    printf("wcsstr %d\n", (int)(wcsstr(a, L"world") - a));
    printf("wcsspn %u wcscspn %u\n", (unsigned)wcsspn(a, L"hel"), (unsigned)wcscspn(a, L" "));
    wmemset(b, L'x', 4);
    b[4] = 0;
    printf("wmemset %u %d\n", (unsigned)wcslen(b), b[3] == L'x');
    wmemcpy(b, L"abcd", 4);
    printf("wmemcmp %d\n", wmemcmp(b, L"abcd", 4) == 0);
    printf("wcsncpy pads: ");
    wcsncpy(b, L"ab", 6);
    printf("%d %d\n", b[2] == 0, b[5] == 0);
    return 0;
}
