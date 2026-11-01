/* 7.19p2, 6.4.4.4p11, 6.4.5p6: <stddef.h>'s wchar_t is the type of wide character constants and wide string elements */
#include <stddef.h>
#include <stdio.h>

int main(void) {
    wchar_t w = L'z';
    printf("%d %d %d\n", _Generic(L'a', wchar_t: 1, default: 0), _Generic(L"ab"[0], wchar_t: 1, default: 0),
           _Generic(&L"ab"[0], wchar_t *: 1, default: 0));
    printf("%d %d\n", w == 'z', sizeof(wchar_t) == sizeof L'a');
    return 0;
}
