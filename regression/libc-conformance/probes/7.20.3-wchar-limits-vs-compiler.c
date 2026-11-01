/* 7.20.3, 6.4.4.4p11: WCHAR_MIN says whether wchar_t is signed, and wide character constants have that signedness */
#include <stdint.h>
#include <stdio.h>

int main(void) {
    printf("%d %d\n", (L'\0' - 1 < 0) == (WCHAR_MIN < 0), (L'\0' - 1 > 0) == (WCHAR_MIN == 0));
    return 0;
}
