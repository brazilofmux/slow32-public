// 6.4.5p6  L, u and U string literals: arrays of wchar_t/char16_t/char32_t; mixed with plain concatenates
#include <stdio.h>
int main(void) {
    const int *w = (const int *)L"AB";
    const unsigned short *u16 = u"CD";
    const unsigned *u32 = U"EF";
    printf("%d %d %d %d %d %d\n", w[1], u16[0], (int)u32[1], (int)sizeof L"AB", (int)sizeof u"CD",
           (int)sizeof(L"x" "yz"));
    return 0;
}
