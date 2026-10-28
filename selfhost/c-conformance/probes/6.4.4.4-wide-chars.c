// 6.4.4.4p11  wide/u/U character constants: L'x' (wchar_t), u'x' (char16_t), U'x' (char32_t)
#include <stdio.h>
int main(void) {
    int w = L'A';
    printf("%d %d %d %d %d\n", (int)w, (int)u'B', (int)U'C', (int)L'\x263a', (int)sizeof(L'a'));
    return 0;
}
