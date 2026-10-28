// 6.4.4.4p11 (7.19p2)  L'x' has type wchar_t, which <stddef.h> must define
#include <stdio.h>
#include <stddef.h>
int main(void) {
    wchar_t w = L'Z';
    printf("%d %d\n", (int)w, (int)(sizeof(wchar_t) == sizeof L'Z'));
    return 0;
}
