/* 7.20.3: PTRDIFF, SIZE, SIG_ATOMIC, WCHAR and WINT limit macros exist, meet their minima and match their types */
#include <signal.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <wchar.h>

int main(void) {
    printf("ptrdiff %d %d %d\n", PTRDIFF_MIN <= -65535, PTRDIFF_MAX >= 65535, _Generic(PTRDIFF_MAX, ptrdiff_t: 1, default: 0));
    printf("size %d %d %d\n", SIZE_MAX >= 65535, SIZE_MAX == (size_t)-1, _Generic(SIZE_MAX, size_t: 1, default: 0));
    printf("sig_atomic %d\n", SIG_ATOMIC_MIN == 0 ? SIG_ATOMIC_MAX >= 255 : SIG_ATOMIC_MIN <= -127 && SIG_ATOMIC_MAX >= 127);
    printf("wchar %d %d\n", WCHAR_MIN == 0 ? WCHAR_MAX >= 255 : WCHAR_MIN <= -127 && WCHAR_MAX >= 127,
           ((wchar_t)-1 < 0) == (WCHAR_MIN < 0));
    printf("wint %d %d\n", WINT_MIN == 0 ? WINT_MAX >= 65535 : WINT_MIN <= -32767 && WINT_MAX >= 32767,
           ((wint_t)-1 < 0) == (WINT_MIN < 0));
    return 0;
}
