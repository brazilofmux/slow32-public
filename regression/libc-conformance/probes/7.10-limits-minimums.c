/* 7.10, 5.2.4.2.1p1: each <limits.h> value is at least the minimum magnitude, tested in #if and at run time */
#include <limits.h>
#include <stdio.h>

#if CHAR_BIT >= 8 && SCHAR_MIN <= -127 && SCHAR_MAX >= 127 && UCHAR_MAX >= 255 \
    && SHRT_MIN <= -32767 && SHRT_MAX >= 32767 && USHRT_MAX >= 65535 \
    && INT_MIN <= -32767 && INT_MAX >= 32767 && UINT_MAX >= 65535 \
    && LONG_MIN <= -2147483647 && LONG_MAX >= 2147483647 && ULONG_MAX >= 4294967295 \
    && LLONG_MIN <= -9223372036854775807 && LLONG_MAX >= 9223372036854775807 && ULLONG_MAX >= 18446744073709551615u
#define PP 1
#else
#define PP 0
#endif

int main(void) {
    printf("#if %d\n", PP);
    printf("char %d %d %d %d\n", CHAR_BIT >= 8, SCHAR_MIN <= -127, SCHAR_MAX >= 127, UCHAR_MAX >= 255);
    printf("short %d %d %d\n", SHRT_MIN <= -32767, SHRT_MAX >= 32767, USHRT_MAX >= 65535);
    printf("int %d %d %d\n", INT_MIN <= -32767, INT_MAX >= 32767, UINT_MAX >= 65535);
    printf("long %d %d %d\n", LONG_MIN <= -2147483647, LONG_MAX >= 2147483647, ULONG_MAX >= 4294967295);
    printf("llong %d %d %d\n", LLONG_MIN <= -9223372036854775807, LLONG_MAX >= 9223372036854775807,
           ULLONG_MAX >= 18446744073709551615u);
    return 0;
}
