/* HOSTLEG -- every <ctype.h> function over EOF and all 256 byte values,
 * in the C locale: above 127 no byte is in any class or changes case.
 * (Until 2026-10-19 the clang runtime's tables were Latin-1 there, and
 * this stopped at 127.) */
#include <stdio.h>
#include <ctype.h>

int main(void) {
    int c;
    for (c = -1; c < 256; c++) {
        printf("%4d %d%d%d%d%d%d%d%d%d%d%d%d %d %d %d %d\n", c,
               !!isalnum(c), !!isalpha(c), !!isblank(c), !!iscntrl(c),
               !!isdigit(c), !!isgraph(c), !!islower(c), !!isprint(c),
               !!ispunct(c), !!isspace(c), !!isupper(c), !!isxdigit(c),
               tolower(c), toupper(c), c < 0 ? 0 : !!isascii(c), c < 0 ? 0 : toascii(c));
    }
    printf("isascii(128)=%d isascii(255)=%d toascii(200)=%d toascii(511)=%d\n",
           !!isascii(128), !!isascii(255), toascii(200), toascii(511));
    return 0;
}
