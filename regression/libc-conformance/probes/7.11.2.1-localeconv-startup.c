/* 7.11.2.1 localeconv: with no setlocale call the program is in the "C" locale, so decimal_point is "." and frac_digits CHAR_MAX */
#include <stdio.h>
#include <limits.h>
#include <locale.h>

int main(void) {
    struct lconv *lc = localeconv();
    printf("[%s] [%s] [%s] %d\n", lc->decimal_point, lc->thousands_sep, lc->grouping, lc->frac_digits == CHAR_MAX);
    return 0;
}
