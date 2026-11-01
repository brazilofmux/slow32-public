/* 7.11.2.1 localeconv: the "C" locale's values -- "." for decimal_point, "" for the other strings, CHAR_MAX for the char members */
#include <stdio.h>
#include <limits.h>
#include <locale.h>

#define S(f) printf(#f " [%s]\n", lc->f)
#define C(f) printf(#f " %s\n", lc->f == CHAR_MAX ? "CHAR_MAX" : "other")

int main(void) {
    struct lconv *lc;
    setlocale(LC_ALL, "C");
    lc = localeconv();
    S(decimal_point); S(thousands_sep); S(grouping); S(mon_decimal_point); S(mon_thousands_sep);
    S(mon_grouping); S(positive_sign); S(negative_sign); S(currency_symbol); S(int_curr_symbol);
    C(int_frac_digits); C(frac_digits); C(p_cs_precedes); C(p_sep_by_space); C(n_cs_precedes);
    C(n_sep_by_space); C(p_sign_posn); C(n_sign_posn); C(int_p_cs_precedes); C(int_p_sep_by_space);
    C(int_n_cs_precedes); C(int_n_sep_by_space); C(int_p_sign_posn); C(int_n_sign_posn);
    return 0;
}
