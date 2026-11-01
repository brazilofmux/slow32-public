/* 7.30.2 wide classification in the "C" locale: each isw* function and wctype/iswctype agree with the narrow class for 0-127 */
#include <ctype.h>
#include <stdio.h>
#include <wctype.h>

int main(void) {
    int c, bad = 0, n[12] = {0};
    for (c = 0; c < 128; c++) {
        wint_t w = (wint_t)c;
        bad += !iswalnum(w) != !isalnum(c);
        bad += !iswalpha(w) != !isalpha(c);
        bad += !iswblank(w) != !isblank(c);
        bad += !iswcntrl(w) != !iscntrl(c);
        bad += !iswdigit(w) != !isdigit(c);
        bad += !iswgraph(w) != !isgraph(c);
        bad += !iswlower(w) != !islower(c);
        bad += !iswprint(w) != !isprint(c);
        bad += !iswpunct(w) != !ispunct(c);
        bad += !iswspace(w) != !isspace(c);
        bad += !iswupper(w) != !isupper(c);
        bad += !iswxdigit(w) != !isxdigit(c);
        n[0] += !!iswctype(w, wctype("alpha"));
        n[1] += !!iswctype(w, wctype("digit"));
        n[2] += !!iswctype(w, wctype("punct"));
    }
    printf("disagreements with ctype: %d\n", bad);
    printf("iswctype alpha %d digit %d punct %d\n", n[0], n[1], n[2]);
    printf("wctype of an unknown name is 0: %d\n", wctype("nonsense") == 0);
    printf("WEOF in no class: %d\n", !iswalpha(WEOF) && !iswspace(WEOF));
    return 0;
}
