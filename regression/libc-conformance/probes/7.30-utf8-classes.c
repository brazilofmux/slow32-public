/* 7.30 in a UTF-8 locale: classes and case mappings of letters, digits, separators and symbols past ASCII, wctype and wctrans by name, and the class relations 7.30.2.1 requires over the Basic Multilingual Plane */
#include <locale.h>
#include <stdio.h>
#include <wctype.h>

int main(void) {
    static const unsigned sample[] = {
        0x00C5, 0x00E5, 0x0130, 0x0131, 0x01C5, 0x03A3, 0x03C2, 0x0416, 0x0436, 0x0561,
        0x05D0, 0x0663, 0x0E01, 0x1680, 0x2003, 0x200B, 0x2029, 0x2160, 0x24B6, 0x3000,
        0x30A2, 0xAC00, 0xFF21, 0xFF41, 0xFEFF, 0x10400, 0x10428, 0x1D400, 0x1F4A9, 0xF0000
    };
    unsigned i, c;
    long bad = 0, alpha = 0, upper = 0, punct = 0;
    if (!setlocale(LC_ALL, "C.UTF-8") && !setlocale(LC_ALL, "en_US.UTF-8")) {
        printf("no UTF-8 locale\n");
        return 0;
    }
    for (i = 0; i < sizeof sample / sizeof sample[0]; i++) {
        c = sample[i];
        printf("U+%04X alpha %d upper %d lower %d digit %d space %d blank %d cntrl %d punct %d graph %d print %d upper-> %04X lower-> %04X\n",
               c, !!iswalpha(c), !!iswupper(c), !!iswlower(c), !!iswdigit(c), !!iswspace(c), !!iswblank(c),
               !!iswcntrl(c), !!iswpunct(c), !!iswgraph(c), !!iswprint(c), (unsigned)towupper(c), (unsigned)towlower(c));
    }
    for (c = 0; c < 0x10000; c++) {
        if (c >= 0xD800 && c <= 0xDFFF) continue;
        bad += iswdigit(c) && !(c >= '0' && c <= '9');
        bad += iswxdigit(c) && !iswdigit(c) && !((c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'));
        bad += !!iswgraph(c) != (iswprint(c) && !iswspace(c));
        bad += iswpunct(c) && (iswalnum(c) || iswspace(c));
        bad += iswalpha(c) && (iswdigit(c) || iswspace(c) || iswcntrl(c) || iswpunct(c));
        bad += iswblank(c) && !iswspace(c);
        bad += !!iswalnum(c) != (iswalpha(c) || iswdigit(c));
        alpha += !!iswalpha(c);
        upper += !!iswupper(c);
        punct += !!iswpunct(c);
    }
    printf("relations broken: %ld\n", bad);
    printf("alpha %ld upper %ld punct %ld\n", alpha, upper, punct);
    printf("wctype: %d %d %d, none %d\n", !!iswctype(0x0436, wctype("lower")), !!iswctype(0x0436, wctype("upper")),
           !!iswctype(0x3000, wctype("blank")), wctype("letter") == 0);
    printf("wctrans: %04X %04X, none %d\n", (unsigned)towctrans(0x0436, wctrans("toupper")),
           (unsigned)towctrans(0x0416, wctrans("tolower")), wctrans("upcase") == 0);
    printf("WEOF: alpha %d, upper-> same %d\n", !!iswalpha(WEOF), towupper(WEOF) == WEOF);
    return 0;
}
