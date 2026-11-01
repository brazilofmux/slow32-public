/* 7.30.3 towlower/towupper and wctrans/towctrans in the "C" locale */
#include <stdio.h>
#include <wctype.h>

int main(void) {
    printf("%c %c %c %c\n", (int)towupper(L'a'), (int)towlower(L'Q'), (int)towupper(L'5'), (int)towlower(L'z'));
    printf("%c %c\n", (int)towctrans(L'm', wctrans("toupper")), (int)towctrans(L'M', wctrans("tolower")));
    printf("wctrans unknown is 0: %d\n", wctrans("sideways") == 0);
    printf("towupper(WEOF) is WEOF: %d\n", towupper(WEOF) == WEOF);
    return 0;
}
