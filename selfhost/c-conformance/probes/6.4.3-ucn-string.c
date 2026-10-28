// 6.4.3p1  \u and \U escapes in a narrow string literal encode as UTF-8
#include <stdio.h>
int main(void) {
    const unsigned char s[] = "A\u00e9\u20ac\U0001F600";
    for (unsigned i = 0; i < sizeof s; i++) printf("%02x", s[i]);
    printf(" %d\n", (int)sizeof s);
    return 0;
}
