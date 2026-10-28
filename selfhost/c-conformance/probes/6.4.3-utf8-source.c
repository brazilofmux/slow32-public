// 6.4.3p1 (5.2.1)  extended source characters written raw (UTF-8) in a string literal pass through
#include <stdio.h>
int main(void) {
    const unsigned char s[] = "Aé€";
    for (unsigned i = 0; i < sizeof s; i++) printf("%02x", s[i]);
    printf(" %d\n", (int)sizeof s);
    return 0;
}
