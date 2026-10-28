// 6.4.4.4p7-p8 in 6.4.5  octal escapes stop after 3 digits; hex escapes take all hex digits
#include <stdio.h>
int main(void) {
    const unsigned char s[] = "\1234\0012\x41g\18";
    for (unsigned i = 0; i < sizeof s; i++) printf("%d ", s[i]);
    printf("| %d\n", (int)sizeof s);
    return 0;
}
