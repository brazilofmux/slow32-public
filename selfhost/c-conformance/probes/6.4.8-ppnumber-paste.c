// 6.4.8p1  pp-numbers include things like 1.2.3 and 12e+ ; ## builds a valid number; stringize keeps the spelling
#include <stdio.h>
#define STR(x) #x
#define XSTR(x) STR(x)
#define CAT(a, b) a ## b
int main(void) {
    double d = CAT(1, e3) + CAT(0x, 10);
    printf("%s %s %.0f %s\n", STR(1.2.3), STR(0x1e+1), d, STR(12e+_x));
    return 0;
}
