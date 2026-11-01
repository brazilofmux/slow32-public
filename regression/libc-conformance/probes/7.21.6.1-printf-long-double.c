/* 7.21.6.1 fprintf: the L modifier with f e g, for values exactly representable in a double */
#include <stdio.h>

int main(void) {
    long double x = 31.25L, y = -0.5L;
    printf("[%Lf][%Le][%Lg][%LE][%LG][%.1Lf] %d\n", x, x, x, y, 1e20L, y, 7);
    char b[64];
    int r = snprintf(b, sizeof b, "%10.3Lf|%-10.2Le|", x, x);
    printf("%d [%s]\n", r, b);
    return 0;
}
