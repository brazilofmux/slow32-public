/* 7.21.6.1 fprintf: * width and precision; a negative width means the - flag, a negative precision means none */
#include <stdio.h>

int main(void) {
    printf("[%*d][%-*d][%*d]\n", 5, 42, 5, 42, -5, 42);
    printf("[%.*d][%.*d][%*.*d]\n", 4, 7, -1, 7, 6, 3, 7);
    printf("[%.*s][%.*s][%*s]\n", 2, "abcdef", -3, "abcdef", -4, "x");
    printf("[%.*f][%.*f][%*.*e]\n", 1, 2.25, -2, 1.5, 12, 2, 1234.5);
    printf("[%0*d][%-0*d][%*c]\n", 6, -3, 6, -3, 3, 'z');
    return 0;
}
