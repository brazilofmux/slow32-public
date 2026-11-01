/* 7.21.6.1 fprintf: %c and %s with width, precision and the - flag; a precision limits the characters read */
#include <stdio.h>

int main(void) {
    char nonterm[3] = { 'x', 'y', 'z' };
    printf("[%c][%3c][%-3c][%c]\n", 'a', 'b', 'c', 256 + 'd');
    printf("[%s][%8s][%-8s][%.2s][%.0s][%8.3s][%-8.3s]\n", "hello", "hello", "hello", "hello", "hello", "hello", "hello");
    printf("[%.3s][%.2s][%s][%3s]\n", nonterm, nonterm, "", "");
    printf("[%.10s]\n", "short");
    return 0;
}
