/* 7.21.6.2 fscanf: a white-space directive matches any amount including none; ordinary characters must match; %% skips white space */
#include <stdio.h>

int main(void) {
    int a = 0, b = 0;
    char c = 0, d = 0;
    int r = sscanf("1,2", "%d , %d", &a, &b);
    printf("%d: %d %d\n", r, a, b);
    r = sscanf("1  ,\n\t2", "%d , %d", &a, &b);
    printf("%d: %d %d\n", r, a, b);
    r = sscanf("1 ,2", "%d,%d", &a, &b);
    printf("%d: %d\n", r, a);
    r = sscanf("50 %x", "%d %%%c", &a, &c);
    printf("%d: %d %c\n", r, a, c);
    r = sscanf("7  %", "%d%%", &a);
    printf("%d: %d\n", r, a);
    r = sscanf(" q r", "%c%c", &c, &d);
    printf("%d: [%c][%c]\n", r, c, d);
    r = sscanf(" q r", " %c %c", &c, &d);
    printf("%d: [%c][%c]\n", r, c, d);
    char s[8];
    r = sscanf("one\ttwo", "%s", s);
    printf("%d: %s\n", r, s);
    return 0;
}
