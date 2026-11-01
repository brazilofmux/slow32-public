/* 7.21.6.2 fscanf: %n stores the characters consumed and is not counted; * suppresses assignment and the count */
#include <stdio.h>

int main(void) {
    int a = 0, b = 0, n1 = -1, n2 = -1;
    int r = sscanf("  10 20 30", "%d%n %*d %d%n", &a, &n1, &b, &n2);
    printf("%d: %d %d %d %d\n", r, a, b, n1, n2);
    r = sscanf("abc", "%*s%n", &n1);
    printf("%d: %d\n", r, n1);
    n1 = -1;
    r = sscanf("", "%n", &n1);
    printf("%d: %d\n", r, n1);
    n1 = -1;
    r = sscanf("xy", "x%n", &n1);
    printf("%d: %d\n", r, n1);
    signed char hn = -1;
    long long lln = -1;
    r = sscanf("hello world", "%*s%hhn %*s%lln", &hn, &lln);
    printf("%d: %d %lld\n", r, hn, lln);
    r = sscanf("1 2 3", "%*d %*d %*d");
    printf("%d\n", r);
    return 0;
}
