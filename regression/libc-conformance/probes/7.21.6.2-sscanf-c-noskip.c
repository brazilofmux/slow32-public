/* 7.21.6.2 fscanf: %c and %[ do not skip leading white space, other conversions do; %s stops at white space */
#include <stdio.h>

int main(void) {
    char c = 0, s[16] = "", t[16] = "";
    int a = 0;
    int r = sscanf("5 x", "%d%c", &a, &c);
    printf("%d: %d [%c]\n", r, a, c);
    r = sscanf("  word rest", "%[ dorw]", s);
    printf("%d: [%s]\n", r, s);
    r = sscanf("\n\tab\vcd", "%s%s", s, t);
    printf("%d: [%s][%s]\n", r, s, t);
    return 0;
}
