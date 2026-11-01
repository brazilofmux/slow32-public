/* 7.21.6.2 fscanf: an input item that is only a prefix of a matching sequence is a matching failure, and its characters stay consumed */
#include <stdio.h>

int main(void) {
    unsigned x = 7;
    double d = 7;
    char rest[8] = "";
    int r = sscanf("0xg", "%x%7s", &x, rest);
    printf("%d %u [%s]\n", r, x, rest);
    rest[0] = 0;
    r = sscanf("25exp", "%lf%7s", &d, rest);
    printf("%d %g [%s]\n", r, d, rest);
    rest[0] = 0;
    r = sscanf("1.5e+z", "%lf%7s", &d, rest);
    printf("%d %g [%s]\n", r, d, rest);
    return 0;
}
