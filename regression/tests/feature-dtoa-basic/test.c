#include <stdio.h>

char *dtoa(double d, int mode, int ndigits, int *decpt, int *sign, char **rve);
void freedtoa(char *s);

static void dump(const char *tag, double val, int mode, int ndigits) {
    int decpt = 0;
    int sign = 0;
    char *rve = 0;
    char *s = dtoa(val, mode, ndigits, &decpt, &sign, &rve);
    if (!s) {
        printf("%s: NULL\n", tag);
        return;
    }
    int len = (int)(rve ? (rve - s) : 0);
    printf("%s: s=%s decpt=%d sign=%d len=%d\n", tag, s, decpt, sign, len);
    freedtoa(s);
}

int main() {
    dump("a", 1.25, 2, 7);
    dump("b", 1.5, 3, 6);
    dump("c", 12345.0, 2, 6);
    return 0;
}
