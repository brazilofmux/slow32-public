// 6.10.8.1p1  __DATE__ is "Mmm dd yyyy" and __TIME__ "hh:mm:ss"
#include <stdio.h>
#include <string.h>
static int dig(char c) { return c >= '0' && c <= '9'; }
int main(void) {
    const char d[] = __DATE__;
    const char t[] = __TIME__;
    const char *months = "JanFebMarAprMayJunJulAugSepOctNovDec";
    int ok = sizeof d == 12 && sizeof t == 9;
    int m, found = 0;
    for (m = 0; m < 12; m++) if (memcmp(d, months + 3 * m, 3) == 0) found = 1;
    ok = ok && found && d[3] == ' ' && (d[4] == ' ' || dig(d[4])) && dig(d[5]) && d[6] == ' '
            && dig(d[7]) && dig(d[8]) && dig(d[9]) && dig(d[10]);
    ok = ok && dig(t[0]) && dig(t[1]) && t[2] == ':' && dig(t[3]) && dig(t[4]) && t[5] == ':'
            && dig(t[6]) && dig(t[7]);
    puts(ok ? "ok" : "bad format");
    return 0;
}
