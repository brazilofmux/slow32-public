/* 7.27.3.5p7 strftime: in the "C" locale the E and O modifiers are ignored -- each modified form prints as the plain one */
#include <stdio.h>
#include <string.h>
#include <time.h>

int main(void) {
    static const char *const mod[] = {"Ec", "EC", "Ex", "EX", "Ey", "EY", "Od", "Oe", "OH", "OI", "Om", "OM",
                                      "OS", "Ou", "OU", "OV", "Ow", "OW", "Oy"};
    struct tm t;
    char a[100], b[100], fa[8], fb[8];
    int i;
    memset(&t, 0, sizeof t);
    t.tm_year = 126; t.tm_mon = 10; t.tm_mday = 3; t.tm_hour = 21; t.tm_min = 4; t.tm_sec = 5;
    t.tm_wday = 2; t.tm_yday = 306;
    for (i = 0; i < (int)(sizeof mod / sizeof mod[0]); i++) {
        sprintf(fa, "%%%s", mod[i]);
        sprintf(fb, "%%%c", mod[i][1]);
        strftime(a, sizeof a, fa, &t);
        strftime(b, sizeof b, fb, &t);
        printf("%-4s [%s] %s\n", fa, a, strcmp(a, b) == 0 ? "same" : "DIFFERENT");
    }
    return 0;
}
