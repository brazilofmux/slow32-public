/* 7.22.1.3 strtof: overflow returns HUGE_VALF with the right sign and sets errno to ERANGE */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <math.h>

static void t(const char *s) {
    char *e;
    float f;
    errno = 0;
    f = strtof(s, &e);
    printf("%-14s +huge=%d -huge=%d erange=%d used=%d\n", s, f == HUGE_VALF, f == -HUGE_VALF, errno == ERANGE, (int)(e - s));
}
int main(void) {
    t("3.5e38"); t("-1e39"); t("1e300"); t("0x1p128"); t("3.4028235e38"); t("3.40282357e38");
    return 0;
}
