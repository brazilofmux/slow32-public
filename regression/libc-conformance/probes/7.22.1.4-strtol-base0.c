/* 7.22.1.4 strtol: base 0 takes the base from the prefix: 0x hexadecimal, 0 octal, else decimal */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

static void t(const char *s) {
    char *e;
    long v;
    errno = 0;
    v = strtol(s, &e, 0);
    printf("%-8s %ld used=%d errno0=%d\n", s, v, (int)(e - s), errno == 0);
}
int main(void) {
    t("0x1A"); t("0X1a"); t("012"); t("08"); t("0"); t("10"); t(" -0x10"); t("+077"); t("0b101"); t("-0");
    return 0;
}
