/* 7.22.1.3 strtod: with no valid subject sequence it returns zero and stores nptr in *endptr */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

static void t(const char *s) {
    char *e = (char *)"x";
    double d = strtod(s, &e);
    printf("[%s] zero=%d at-start=%d\n", s, d == 0.0, e == s);
}
int main(void) {
    t(""); t("   "); t("+"); t("-"); t("."); t("-.e1"); t("e5"); t("abc"); t(" +x1"); t("0x");
    return 0;
}
