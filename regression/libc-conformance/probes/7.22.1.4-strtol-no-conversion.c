/* 7.22.1.4 strtol family: no subject sequence gives 0 and stores nptr in *endptr */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

static void t(const char *s) {
    char *e1, *e2, *e3, *e4;
    long a; unsigned long b; long long c; unsigned long long d;
    a = strtol(s, &e1, 10); b = strtoul(s, &e2, 0); c = strtoll(s, &e3, 16); d = strtoull(s, &e4, 36);
    printf("[%s] %d %d\n", s, a == 0 && b == 0 && c == 0 && d == 0, e1 == s && e2 == s && e3 == s && e4 == s);
}
int main(void) {
    t(""); t("   "); t("-"); t("+ 1"); t(" -+2"); t("@");
    printf("null endptr: %ld\n", strtol("42", NULL, 10));
    return 0;
}
