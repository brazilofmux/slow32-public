/* 7.22.1.4 strtol: "0x" with no hexadecimal digit after it converts the 0 and leaves endptr at the x */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

static void t(const char *s, int base) {
    char *e;
    long v = strtol(s, &e, base);
    unsigned long u = strtoul(s, &e, base);
    printf("%-6s base %2d -> %ld %lu rest=\"%s\"\n", s, base, v, u, e);
}
int main(void) {
    t("0x", 16); t("0x", 0); t("0xg", 16); t("0xg", 0); t(" -0x", 16); t("0X ", 0); t("0x1g", 16);
    return 0;
}
