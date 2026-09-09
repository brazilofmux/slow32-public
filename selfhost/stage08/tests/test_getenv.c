/* GitHub issue 55 / selfhost ISSUES-68: getenv over the MMIO GETENV
 * request.  It used to be a stub returning NULL, so SQLite's shell warned
 * that it could not find a home directory on every run.  The harness sets
 * S32_SELFTEST_ENV=ok before running this; the other three cases are
 * environment-independent. */
#include <stdlib.h>

static int streq(const char *a, const char *b) {
    while (*a != 0 && *a == *b) { a = a + 1; b = b + 1; }
    return *a == *b;
}

int main(void) {
    char *v;
    v = getenv("S32_SELFTEST_ENV");
    if (v == 0) return 1;                                  /* set, must be found */
    if (!streq(v, "ok")) return 2;                          /* and be the value */
    if (getenv("S32_SELFTEST_UNSET_XYZZY") != 0) return 3;  /* unset -> NULL */
    if (getenv("HAS=EQUALS") != 0) return 4;                /* C11 7.22.4.6 */
    if (getenv("") != 0) return 5;                          /* empty -> NULL */
    return 0;
}
