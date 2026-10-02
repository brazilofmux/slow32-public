/* strftime's %z and %Z from a struct tm built by hand: offsets with
 * minutes in them, east and west, which the one zone the harness runs
 * in does not have.  (The host is not asked: it need not take %z from
 * tm_gmtoff, and macOS does not.)  Both libraries build strftime from
 * one source, so what this prints is written down beside it. */
#include <stdio.h>
#include <string.h>
#include <time.h>

static void show(long off, const char *zone) {
    struct tm t;
    char buf[64];
    unsigned int n;
    memset(&t, 0, sizeof t);
    t.tm_year = 126; t.tm_mon = 9; t.tm_mday = 2; t.tm_hour = 13; t.tm_min = 5; t.tm_sec = 9;
    t.tm_wday = 5; t.tm_yday = 274;
    t.tm_gmtoff = off;
    t.tm_zone = zone;
    n = (unsigned int)strftime(buf, sizeof buf, "%Y-%m-%d %H:%M:%S %z %Z|", &t);
    printf("%ld -> %u [%s]\n", off, n, buf);
}

int main(void) {
    show(0, "UTC");
    show(3600, "CET");
    show(-21600, "CST");
    show(19800, "IST");          /* +0530 */
    show(-12600, "NST");         /* -0330 */
    show(20700, "+0545");
    show(45900, "+1245");
    show(-34200, "-0930");
    show(59, "odd");             /* less than a minute: +0000 */
    show(-3599, "odd");
    show(86399, "edge");
    show(0, 0);                  /* no zone name: %Z is nothing */
    return 0;
}
