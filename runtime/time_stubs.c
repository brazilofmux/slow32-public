#include <time.h>
#include <stddef.h>
#include <string.h>

/* clock() - return processor time used (stub: always 0) */
clock_t clock(void) {
    return (clock_t)0;
}

/* strftime - format time into string */
/* Minimal implementation supporting common format specifiers */
size_t strftime(char *s, size_t maxsize, const char *format, const struct tm *tm) {
    if (maxsize == 0) return 0;

    size_t pos = 0;
    const char *p = format;

    while (*p && pos < maxsize - 1) {
        if (*p != '%') {
            s[pos++] = *p++;
            continue;
        }
        p++; /* skip '%' */

        char buf[16];
        const char *src = buf;
        int len = 0;

        switch (*p) {
        case 'Y': /* 4-digit year */
            len = 0;
            { int y = tm->tm_year + 1900;
              buf[3] = '0' + y % 10; y /= 10;
              buf[2] = '0' + y % 10; y /= 10;
              buf[1] = '0' + y % 10; y /= 10;
              buf[0] = '0' + y % 10;
              len = 4; }
            break;
        case 'm': /* month 01-12 */
            buf[0] = '0' + (tm->tm_mon + 1) / 10;
            buf[1] = '0' + (tm->tm_mon + 1) % 10;
            len = 2;
            break;
        case 'd': /* day 01-31 */
            buf[0] = '0' + tm->tm_mday / 10;
            buf[1] = '0' + tm->tm_mday % 10;
            len = 2;
            break;
        case 'H': /* hour 00-23 */
            buf[0] = '0' + tm->tm_hour / 10;
            buf[1] = '0' + tm->tm_hour % 10;
            len = 2;
            break;
        case 'M': /* minute 00-59 */
            buf[0] = '0' + tm->tm_min / 10;
            buf[1] = '0' + tm->tm_min % 10;
            len = 2;
            break;
        case 'S': /* second 00-60 */
            buf[0] = '0' + tm->tm_sec / 10;
            buf[1] = '0' + tm->tm_sec % 10;
            len = 2;
            break;
        case '%': /* literal % */
            buf[0] = '%';
            len = 1;
            break;
        case 'j': /* day of year 001-366 */
            { int d = tm->tm_yday + 1;
              buf[0] = '0' + d / 100;
              buf[1] = '0' + (d / 10) % 10;
              buf[2] = '0' + d % 10;
              len = 3; }
            break;
        case 'w': /* weekday 0-6 */
            buf[0] = '0' + tm->tm_wday;
            len = 1;
            break;
        case 'y': /* 2-digit year */
            { int y2 = (tm->tm_year + 1900) % 100;
              buf[0] = '0' + y2 / 10;
              buf[1] = '0' + y2 % 10;
              len = 2; }
            break;
        case '\0':
            /* trailing % at end of format string */
            goto done;
        default:
            /* unsupported specifier - output as-is */
            buf[0] = '%';
            buf[1] = *p;
            len = 2;
            break;
        }
        p++;

        /* copy formatted field into output */
        for (int i = 0; i < len && pos < maxsize - 1; i++) {
            s[pos++] = src[i];
        }
    }
done:
    s[pos] = '\0';
    return pos;
}
