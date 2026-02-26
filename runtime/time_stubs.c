#include <time.h>
#include <stddef.h>
#include <string.h>

/* clock() - return processor time used (stub: always 0) */
clock_t clock(void) {
    return (clock_t)0;
}

/* strftime - format time into string */
static const char *const wday_abbr[] = {
    "Sun","Mon","Tue","Wed","Thu","Fri","Sat"
};
static const char *const wday_full[] = {
    "Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"
};
static const char *const mon_abbr[] = {
    "Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"
};
static const char *const mon_full[] = {
    "January","February","March","April","May","June",
    "July","August","September","October","November","December"
};

static int fmt_2d(char *buf, int val) {
    buf[0] = '0' + val / 10;
    buf[1] = '0' + val % 10;
    return 2;
}

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

        char buf[32];
        const char *src = buf;
        int len = 0;

        switch (*p) {
        case 'A': /* full weekday name */
            src = wday_full[tm->tm_wday % 7];
            len = strlen(src);
            break;
        case 'a': /* abbreviated weekday name */
            src = wday_abbr[tm->tm_wday % 7];
            len = 3;
            break;
        case 'B': /* full month name */
            src = mon_full[tm->tm_mon % 12];
            len = strlen(src);
            break;
        case 'b': /* abbreviated month name */
        case 'h': /* %h is equivalent to %b */
            src = mon_abbr[tm->tm_mon % 12];
            len = 3;
            break;
        case 'C': /* century 00-99 */
            len = fmt_2d(buf, (tm->tm_year + 1900) / 100);
            break;
        case 'c': /* date and time: "Sun Feb 26 14:30:00 2026" */
            { const char *wd = wday_abbr[tm->tm_wday % 7];
              const char *mn = mon_abbr[tm->tm_mon % 12];
              int i = 0;
              buf[i++] = wd[0]; buf[i++] = wd[1]; buf[i++] = wd[2]; buf[i++] = ' ';
              buf[i++] = mn[0]; buf[i++] = mn[1]; buf[i++] = mn[2]; buf[i++] = ' ';
              buf[i++] = '0' + tm->tm_mday / 10;
              buf[i++] = '0' + tm->tm_mday % 10;
              buf[i++] = ' ';
              i += fmt_2d(buf + i, tm->tm_hour); buf[i++] = ':';
              i += fmt_2d(buf + i, tm->tm_min); buf[i++] = ':';
              i += fmt_2d(buf + i, tm->tm_sec); buf[i++] = ' ';
              { int y = tm->tm_year + 1900;
                buf[i+3] = '0' + y % 10; y /= 10;
                buf[i+2] = '0' + y % 10; y /= 10;
                buf[i+1] = '0' + y % 10; y /= 10;
                buf[i+0] = '0' + y % 10;
                i += 4; }
              len = i; }
            break;
        case 'D': /* %m/%d/%y */
            { int i = 0;
              i += fmt_2d(buf + i, tm->tm_mon + 1); buf[i++] = '/';
              i += fmt_2d(buf + i, tm->tm_mday); buf[i++] = '/';
              i += fmt_2d(buf + i, (tm->tm_year + 1900) % 100);
              len = i; }
            break;
        case 'd': /* day 01-31 */
            len = fmt_2d(buf, tm->tm_mday);
            break;
        case 'e': /* day, space-padded " 1"-"31" */
            buf[0] = tm->tm_mday < 10 ? ' ' : '0' + tm->tm_mday / 10;
            buf[1] = '0' + tm->tm_mday % 10;
            len = 2;
            break;
        case 'F': /* %Y-%m-%d */
            { int i = 0;
              int y = tm->tm_year + 1900;
              buf[i+3] = '0' + y % 10; y /= 10;
              buf[i+2] = '0' + y % 10; y /= 10;
              buf[i+1] = '0' + y % 10; y /= 10;
              buf[i+0] = '0' + y % 10;
              i += 4; buf[i++] = '-';
              i += fmt_2d(buf + i, tm->tm_mon + 1); buf[i++] = '-';
              i += fmt_2d(buf + i, tm->tm_mday);
              len = i; }
            break;
        case 'H': /* hour 00-23 */
            len = fmt_2d(buf, tm->tm_hour);
            break;
        case 'I': /* hour 01-12 */
            { int h = tm->tm_hour % 12;
              if (h == 0) h = 12;
              len = fmt_2d(buf, h); }
            break;
        case 'j': /* day of year 001-366 */
            { int d = tm->tm_yday + 1;
              buf[0] = '0' + d / 100;
              buf[1] = '0' + (d / 10) % 10;
              buf[2] = '0' + d % 10;
              len = 3; }
            break;
        case 'M': /* minute 00-59 */
            len = fmt_2d(buf, tm->tm_min);
            break;
        case 'm': /* month 01-12 */
            len = fmt_2d(buf, tm->tm_mon + 1);
            break;
        case 'n': /* newline */
            buf[0] = '\n';
            len = 1;
            break;
        case 'p': /* AM/PM */
            src = tm->tm_hour < 12 ? "AM" : "PM";
            len = 2;
            break;
        case 'R': /* %H:%M */
            { int i = 0;
              i += fmt_2d(buf + i, tm->tm_hour); buf[i++] = ':';
              i += fmt_2d(buf + i, tm->tm_min);
              len = i; }
            break;
        case 'S': /* second 00-60 */
            len = fmt_2d(buf, tm->tm_sec);
            break;
        case 'T': /* %H:%M:%S */
            { int i = 0;
              i += fmt_2d(buf + i, tm->tm_hour); buf[i++] = ':';
              i += fmt_2d(buf + i, tm->tm_min); buf[i++] = ':';
              i += fmt_2d(buf + i, tm->tm_sec);
              len = i; }
            break;
        case 't': /* tab */
            buf[0] = '\t';
            len = 1;
            break;
        case 'u': /* weekday 1-7 (Monday=1) */
            buf[0] = '0' + (tm->tm_wday == 0 ? 7 : tm->tm_wday);
            len = 1;
            break;
        case 'w': /* weekday 0-6 (Sunday=0) */
            buf[0] = '0' + tm->tm_wday;
            len = 1;
            break;
        case 'X': /* time: "14:30:00" */
            { int i = 0;
              i += fmt_2d(buf + i, tm->tm_hour); buf[i++] = ':';
              i += fmt_2d(buf + i, tm->tm_min); buf[i++] = ':';
              i += fmt_2d(buf + i, tm->tm_sec);
              len = i; }
            break;
        case 'x': /* date: "02/26/26" */
            { int i = 0;
              i += fmt_2d(buf + i, tm->tm_mon + 1); buf[i++] = '/';
              i += fmt_2d(buf + i, tm->tm_mday); buf[i++] = '/';
              i += fmt_2d(buf + i, (tm->tm_year + 1900) % 100);
              len = i; }
            break;
        case 'Y': /* 4-digit year */
            { int y = tm->tm_year + 1900;
              buf[3] = '0' + y % 10; y /= 10;
              buf[2] = '0' + y % 10; y /= 10;
              buf[1] = '0' + y % 10; y /= 10;
              buf[0] = '0' + y % 10;
              len = 4; }
            break;
        case 'y': /* 2-digit year */
            len = fmt_2d(buf, (tm->tm_year + 1900) % 100);
            break;
        case 'Z': /* timezone name (not available) */
            src = "UTC";
            len = 3;
            break;
        case '%': /* literal % */
            buf[0] = '%';
            len = 1;
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
