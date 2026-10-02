/* time_std.c -- mktime, asctime, ctime, difftime, strftime.
 *
 * One source for both C libraries: the clang runtime builds it into
 * libc_mmio/libc_debug, and the self-hosted library compiles it with
 * stage08 cc (selfhost/stage08/build-s12cc.sh), each against its own
 * <time.h>.  So it assumes nothing about time_t but that it is an
 * integer -- 64 bits unsigned on one side, 32 signed on the other --
 * and does its own arithmetic in long long.
 *
 * What it needs from the library it is built into: localtime,
 * localtime_r, and __s32_query_tz (the host's zone rules for an
 * instant, through the emulator; "no zone service" is UTC).
 *
 * regression/libc-tests/time_conv.c holds both builds to the host's C
 * library, conversion by conversion.
 */
#include <time.h>
#include <string.h>

static int is_leap(long long y) {
    return (y % 4 == 0 && y % 100 != 0) || y % 400 == 0;
}

/* the leap years before year y, counted from year 1 */
static long long leaps_before(long long y) {
    y = y - 1;
    return y / 4 - y / 100 + y / 400;
}

/* days from 1970-01-01 to the first day of month m (0..11) of year y */
static long long days_to_month(long long y, int m) {
    static const short before[12] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};
    long long days = (y - 1970) * 365 + leaps_before(y) - leaps_before(1970) + before[m];
    if (m >= 2 && is_leap(y)) days++;
    return days;
}

/* the zone's offset from UTC, in seconds east, at an instant */
static long zone_at(long long utc, int *isdst) {
    long off = 0;
    char abbrev[8];
    *isdst = 0;
    if ((long long)(time_t)utc != utc) return 0;
    if (__s32_query_tz((time_t)utc, &off, isdst, abbrev) != 0) {
        *isdst = 0;
        return 0;
    }
    return off;
}

/* The instant a broken-down local time names.  The fields need not be
 * in range: month 14 is February of the next year, day 0 the last day
 * of the month before, second -1 the minute before.  They come back in
 * range, with the day of the week and of the year filled in.
 *
 * Local time is UTC plus an offset that depends on the instant, which
 * is the unknown; two guesses settle it everywhere but in the hour a
 * zone repeats when its clocks go back.  There the caller's tm_isdst
 * says which of the two it means (negative: the first). */
time_t mktime(struct tm *tm) {
    long long year, local, t, c;
    long off;
    int mon, dst, cdst, k;
    time_t result;

    year = (long long)tm->tm_year + 1900 + tm->tm_mon / 12;
    mon = tm->tm_mon % 12;
    if (mon < 0) {
        mon += 12;
        year--;
    }
    local = (days_to_month(year, mon) + tm->tm_mday - 1) * 86400
          + (long long)tm->tm_hour * 3600 + (long long)tm->tm_min * 60 + tm->tm_sec;

    off = zone_at(local, &dst);
    t = local - off;
    if (t + zone_at(t, &dst) != local) {
        c = local - zone_at(t, &cdst);
        if (c + zone_at(c, &cdst) == local) {
            t = c;
            dst = cdst;
        }
        /* else no instant has this local time (the clocks skipped it): t stands */
    }
    if (tm->tm_isdst >= 0 && (dst != 0) != (tm->tm_isdst != 0)) {
        for (k = -1; k <= 1; k += 2) {
            c = t + k * 3600;
            if (c + zone_at(c, &cdst) == local && (cdst != 0) == (tm->tm_isdst != 0)) {
                t = c;
                break;
            }
        }
    }

    result = (time_t)t;
    if ((long long)result != t) return (time_t)-1;
    localtime_r(&result, tm);
    return result;
}

static const char wday_name[7][10] = {
    "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"
};
static const char mon_name[12][10] = {
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
};

static int in_range(int v, int n) {
    v = v % n;
    return v < 0 ? v + n : v;
}

/* ---- strftime ------------------------------------------------------------ */

struct sf_out {
    char *s;
    size_t max;
    size_t len;         /* what the whole result needs, whether it fits or not */
};

static void sf_ch(struct sf_out *o, int c) {
    if (o->len < o->max) o->s[o->len] = (char)c;
    o->len++;
}

static void sf_str(struct sf_out *o, const char *z, int n) {
    while (*z && n != 0) {
        sf_ch(o, *z++);
        n--;
    }
}

/* v in at least `width` places, filled with `pad` */
static void sf_num(struct sf_out *o, long long v, int width, int pad) {
    char digits[24];
    int n = 0, neg = 0;
    if (v < 0) {
        neg = 1;
        v = -v;
    }
    do {
        digits[n++] = (char)('0' + v % 10);
        v /= 10;
    } while (v > 0);
    if (neg) {
        sf_ch(o, '-');
        width--;
    }
    while (width > n) {
        sf_ch(o, pad);
        width--;
    }
    while (n > 0) sf_ch(o, digits[--n]);
}

/* the weekday of January 1 of the year tm is in, Monday 0 .. Sunday 6 */
static int jan1_monday0(const struct tm *tm) {
    return in_range(tm->tm_wday + 6 - tm->tm_yday, 7);
}

/* ISO 8601 has 53 weeks in a year that begins on a Thursday, and in a
 * leap year that begins on a Wednesday; 52 in every other. */
static int iso_weeks_in(long long year, int jan1) {
    return (jan1 == 3 || (jan1 == 2 && is_leap(year))) ? 53 : 52;
}

/* ISO 8601 week number, and the year that week belongs to: the week is
 * Monday to Sunday and belongs to the year its Thursday is in. */
static int iso_week(const struct tm *tm, long long *iso_year) {
    long long year = (long long)tm->tm_year + 1900;
    int wd = in_range(tm->tm_wday + 6, 7);
    int week = (tm->tm_yday - wd + 10) / 7;
    int jan1 = jan1_monday0(tm);
    if (week < 1) {
        /* the last week of the year before, whose own January 1 is... */
        int prev_days = is_leap(year - 1) ? 366 : 365;
        year--;
        week = iso_weeks_in(year, in_range(jan1 - prev_days, 7));
    } else if (week > iso_weeks_in(year, jan1)) {
        week = 1;
        year++;
    }
    *iso_year = year;
    return week;
}

static void sf_conv(struct sf_out *o, int c, const struct tm *tm);

static void sf_format(struct sf_out *o, const char *f, const struct tm *tm) {
    while (*f) {
        if (*f != '%') {
            sf_ch(o, *f++);
            continue;
        }
        f++;
        if (*f == 'E' || *f == 'O') f++;        /* the alternative forms are the plain ones in the C locale */
        if (!*f) break;
        sf_conv(o, *f++, tm);
    }
}

static void sf_conv(struct sf_out *o, int c, const struct tm *tm) {
    long long year = (long long)tm->tm_year + 1900;
    long long iso_year;
    int h;
    long off;

    switch (c) {
    case 'a': sf_str(o, wday_name[in_range(tm->tm_wday, 7)], 3); break;
    case 'A': sf_str(o, wday_name[in_range(tm->tm_wday, 7)], -1); break;
    case 'b':
    case 'h': sf_str(o, mon_name[in_range(tm->tm_mon, 12)], 3); break;
    case 'B': sf_str(o, mon_name[in_range(tm->tm_mon, 12)], -1); break;
    case 'c': sf_format(o, "%a %b %e %H:%M:%S %Y", tm); break;
    case 'C': sf_num(o, (year - in_range((int)(year % 100), 100)) / 100, 2, '0'); break;
    case 'd': sf_num(o, tm->tm_mday, 2, '0'); break;
    case 'D': sf_format(o, "%m/%d/%y", tm); break;
    case 'e': sf_num(o, tm->tm_mday, 2, ' '); break;
    case 'F': sf_format(o, "%Y-%m-%d", tm); break;
    case 'g': iso_week(tm, &iso_year); sf_num(o, in_range((int)(iso_year % 100), 100), 2, '0'); break;
    case 'G': iso_week(tm, &iso_year); sf_num(o, iso_year, 1, '0'); break;
    case 'H': sf_num(o, tm->tm_hour, 2, '0'); break;
    case 'I': h = tm->tm_hour % 12; sf_num(o, h == 0 ? 12 : h, 2, '0'); break;
    case 'j': sf_num(o, tm->tm_yday + 1, 3, '0'); break;
    case 'm': sf_num(o, tm->tm_mon + 1, 2, '0'); break;
    case 'M': sf_num(o, tm->tm_min, 2, '0'); break;
    case 'n': sf_ch(o, '\n'); break;
    case 'p': sf_str(o, tm->tm_hour < 12 ? "AM" : "PM", -1); break;
    case 'r': sf_format(o, "%I:%M:%S %p", tm); break;
    case 'R': sf_format(o, "%H:%M", tm); break;
    case 'S': sf_num(o, tm->tm_sec, 2, '0'); break;
    case 't': sf_ch(o, '\t'); break;
    case 'T': sf_format(o, "%H:%M:%S", tm); break;
    case 'u': sf_num(o, tm->tm_wday == 0 ? 7 : tm->tm_wday, 1, '0'); break;
    case 'U': sf_num(o, (tm->tm_yday + 7 - tm->tm_wday) / 7, 2, '0'); break;                   /* weeks begin on Sunday */
    case 'V': sf_num(o, iso_week(tm, &iso_year), 2, '0'); break;
    case 'w': sf_num(o, tm->tm_wday, 1, '0'); break;
    case 'W': sf_num(o, (tm->tm_yday + 7 - in_range(tm->tm_wday + 6, 7)) / 7, 2, '0'); break;  /* on Monday */
    case 'x': sf_format(o, "%m/%d/%y", tm); break;
    case 'X': sf_format(o, "%H:%M:%S", tm); break;
    case 'y': sf_num(o, in_range((int)(year % 100), 100), 2, '0'); break;
    case 'Y': sf_num(o, year, 1, '0'); break;
    case 'z':
        off = tm->tm_gmtoff;
        sf_ch(o, off < 0 ? '-' : '+');
        if (off < 0) off = -off;
        sf_num(o, off / 3600, 2, '0');
        sf_num(o, off % 3600 / 60, 2, '0');
        break;
    case 'Z': if (tm->tm_zone) sf_str(o, tm->tm_zone, -1); break;
    case '%': sf_ch(o, '%'); break;
    default:                    /* not a conversion: it stands as written */
        sf_ch(o, '%');
        sf_ch(o, c);
        break;
    }
}

/* The formatted time, when it and its terminator fit in max bytes --
 * the count without the terminator; otherwise 0, and the array's
 * contents are not to be relied on. */
size_t strftime(char *s, size_t max, const char *format, const struct tm *tm) {
    struct sf_out o;
    o.s = s;
    o.max = max;
    o.len = 0;
    sf_format(&o, format, tm);
    if (o.len >= max) return 0;
    s[o.len] = 0;
    return o.len;
}

/* "Thu Jan  1 00:00:00 1970\n", in an array of the library's */
char *asctime(const struct tm *tm) {
    static char text[40];
    struct sf_out o;
    o.s = text;
    o.max = sizeof text - 1;
    o.len = 0;
    sf_str(&o, wday_name[in_range(tm->tm_wday, 7)], 3);
    sf_ch(&o, ' ');
    sf_str(&o, mon_name[in_range(tm->tm_mon, 12)], 3);
    sf_ch(&o, ' ');
    sf_num(&o, tm->tm_mday, 2, ' ');
    sf_ch(&o, ' ');
    sf_num(&o, tm->tm_hour, 2, '0');
    sf_ch(&o, ':');
    sf_num(&o, tm->tm_min, 2, '0');
    sf_ch(&o, ':');
    sf_num(&o, tm->tm_sec, 2, '0');
    sf_ch(&o, ' ');
    sf_num(&o, (long long)tm->tm_year + 1900, 1, '0');
    sf_ch(&o, '\n');
    text[o.len < o.max ? o.len : o.max] = 0;
    return text;
}

char *ctime(const time_t *timer) {
    return asctime(localtime(timer));
}

double difftime(time_t later, time_t earlier) {
    return (double)((long long)later - (long long)earlier);
}
