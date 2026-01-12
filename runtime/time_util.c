// Time utility functions for SLOW-32
// Provides gmtime, localtime, mktime, asctime, ctime
// Note: SLOW-32 does not support timezones, localtime == gmtime

#include "time.h"

// Days in each month (non-leap year)
static const int days_in_month[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

// Cumulative days before each month (non-leap year)
static const int days_before_month[12] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};

// Check if a year is a leap year
static int is_leap_year(int year) {
    return (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0);
}

// Days in a year
static int days_in_year(int year) {
    return is_leap_year(year) ? 366 : 365;
}

// Static buffer for gmtime/localtime result (not thread-safe)
static struct tm tm_result;

// Static buffer for asctime/ctime result
static char time_string[26];

// Convert time_t to struct tm (UTC)
struct tm *gmtime(const time_t *timer) {
    if (timer == 0) return 0;

    time_t t = *timer;

    // Calculate seconds, minutes, hours of the day
    tm_result.tm_sec = (int)(t % 60);
    t /= 60;
    tm_result.tm_min = (int)(t % 60);
    t /= 60;
    tm_result.tm_hour = (int)(t % 24);
    t /= 24;

    // Now t is days since epoch (Jan 1, 1970)
    // Calculate day of week (Jan 1, 1970 was Thursday = 4)
    tm_result.tm_wday = (int)((t + 4) % 7);

    // Calculate year
    int year = 1970;
    while (t >= (time_t)days_in_year(year)) {
        t -= days_in_year(year);
        year++;
    }
    tm_result.tm_year = year - 1900;
    tm_result.tm_yday = (int)t;

    // Calculate month and day
    int leap = is_leap_year(year);
    int day = (int)t;
    int month;
    for (month = 0; month < 11; month++) {
        int dim = days_in_month[month];
        if (month == 1 && leap) dim++;  // February in leap year
        if (day < dim) break;
        day -= dim;
    }
    tm_result.tm_mon = month;
    tm_result.tm_mday = day + 1;  // Days are 1-based

    tm_result.tm_isdst = 0;  // No DST support

    return &tm_result;
}

// localtime - same as gmtime (no timezone support)
struct tm *localtime(const time_t *timer) {
    return gmtime(timer);
}

// Convert struct tm to time_t
time_t mktime(struct tm *tm) {
    if (tm == 0) return (time_t)-1;

    int year = tm->tm_year + 1900;
    int mon = tm->tm_mon;
    int mday = tm->tm_mday;

    // Normalize month
    while (mon < 0) {
        mon += 12;
        year--;
    }
    while (mon >= 12) {
        mon -= 12;
        year++;
    }

    // Calculate days since epoch
    time_t days = 0;

    // Days from years
    for (int y = 1970; y < year; y++) {
        days += days_in_year(y);
    }
    for (int y = year; y < 1970; y++) {
        days -= days_in_year(y);
    }

    // Days from months
    days += days_before_month[mon];
    if (mon > 1 && is_leap_year(year)) {
        days++;  // Add leap day
    }

    // Days in current month (1-based to 0-based)
    days += (mday - 1);

    // Calculate yday and wday
    tm->tm_yday = (int)(days_before_month[mon] + (mon > 1 && is_leap_year(year) ? 1 : 0) + mday - 1);
    tm->tm_wday = (int)((days + 4) % 7);  // Thursday = 4
    tm->tm_isdst = 0;

    // Convert to seconds
    time_t result = days * 86400 + tm->tm_hour * 3600 + tm->tm_min * 60 + tm->tm_sec;

    return result;
}

// Helper to format a number with leading zero
static void format_2digit(char *buf, int val) {
    buf[0] = '0' + (val / 10);
    buf[1] = '0' + (val % 10);
}

// Format time as "Www Mmm dd hh:mm:ss yyyy\n"
char *asctime(const struct tm *tm) {
    static const char *wday_name[7] = {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};
    static const char *mon_name[12] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

    if (tm == 0) return 0;

    // Format: "Www Mmm dd hh:mm:ss yyyy\n\0"
    char *p = time_string;

    // Day of week
    const char *wd = wday_name[tm->tm_wday % 7];
    *p++ = wd[0]; *p++ = wd[1]; *p++ = wd[2];
    *p++ = ' ';

    // Month
    const char *mn = mon_name[tm->tm_mon % 12];
    *p++ = mn[0]; *p++ = mn[1]; *p++ = mn[2];
    *p++ = ' ';

    // Day of month (space-padded)
    int day = tm->tm_mday;
    if (day < 10) {
        *p++ = ' ';
        *p++ = '0' + day;
    } else {
        *p++ = '0' + (day / 10);
        *p++ = '0' + (day % 10);
    }
    *p++ = ' ';

    // Hour
    format_2digit(p, tm->tm_hour);
    p += 2;
    *p++ = ':';

    // Minute
    format_2digit(p, tm->tm_min);
    p += 2;
    *p++ = ':';

    // Second
    format_2digit(p, tm->tm_sec);
    p += 2;
    *p++ = ' ';

    // Year (4 digits)
    int year = tm->tm_year + 1900;
    *p++ = '0' + (year / 1000);
    *p++ = '0' + ((year / 100) % 10);
    *p++ = '0' + ((year / 10) % 10);
    *p++ = '0' + (year % 10);

    *p++ = '\n';
    *p = '\0';

    return time_string;
}

// Convert time_t to string
char *ctime(const time_t *timer) {
    return asctime(gmtime(timer));
}

// difftime - returns difference in seconds
// Note: Returns long long instead of double (SLOW-32 has no FPU)
long long difftime(time_t time1, time_t time0) {
    return (long long)time1 - (long long)time0;
}
