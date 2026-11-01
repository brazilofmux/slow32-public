#include <time.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <limits.h>
#include <errno.h>

static int is_leap(long long year) {
    return (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0);
}

static const short days_before_month[12] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};

// floor division and its remainder, for times before the epoch
static long long floor_div(long long a, long long b) {
    long long q = a / b;
    if ((a % b != 0) && ((a < 0) != (b < 0))) q--;
    return q;
}

// The caller's struct tm instead of the library's.  time_t is signed: a
// time before 1970 is a negative count.  The date is computed directly
// from the day count (days to the civil date through 400-year eras), so
// every time_t takes the same few steps; one whose year does not fit
// tm_year's int is EOVERFLOW, as POSIX has it.
struct tm *gmtime_r(const time_t *timer, struct tm *result) {
    long long t = (long long)*timer;
    long long days = floor_div(t, 86400LL);
    long long secs = t - days * 86400LL;

    // days since 0000-03-01, split into 400-year eras of 146097 days
    long long z = days + 719468LL;
    long long era = floor_div(z, 146097LL);
    long long doe = z - era * 146097LL;                                   // [0, 146096]
    long long yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365; // [0, 399]
    long long doy = doe - (365 * yoe + yoe / 4 - yoe / 100);              // [0, 365], from March 1
    long long mp = (5 * doy + 2) / 153;                                   // [0, 11], March = 0
    int mday = (int)(doy - (153 * mp + 2) / 5 + 1);
    int mon = (int)(mp < 10 ? mp + 2 : mp - 10);                          // [0, 11], January = 0
    long long year = yoe + era * 400 + (mon < 2 ? 1 : 0);

    if (year - 1900 < INT_MIN || year - 1900 > INT_MAX) {
        errno = EOVERFLOW;
        return NULL;
    }
    result->tm_year = (int)(year - 1900);
    result->tm_mon = mon;
    result->tm_mday = mday;
    result->tm_yday = days_before_month[mon] + mday - 1 + ((mon >= 2 && is_leap(year)) ? 1 : 0);
    result->tm_hour = (int)(secs / 3600);
    result->tm_min = (int)(secs / 60 % 60);
    result->tm_sec = (int)(secs % 60);
    result->tm_wday = (int)(days - floor_div(days + 4, 7LL) * 7 + 4);     // 1970-01-01 was a Thursday
    result->tm_isdst = 0;
    result->tm_gmtoff = 0;
    result->tm_zone = "UTC";
    return result;
}

static struct tm static_tm;
static char localtime_zone[8];

// __s32_query_tz is resolved from the linked libc variant: the real MMIO
// implementation in time_mmio.c (libc_mmio) or the UTC stub in time_tz_stub.c
// (libc_debug). Referencing it here pulls whichever object the archive provides.

struct tm *gmtime(const time_t *timer) {
    return gmtime_r(timer, &static_tm);
}

// The zone's abbreviation is kept in one place for every result (it is the
// zone's, not the call's), so the struct tm is the caller's but tm_zone is not.
struct tm *localtime_r(const time_t *timer, struct tm *result) {
    long gmtoff = 0;
    int isdst = 0;
    char abbrev[8] = {0};

    if (__s32_query_tz(*timer, &gmtoff, &isdst, abbrev) != 0) {
        // No timezone service available: behave as UTC.
        return gmtime_r(timer, result);
    }

    // Local wall-clock = gmtime(UTC + offset).  The sum cannot leave
    // time_t's range for any time whose year gmtime_r can represent.
    if ((gmtoff > 0 && *timer > LLONG_MAX - gmtoff) || (gmtoff < 0 && *timer < LLONG_MIN - gmtoff)) {
        errno = EOVERFLOW;
        return NULL;
    }
    time_t local = *timer + gmtoff;
    if (gmtime_r(&local, result) == NULL) {
        return NULL;
    }

    result->tm_isdst = isdst;
    result->tm_gmtoff = gmtoff;
    if (abbrev[0] != '\0') {
        int i = 0;
        for (; i < (int)sizeof(localtime_zone) - 1 && abbrev[i] != '\0'; i++) {
            localtime_zone[i] = abbrev[i];
        }
        localtime_zone[i] = '\0';
        result->tm_zone = localtime_zone;
    } else {
        result->tm_zone = "UTC";
    }
    return result;
}

struct tm *localtime(const time_t *timer) {
    return localtime_r(timer, &static_tm);
}

// mktime, asctime, ctime, difftime and strftime are time_std.c's: one source
// for this library and the self-hosted one.

// The zone is asked for at each conversion (__s32_query_tz), so there is
// nothing for tzset to read ahead of time
void tzset(void) {
}
