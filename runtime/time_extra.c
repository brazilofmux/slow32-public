#include <time.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

static int is_leap(int year) {
    return (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0);
}

static const int days_in_month[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

// The caller's struct tm instead of the library's
struct tm *gmtime_r(const time_t *timer, struct tm *result) {
    int64_t t = (int64_t)*timer;
    int64_t t_total = t;
    int year = 1970;
    
    // Calculate year
    if (t >= 0) {
        while (1) {
            int days = is_leap(year) ? 366 : 365;
            int64_t sec_in_year = (int64_t)days * 86400LL;
            if (t < sec_in_year) break;
            t -= sec_in_year;
            year++;
        }
    } else {
        while (t < 0) {
            year--;
            int days = is_leap(year) ? 366 : 365;
            int64_t sec_in_year = (int64_t)days * 86400LL;
            t += sec_in_year;
        }
    }
    result->tm_year = year - 1900;
    
    // Calculate day of year
    result->tm_yday = (int)(t / 86400LL);
    t %= 86400LL;
    
    // Calculate month and day of month
    int d = result->tm_yday;
    int m = 0;
    int leap = is_leap(year);
    while (m < 12) {
        int dim = days_in_month[m];
        if (m == 1 && leap) dim++;
        if (d < dim) break;
        d -= dim;
        m++;
    }
    result->tm_mon = m;
    result->tm_mday = d + 1;
    
    // Calculate time
    result->tm_hour = (int)(t / 3600ULL);
    t %= 3600ULL;
    result->tm_min = (int)(t / 60ULL);
    result->tm_sec = (int)(t % 60ULL);
    
    result->tm_isdst = 0;
    
    // Calculate day of week (1970-01-01 was Thursday=4)
    // Total days from epoch
    int64_t total_days = t_total / 86400LL;
    int64_t rem = t_total % 86400LL;
    if (rem < 0) {
        rem += 86400LL;
        total_days -= 1;
    }
    int wday = (int)((total_days + 4) % 7);
    if (wday < 0) wday += 7;
    result->tm_wday = wday;

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

    // Local wall-clock = gmtime(UTC + offset). Compute via signed math so a
    // westward (negative) offset is applied correctly to the unsigned time_t.
    time_t local = (time_t)((long long)*timer + (long long)gmtoff);
    gmtime_r(&local, result);

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
