#include <string.h>
#include <stdlib.h>
#include <time.h>
#include "date.h"

/* Standard astronomical Julian Day Number formula */
int32_t date_to_jdn(int year, int month, int day) {
    int a, y, m;
    a = (14 - month) / 12;
    y = year + 4800 - a;
    m = month + 12 * a - 3;
    return day + (153 * m + 2) / 5 + 365 * y + y / 4 - y / 100 + y / 400 - 32045;
}

void date_from_jdn(int32_t jdn, int *year, int *month, int *day) {
    int32_t a, b, c, d, e, m;
    a = jdn + 32044;
    b = (4 * a + 3) / 146097;
    c = a - (146097 * b) / 4;
    d = (4 * c + 3) / 1461;
    e = c - (1461 * d) / 4;
    m = (5 * e + 2) / 153;
    *day   = e - (153 * m + 2) / 5 + 1;
    *month = m + 3 - 12 * (m / 10);
    *year  = 100 * b + d - 4800 + m / 10;
}

int32_t date_from_dbf(const char *raw) {
    char tmp[5];
    int y, m, d;

    if (raw[0] == ' ' || raw[0] == '\0') return 0;

    /* YYYYMMDD */
    tmp[0] = raw[0]; tmp[1] = raw[1]; tmp[2] = raw[2]; tmp[3] = raw[3]; tmp[4] = '\0';
    y = atoi(tmp);
    tmp[0] = raw[4]; tmp[1] = raw[5]; tmp[2] = '\0';
    m = atoi(tmp);
    tmp[0] = raw[6]; tmp[1] = raw[7]; tmp[2] = '\0';
    d = atoi(tmp);

    if (y == 0 && m == 0 && d == 0) return 0;
    return date_to_jdn(y, m, d);
}

void date_to_dbf(int32_t jdn, char *buf) {
    int y, m, d;
    if (jdn == 0) {
        memcpy(buf, "        ", 8);
        buf[8] = '\0';
        return;
    }
    date_from_jdn(jdn, &y, &m, &d);
    /* Manual formatting to avoid snprintf overhead */
    buf[0] = '0' + (y / 1000) % 10;
    buf[1] = '0' + (y / 100) % 10;
    buf[2] = '0' + (y / 10) % 10;
    buf[3] = '0' + y % 10;
    buf[4] = '0' + m / 10;
    buf[5] = '0' + m % 10;
    buf[6] = '0' + d / 10;
    buf[7] = '0' + d % 10;
    buf[8] = '\0';
}

int32_t date_from_mdy(const char *s) {
    int mm = 0, dd = 0, yy = 0;
    const char *p = s;

    /* Skip leading { if present */
    if (*p == '{') p++;
    while (*p == ' ') p++;

    mm = atoi(p);
    while (*p && *p != '/') p++;
    if (*p == '/') p++;
    dd = atoi(p);
    while (*p && *p != '/' && *p != '}') p++;
    if (*p == '/') p++;
    yy = atoi(p);

    if (yy < 50) yy += 2000;
    else if (yy < 100) yy += 1900;

    if (mm == 0 && dd == 0 && yy == 0) return 0;
    return date_to_jdn(yy, mm, dd);
}

void date_to_mdy(int32_t jdn, char *buf) {
    int y, m, d;
    if (jdn == 0) {
        memcpy(buf, "  /  /  ", 8);
        buf[8] = '\0';
        return;
    }
    date_from_jdn(jdn, &y, &m, &d);
    buf[0] = '0' + m / 10;
    buf[1] = '0' + m % 10;
    buf[2] = '/';
    buf[3] = '0' + d / 10;
    buf[4] = '0' + d % 10;
    buf[5] = '/';
    buf[6] = '0' + (y / 10) % 10;
    buf[7] = '0' + y % 10;
    buf[8] = '\0';
}

int date_dow(int32_t jdn) {
    /* JDN mod 7: 0=Mon, 1=Tue, ..., 6=Sun. We want 1=Sun..7=Sat. */
    int d = (jdn + 1) % 7; /* 0=Sun..6=Sat */
    return d + 1;           /* 1=Sun..7=Sat */
}

static const char *dow_names[] = {
    "", "Sunday", "Monday", "Tuesday", "Wednesday",
    "Thursday", "Friday", "Saturday"
};

static const char *month_names[] = {
    "", "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
};

const char *date_dow_name(int dow) {
    if (dow < 1 || dow > 7) return "";
    return dow_names[dow];
}

const char *date_month_name(int month) {
    if (month < 1 || month > 12) return "";
    return month_names[month];
}

int32_t date_today(void) {
    time_t t;
    struct tm *tm;
    t = time(NULL);
    tm = localtime(&t);
    if (!tm) return 0;
    return date_to_jdn(tm->tm_year + 1900, tm->tm_mon + 1, tm->tm_mday);
}
