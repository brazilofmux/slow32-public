#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "field.h"

int field_format_char(char *buf, int field_len, const char *value) {
    int i, vlen;
    vlen = strlen(value);
    if (vlen > field_len) vlen = field_len;
    for (i = 0; i < vlen; i++)
        buf[i] = value[i];
    for (; i < field_len; i++)
        buf[i] = ' ';
    buf[field_len] = '\0';
    return 0;
}

int field_format_numeric(char *buf, int field_len, int decimals, const char *value) {
    double v;
    int len;
    char tmp[64];

    v = atof(value);
    if (decimals > 0)
        snprintf(tmp, sizeof(tmp), "%*.*f", field_len, decimals, v);
    else
        snprintf(tmp, sizeof(tmp), "%*d", field_len, (int)v);

    len = strlen(tmp);
    if (len > field_len) {
        /* Overflow: fill with asterisks */
        int i;
        for (i = 0; i < field_len; i++)
            buf[i] = '*';
        buf[field_len] = '\0';
        return 0;
    }
    /* Right-justify, space pad */
    if (len < field_len) {
        int pad = field_len - len;
        int i;
        for (i = 0; i < pad; i++)
            buf[i] = ' ';
        memcpy(buf + pad, tmp, len);
    } else {
        memcpy(buf, tmp, len);
    }
    buf[field_len] = '\0';
    return 0;
}

int field_format_date(char *buf, const char *value) {
    /* Input: MM/DD/YY or MM/DD/YYYY, Output: YYYYMMDD (8 bytes) */
    int mm = 0, dd = 0, yy = 0;
    const char *p = value;
    char tmp[16];

    /* Skip leading { if present */
    if (*p == '{') p++;

    mm = atoi(p);
    while (*p && *p != '/') p++;
    if (*p == '/') p++;
    dd = atoi(p);
    while (*p && *p != '/' && *p != '}') p++;
    if (*p == '/') p++;
    yy = atoi(p);

    if (yy < 50) yy += 2000;
    else if (yy < 100) yy += 1900;

    snprintf(tmp, sizeof(tmp), "%04d%02d%02d", yy, mm, dd);
    memcpy(buf, tmp, 8);
    buf[8] = '\0';
    return 0;
}

int field_format_logical(char *buf, const char *value) {
    const char *p = value;
    while (*p == ' ' || *p == '.') p++;
    if (*p == 'T' || *p == 't' || *p == 'Y' || *p == 'y')
        buf[0] = 'T';
    else
        buf[0] = 'F';
    buf[1] = '\0';
    return 0;
}

void field_display_char(char *buf, const char *raw, int field_len) {
    memcpy(buf, raw, field_len);
    buf[field_len] = '\0';
}

void field_display_numeric(char *buf, const char *raw, int field_len) {
    memcpy(buf, raw, field_len);
    buf[field_len] = '\0';
}

void field_display_date(char *buf, const char *raw) {
    /* Input: YYYYMMDD, Output: MM/DD/YY */
    if (raw[0] == ' ' || raw[0] == '\0') {
        strcpy(buf, "  /  /  ");
        return;
    }
    buf[0] = raw[4]; buf[1] = raw[5]; /* MM */
    buf[2] = '/';
    buf[3] = raw[6]; buf[4] = raw[7]; /* DD */
    buf[5] = '/';
    buf[6] = raw[2]; buf[7] = raw[3]; /* YY (last 2 digits of year) */
    buf[8] = '\0';
}

void field_display_logical(char *buf, const char *raw) {
    if (raw[0] == 'T' || raw[0] == 't')
        strcpy(buf, ".T.");
    else
        strcpy(buf, ".F.");
}
