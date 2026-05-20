/* Selfhost bootstrap libc: conversion functions
 *
 * Provides strtol for the selfhost assembler.
 * Ported from runtime/stdlib_extra.c, simplified for cc-min subset-C.
 */

long strtol(const char *nptr, char **endptr, int base) {
    const char *s;
    long result;
    int sign;
    int found_digit;
    int digit;

    s = nptr;
    result = 0;
    sign = 1;
    found_digit = 0;

    /* Skip whitespace */
    while (*s == ' ' || *s == '\t' || *s == '\n' || *s == '\r') s = s + 1;

    /* Handle sign */
    if (*s == '-') {
        sign = -1;
        s = s + 1;
    } else if (*s == '+') {
        s = s + 1;
    }

    /* Handle base */
    if (base == 0) {
        if (*s == '0') {
            s = s + 1;
            if (*s == 'x' || *s == 'X') {
                base = 16;
                s = s + 1;
            } else {
                base = 8;
                found_digit = 1;
            }
        } else {
            base = 10;
        }
    } else if (base == 16) {
        if (*s == '0' && (s[1] == 'x' || s[1] == 'X')) {
            s = s + 2;
        }
    }

    /* Convert digits */
    while (*s) {
        if (*s >= '0' && *s <= '9') {
            digit = *s - '0';
        } else if (*s >= 'a' && *s <= 'z') {
            digit = *s - 'a' + 10;
        } else if (*s >= 'A' && *s <= 'Z') {
            digit = *s - 'A' + 10;
        } else {
            break;
        }

        if (digit >= base) break;

        found_digit = 1;
        result = result * base + digit;
        s = s + 1;
    }

    if (endptr) {
        if (found_digit) {
            *endptr = (char *)s;
        } else {
            *endptr = (char *)nptr;
        }
    }

    return result * sign;
}

/* --- 64-bit conversion functions --- */

static void rev_str(char *start, char *end) {
    char tmp;
    while (start < end) {
        tmp = *start;
        *start = *end;
        *end = tmp;
        start = start + 1;
        end = end - 1;
    }
}

int slow32_utoa64(unsigned long long val, char *buf) {
    int i;
    int d;
    if (val == 0) {
        buf[0] = '0';
        buf[1] = 0;
        return 1;
    }
    i = 0;
    while (val > 0) {
        d = (int)(val % 10);
        buf[i] = '0' + (char)d;
        val = val / 10;
        i = i + 1;
    }
    buf[i] = 0;
    rev_str(buf, buf + i - 1);
    return i;
}

int slow32_ltoa64(long long val, char *buf) {
    char *p;
    unsigned long long uval;
    int len;
    p = buf;
    if (val < 0) {
        *p = '-';
        p = p + 1;
        uval = (unsigned long long)(0 - val);
    } else {
        uval = (unsigned long long)val;
    }
    len = slow32_utoa64(uval, p);
    return (int)(p - buf) + len;
}

int slow32_utox64(unsigned long long val, char *buf, int upper) {
    int i;
    int d;
    if (val == 0) {
        buf[0] = '0';
        buf[1] = 0;
        return 1;
    }
    i = 0;
    while (val > 0) {
        d = (int)(val & 15);
        if (d < 10) buf[i] = '0' + (char)d;
        else buf[i] = (upper ? 'A' : 'a') + (char)(d - 10);
        val = val >> 4;
        i = i + 1;
    }
    buf[i] = 0;
    rev_str(buf, buf + i - 1);
    return i;
}

int slow32_utoo64(unsigned long long val, char *buf) {
    int i;
    int d;
    if (val == 0) {
        buf[0] = '0';
        buf[1] = 0;
        return 1;
    }
    i = 0;
    while (val > 0) {
        d = (int)(val & 7);
        buf[i] = '0' + (char)d;
        val = val >> 3;
        i = i + 1;
    }
    buf[i] = 0;
    rev_str(buf, buf + i - 1);
    return i;
}
