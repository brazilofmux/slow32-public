/* Selfhost bootstrap libc: conversion functions
 *
 * strtol, as the standard has it: the white space isspace knows, a
 * 0x that is a prefix only before a hexadecimal digit, a value past
 * either end of long clamped there with ERANGE, the end pointer left
 * at the start when nothing was converted.  Kept to what stage07
 * compiles.
 */

/* errno is mmio_no_start.s's; ERANGE and EINVAL as <errno.h> numbers them */
extern int errno;

/* The value is gathered below zero: a negative long has one more value
 * than a positive one, so every number that fits can be reached that
 * way, and no test needs a wider type or unsigned arithmetic. */
long strtol(const char *nptr, char **endptr, int base) {
    const char *s;
    const char *after;      /* past the last digit taken; 0 while none is */
    long acc;
    long limit;
    long multmin;
    int neg;
    int over;
    int digit;

    s = nptr;
    acc = 0;
    neg = 0;
    over = 0;
    after = (const char *)0;

    if (base != 0 && (base < 2 || base > 36)) {
        errno = 22;
        if (endptr) *endptr = (char *)nptr;
        return 0;
    }

    while (*s == ' ' || (*s >= 9 && *s <= 13)) s = s + 1;

    if (*s == '-') {
        neg = 1;
        s = s + 1;
    } else if (*s == '+') {
        s = s + 1;
    }

    /* 0x is a prefix only when a hexadecimal digit follows it; alone, its 0 is the number */
    if ((base == 0 || base == 16) && s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) {
        digit = s[2];
        if ((digit >= '0' && digit <= '9') || (digit >= 'a' && digit <= 'f') || (digit >= 'A' && digit <= 'F')) {
            s = s + 2;
            base = 16;
        }
    }
    if (base == 0) {
        if (*s == '0') base = 8;
        else base = 10;
    }

    limit = -2147483647;
    if (neg) limit = -2147483647 - 1;
    multmin = limit / base;

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

        if (!over) {
            if (acc < multmin) {
                over = 1;
            } else {
                acc = acc * base;
                if (acc < limit + digit) over = 1;
                else acc = acc - digit;
            }
        }
        s = s + 1;
        after = s;
    }

    if (endptr) {
        if (after) *endptr = (char *)after;
        else *endptr = (char *)nptr;
    }
    if (over) {
        errno = 34;
        if (neg) return limit;
        return 0 - limit;
    }
    if (neg) return acc;
    return 0 - acc;
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

/* slow32_utoa64: superseded by runtime/convert.c in the gen1 lib */

/* slow32_ltoa64: superseded by runtime/convert.c in the gen1 lib */

/* slow32_utox64: superseded by runtime/convert.c in the gen1 lib */

/* slow32_utoo64: superseded by runtime/convert.c in the gen1 lib */
