/* Selfhost bootstrap libc: conversion functions
 *
 * Provides strtol for the selfhost assembler.
 * Ported from runtime/stdlib_extra.c, simplified for cc.fth subset-C.
 */

long strtol(const char *nptr, char **endptr, int base) {
    const char *s;
    long result;
    int sign;
    int found_digit;

    s = nptr;
    result = 0;
    sign = 1;
    found_digit = 0;

    /* Skip whitespace */
    while (*s == ' ' || *s == '\t' || *s == '\n' || *s == '\r') s++;

    /* Handle sign */
    if (*s == '-') {
        sign = -1;
        s++;
    } else if (*s == '+') {
        s++;
    }

    /* Handle base */
    if (base == 0) {
        if (*s == '0') {
            s++;
            if (*s == 'x' || *s == 'X') {
                base = 16;
                s++;
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
        int digit;
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
        s++;
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
