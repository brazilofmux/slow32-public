#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "convert.h"

#include <errno.h>

// Define limits since we can't include standard limits.h
#define ULONG_MAX 0xFFFFFFFFUL

/* What every strto* begins with: the white space isspace knows, a sign,
 * and the prefix the base allows -- 0x or 0X for 16 (or 0), but only
 * before a hexadecimal digit: in "0x" and "0xg" the number is the 0 and
 * the x is what follows it.  Returns where the digits begin, with the
 * base settled; 0 for a base that is none (EINVAL). */
static int digit_of(int c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'z') return c - 'a' + 10;
    if (c >= 'A' && c <= 'Z') return c - 'A' + 10;
    return 99;
}

static const char *scan_prefix(const char *s, int *base, int *neg) {
    *neg = 0;
    if (*base != 0 && (*base < 2 || *base > 36)) {
        errno = EINVAL;
        return 0;
    }
    while (*s == ' ' || (*s >= '\t' && *s <= '\r')) s++;
    if (*s == '-') {
        *neg = 1;
        s++;
    } else if (*s == '+') {
        s++;
    }
    if ((*base == 0 || *base == 16) && s[0] == '0' && (s[1] == 'x' || s[1] == 'X') && digit_of(s[2]) < 16) {
        s += 2;
        *base = 16;
    } else if (*base == 0) {
        *base = (*s == '0') ? 8 : 10;
    }
    return s;
}

// strtoul - convert string to unsigned long.  Every digit is consumed
// even when the value has passed ULONG_MAX (the result is then
// ULONG_MAX, with ERANGE); a minus sign negates the value as unsigned.
unsigned long strtoul(const char *nptr, char **endptr, int base) {
    unsigned long result = 0;
    unsigned long cutoff;
    unsigned int cutlim;
    int neg, any = 0, over = 0, d;
    const char *s = scan_prefix(nptr, &base, &neg);

    if (!s) {
        if (endptr) *endptr = (char *)nptr;
        return 0;
    }
    cutoff = ULONG_MAX / (unsigned int)base;
    cutlim = ULONG_MAX % (unsigned int)base;
    while ((d = digit_of(*s)) < base) {
        if (result > cutoff || (result == cutoff && (unsigned int)d > cutlim)) over = 1;
        else result = result * base + d;
        any = 1;
        s++;
    }
    if (endptr) *endptr = (char *)(any ? s : nptr);
    if (over) {
        errno = ERANGE;
        return ULONG_MAX;
    }
    return neg ? 0u - result : result;
}

// itoa - convert integer to string (non-standard but useful)
char *itoa(int value, char *str, int base) {
    if (base == 10) {
        slow32_ltoa(value, str);
        return str;
    }
    if (base == 16) {
        slow32_utox((unsigned int)value, str, true);
        return str;
    }
    if (base == 8) {
        slow32_utoo((unsigned int)value, str);
        return str;
    }

    if (base < 2 || base > 36) {
        *str = '\0';
        return str;
    }
    
    char *ptr = str;
    char *ptr1 = str;
    char tmp_char;
    int tmp_value;
    
    // Convert to string (backwards)
    // Note: for non-standard bases, we treat as unsigned or signed?
    // Standard practice for itoa with base != 10 is usually unsigned logic
    // but handled carefully.
    // My previous impl handled negative for base 10 only.
    // So for base != 10, it fell through to unsigned-like division?
    // Wait, previous impl:
    // if (value < 0 && base == 10) { ... }
    // do { tmp_value = value % base; value /= base; ... }
    // If value is negative and base != 10, % returns negative?
    // In C, % with negative operand is implementation defined or negative.
    // If negative, '0' + negative is bad.
    // So usually itoa casts to unsigned for non-10 base.
    
    unsigned int uval = (unsigned int)value;
    do {
        unsigned int digit = uval % base;
        uval /= base;
        if (digit < 10) *ptr++ = '0' + digit;
        else *ptr++ = 'A' + (digit - 10);
    } while (uval);
    
    *ptr-- = '\0';
    
    // Reverse
    while (ptr1 < ptr) {
        tmp_char = *ptr;
        *ptr-- = *ptr1;
        *ptr1++ = tmp_char;
    }
    
    return str;
}

// utoa - convert unsigned integer to string
char *utoa(unsigned int value, char *str, int base) {
    if (base == 10) {
        slow32_utoa(value, str);
        return str;
    }
    if (base == 16) {
        slow32_utox(value, str, true);
        return str;
    }
    if (base == 8) {
        slow32_utoo(value, str);
        return str;
    }

    if (base < 2 || base > 36) {
        *str = '\0';
        return str;
    }
    
    char *ptr = str;
    char *ptr1 = str;
    char tmp_char;
    unsigned int tmp_value;
    
    do {
        tmp_value = value % base;
        value /= base;
        if (tmp_value < 10) *ptr++ = '0' + tmp_value;
        else *ptr++ = 'A' + (tmp_value - 10);
    } while (value);
    
    *ptr-- = '\0';
    while (ptr1 < ptr) {
        tmp_char = *ptr;
        *ptr-- = *ptr1;
        *ptr1++ = tmp_char;
    }
    return str;
}

// strtoull - convert string to unsigned 64-bit integer
#define ULLONG_MAX_VAL 0xFFFFFFFFFFFFFFFFULL
#define LLONG_MAX_VAL  0x7FFFFFFFFFFFFFFFLL
#define LLONG_MIN_VAL  (-LLONG_MAX_VAL - 1LL)

/* The digits, gathered up to `limit` (2^63-1 or more: strtoll's or
 * strtoull's): 0 when they fit, 1 when the value passed it (every digit
 * is consumed all the same).  The test divides, so it is made only near
 * the top: below 2^57 one more digit of any base leaves the value under
 * 2^63, which is under any limit this is given. */
static int scan_digits64(const char **sp, int base, unsigned long long limit,
                         unsigned long long *value, int *any) {
    const char *s = *sp;
    unsigned long long acc = 0;
    int over = 0, d;

    *any = 0;
    while ((d = digit_of(*s)) < base) {
        if (!over) {
            if (acc >> 57) {
                if (acc > (limit - d) / base) over = 1;
                else acc = acc * base + d;
            } else {
                acc = acc * base + d;
            }
        }
        *any = 1;
        s++;
    }
    *sp = s;
    *value = acc;
    return over;
}

unsigned long long strtoull(const char *nptr, char **endptr, int base) {
    unsigned long long result;
    int neg, any, over;
    const char *s = scan_prefix(nptr, &base, &neg);

    if (!s) {
        if (endptr) *endptr = (char *)nptr;
        return 0;
    }
    over = scan_digits64(&s, base, ULLONG_MAX_VAL, &result, &any);
    if (endptr) *endptr = (char *)(any ? s : nptr);
    if (over) {
        errno = ERANGE;
        return ULLONG_MAX_VAL;
    }
    return neg ? 0ULL - result : result;
}

// strtoll - convert string to signed 64-bit integer
long long strtoll(const char *nptr, char **endptr, int base) {
    unsigned long long acc;
    int neg, any, over;
    const char *s = scan_prefix(nptr, &base, &neg);

    if (!s) {
        if (endptr) *endptr = (char *)nptr;
        return 0;
    }
    /* a negative number has one more value than a positive one */
    over = scan_digits64(&s, base, (unsigned long long)LLONG_MAX_VAL + (neg ? 1u : 0u), &acc, &any);
    if (endptr) *endptr = (char *)(any ? s : nptr);
    if (over) {
        errno = ERANGE;
        return neg ? LLONG_MIN_VAL : LLONG_MAX_VAL;
    }
    return neg ? (long long)(0ULL - acc) : (long long)acc;
}

// atoll
long long atoll(const char *nptr) {
    return strtoll(nptr, (char **)0, 10);
}

// ltoa
char *ltoa(long value, char *str, int base) {
    if (base == 10) {
        slow32_ltoa((int)value, str);
        return str;
    }
    return itoa((int)value, str, base);
}

// ultoa
char *ultoa(unsigned long value, char *str, int base) {
    if (base == 10) {
        slow32_utoa((unsigned int)value, str);
        return str;
    }
    return utoa((unsigned int)value, str, base);
}
