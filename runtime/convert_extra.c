#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "convert.h"

// Define limits since we can't include standard limits.h
#define ULONG_MAX 0xFFFFFFFFUL

// strtoul - convert string to unsigned long
unsigned long strtoul(const char *nptr, char **endptr, int base) {
    const char *s = nptr;
    unsigned long result = 0;
    unsigned long cutoff;
    int cutlim;
    int c;
    int neg = 0;
    int any = 0;
    
    // Skip whitespace
    while (isspace(*s)) s++;
    
    // Check for sign
    if (*s == '-') {
        neg = 1;
        s++;
    } else if (*s == '+') {
        s++;
    }
    
    // Determine base
    if ((base == 0 || base == 16) && *s == '0' && (s[1] == 'x' || s[1] == 'X')) {
        s += 2;
        base = 16;
    } else if (base == 0) {
        base = (*s == '0') ? 8 : 10;
    }
    
    // Check base validity
    if (base < 2 || base > 36) {
        if (endptr) *endptr = (char *)nptr;
        return 0;
    }
    
    // Calculate overflow cutoff values
    cutoff = ULONG_MAX / base;
    cutlim = ULONG_MAX % base;
    
    // Process digits
    while ((c = *s) != '\0') {
        if (isdigit(c)) {
            c -= '0';
        } else if (isalpha(c)) {
            c = toupper(c) - 'A' + 10;
        } else {
            break;
        }
        
        if (c >= base) break;
        
        // Check for overflow
        if (result > cutoff || (result == cutoff && c > cutlim)) {
            result = ULONG_MAX;
            any = -1;
            break;
        }
        
        result = result * base + c;
        any = 1;
        s++;
    }
    
    if (any < 0) {
        result = ULONG_MAX;
    } else if (neg) {
        result = -result;
    }
    
    if (endptr) {
        *endptr = (char *)(any ? s : nptr);
    }
    
    return result;
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
unsigned long long strtoull(const char *nptr, char **endptr, int base) {
    const char *s = nptr;
    unsigned long long result = 0;
    unsigned long long cutoff;
    int cutlim;
    int c;
    int neg = 0;
    int any = 0;

    while (isspace(*s)) s++;

    if (*s == '-') {
        neg = 1;
        s++;
    } else if (*s == '+') {
        s++;
    }

    if ((base == 0 || base == 16) && *s == '0' && (s[1] == 'x' || s[1] == 'X')) {
        s += 2;
        base = 16;
    } else if (base == 0) {
        base = (*s == '0') ? 8 : 10;
    }

    if (base < 2 || base > 36) {
        if (endptr) *endptr = (char *)nptr;
        return 0;
    }

    #define ULLONG_MAX_VAL 0xFFFFFFFFFFFFFFFFULL
    cutoff = ULLONG_MAX_VAL / base;
    cutlim = ULLONG_MAX_VAL % base;

    while ((c = *s) != '\0') {
        if (isdigit(c)) {
            c -= '0';
        } else if (isalpha(c)) {
            c = toupper(c) - 'A' + 10;
        } else {
            break;
        }

        if (c >= base) break;

        if (result > cutoff || (result == cutoff && c > cutlim)) {
            result = ULLONG_MAX_VAL;
            any = -1;
            break;
        }

        result = result * base + c;
        any = 1;
        s++;
    }

    if (any < 0) {
        result = ULLONG_MAX_VAL;
    } else if (neg) {
        result = -result;
    }

    if (endptr) {
        *endptr = (char *)(any ? s : nptr);
    }

    return result;
}

// strtoll - convert string to signed 64-bit integer
long long strtoll(const char *nptr, char **endptr, int base) {
    const char *s = nptr;
    unsigned long long acc = 0;
    int neg = 0;
    int any = 0;
    int c;

    while (isspace(*s)) s++;

    if (*s == '-') {
        neg = 1;
        s++;
    } else if (*s == '+') {
        s++;
    }

    if ((base == 0 || base == 16) && *s == '0' && (s[1] == 'x' || s[1] == 'X')) {
        s += 2;
        base = 16;
    } else if (base == 0) {
        base = (*s == '0') ? 8 : 10;
    }

    if (base < 2 || base > 36) {
        if (endptr) *endptr = (char *)nptr;
        return 0;
    }

    #define LLONG_MAX_VAL  0x7FFFFFFFFFFFFFFFLL
    #define LLONG_MIN_VAL  (-LLONG_MAX_VAL - 1LL)
    unsigned long long limit = neg ?
        ((unsigned long long)LLONG_MAX_VAL + 1) :
        (unsigned long long)LLONG_MAX_VAL;
    unsigned long long cutoff = limit / base;
    int cutlim = limit % base;

    while ((c = *s) != '\0') {
        if (isdigit(c)) {
            c -= '0';
        } else if (isalpha(c)) {
            c = toupper(c) - 'A' + 10;
        } else {
            break;
        }

        if (c >= base) break;

        if (acc > cutoff || (acc == cutoff && c > cutlim)) {
            acc = limit;
            any = -1;
            break;
        }

        acc = acc * base + c;
        any = 1;
        s++;
    }

    if (endptr) {
        *endptr = (char *)(any ? s : nptr);
    }

    if (any < 0) {
        return neg ? LLONG_MIN_VAL : LLONG_MAX_VAL;
    }

    return neg ? -(long long)acc : (long long)acc;
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
