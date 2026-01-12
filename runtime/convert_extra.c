#include <stdlib.h>
#include <string.h>
#include <ctype.h>

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
    if (base < 2 || base > 36) {
        *str = '\0';
        return str;
    }
    
    char *ptr = str;
    char *ptr1 = str;
    char tmp_char;
    int tmp_value;
    
    // Handle negative numbers for base 10
    if (value < 0 && base == 10) {
        value = -value;
        *ptr++ = '-';
        ptr1++;
    }
    
    // Convert to string (backwards)
    char *start = ptr;
    do {
        tmp_value = value % base;
        value /= base;
        
        if (tmp_value < 10) {
            *ptr++ = '0' + tmp_value;
        } else {
            *ptr++ = 'A' + (tmp_value - 10);
        }
    } while (value);
    
    *ptr-- = '\0';
    
    // Reverse the string
    while (ptr1 < ptr) {
        tmp_char = *ptr;
        *ptr-- = *ptr1;
        *ptr1++ = tmp_char;
    }
    
    return str;
}

// utoa - convert unsigned integer to string
char *utoa(unsigned int value, char *str, int base) {
    if (base < 2 || base > 36) {
        *str = '\0';
        return str;
    }
    
    char *ptr = str;
    char *ptr1 = str;
    char tmp_char;
    unsigned int tmp_value;
    
    // Convert to string (backwards)
    do {
        tmp_value = value % base;
        value /= base;
        
        if (tmp_value < 10) {
            *ptr++ = '0' + tmp_value;
        } else {
            *ptr++ = 'A' + (tmp_value - 10);
        }
    } while (value);
    
    *ptr-- = '\0';
    
    // Reverse the string
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

    // Calculate overflow cutoff values for 64-bit
    #define ULLONG_MAX_VAL 0xFFFFFFFFFFFFFFFFULL
    cutoff = ULLONG_MAX_VAL / base;
    cutlim = ULLONG_MAX_VAL % base;

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

    // Calculate overflow limits for signed 64-bit
    #define LLONG_MAX_VAL  0x7FFFFFFFFFFFFFFFLL
    #define LLONG_MIN_VAL  (-LLONG_MAX_VAL - 1LL)
    unsigned long long limit = neg ?
        ((unsigned long long)LLONG_MAX_VAL + 1) :  // -LLONG_MIN
        (unsigned long long)LLONG_MAX_VAL;
    unsigned long long cutoff = limit / base;
    int cutlim = limit % base;

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

// atoll - convert string to long long (convenience wrapper)
long long atoll(const char *nptr) {
    return strtoll(nptr, (char **)0, 10);
}

// ltoa - convert long to string
char *ltoa(long value, char *str, int base) {
    if (base < 2 || base > 36) {
        *str = '\0';
        return str;
    }
    
    char *ptr = str;
    char *ptr1 = str;
    char tmp_char;
    long tmp_value;
    
    // Handle negative numbers for base 10
    if (value < 0 && base == 10) {
        value = -value;
        *ptr++ = '-';
        ptr1++;
    }
    
    // Convert to string (backwards)
    do {
        tmp_value = value % base;
        value /= base;
        
        if (tmp_value < 10) {
            *ptr++ = '0' + tmp_value;
        } else {
            *ptr++ = 'A' + (tmp_value - 10);
        }
    } while (value);
    
    *ptr-- = '\0';
    
    // Reverse the string
    while (ptr1 < ptr) {
        tmp_char = *ptr;
        *ptr-- = *ptr1;
        *ptr1++ = tmp_char;
    }
    
    return str;
}

// ultoa - convert unsigned long to string
char *ultoa(unsigned long value, char *str, int base) {
    if (base < 2 || base > 36) {
        *str = '\0';
        return str;
    }
    
    char *ptr = str;
    char *ptr1 = str;
    char tmp_char;
    unsigned long tmp_value;
    
    // Convert to string (backwards)
    do {
        tmp_value = value % base;
        value /= base;
        
        if (tmp_value < 10) {
            *ptr++ = '0' + tmp_value;
        } else {
            *ptr++ = 'A' + (tmp_value - 10);
        }
    } while (value);
    
    *ptr-- = '\0';
    
    // Reverse the string
    while (ptr1 < ptr) {
        tmp_char = *ptr;
        *ptr-- = *ptr1;
        *ptr1++ = tmp_char;
    }
    
    return str;
}