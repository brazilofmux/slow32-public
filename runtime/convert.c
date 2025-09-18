// Integer to string conversion functions
// Separated from printf.c to prevent aggressive inlining optimizations

#include <stdbool.h>
#include <stddef.h>

// Reverse a string in place
static void ReverseDigits(char *pFirst, char *pLast) {
    while (pFirst < pLast) {
        char temp = *pLast;
        *pLast = *pFirst;
        *pFirst = temp;
        pFirst++;
        pLast--;
    }
}

// Convert unsigned to decimal string
size_t utoa(unsigned int uval, char *buf) {
    char *p = buf;
    char *q = p;

    if (uval == 0) {
        *p++ = '0';
        *p = '\0';
        return 1;
    }

    while (uval > 0) {
        *p++ = '0' + (uval % 10);
        uval /= 10;
    }
    *p = '\0';
    ReverseDigits(q, p-1);
    return p - buf;
}

// Convert signed to decimal string
size_t ltoa(int val, char *buf) {
    char *p = buf;
    unsigned int uval;
    bool is_negative = val < 0;

    if (is_negative) {
        *p++ = '-';
        uval = (unsigned int)(-val);
    } else {
        uval = (unsigned int)val;
    }
    p += utoa(uval, p);
    return p - buf;
}

// Convert unsigned to octal string
size_t utoo(unsigned int uval, char *buf) {
    char *p = buf;
    char *q = p;

    if (uval == 0) {
        *p++ = '0';
        *p = '\0';
        return 1;
    }

    while (uval > 0) {
        *p++ = '0' + (uval & 7);
        uval >>= 3;
    }
    *p = '\0';
    ReverseDigits(q, p-1);
    return p - buf;
}

// Hex conversion digits
const char Digits16U[17] = "0123456789ABCDEF";
const char Digits16L[17] = "0123456789abcdef";

// Convert unsigned to hex string (uppercase)
size_t utox(unsigned int uval, char *buf, bool uppercase) {
    char *p = buf;
    char *q = p;
    const char *digits = uppercase ? Digits16U : Digits16L;

    if (uval == 0) {
        *p++ = '0';
        *p = '\0';
        return 1;
    }

    while (uval > 0) {
        *p++ = digits[uval & 0xF];
        uval >>= 4;
    }
    *p = '\0';
    ReverseDigits(q, p-1);
    return p - buf;
}