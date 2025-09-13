// Minimal printf implementation for SLOW-32
// Based on TinyMUX sprintf code but simplified for our architecture

#include <stddef.h>
#include <stdbool.h>
#include <stdarg.h>

// Forward declarations
void putchar(int c);
size_t strlen(const char *s);
void *memcpy(void *dest, const void *src, size_t n);

// Integer to string conversion helpers
static void ReverseDigits(char *pFirst, char *pLast) {
    while (pFirst < pLast) {
        char temp = *pLast;
        *pLast = *pFirst;
        *pFirst = temp;
        pFirst++;
        pLast--;
    }
}

static const char Digits16U[17] = "0123456789ABCDEF";
static const char Digits16L[17] = "0123456789abcdef";

// Convert unsigned to hex string
static size_t utox(unsigned int uval, char *buf, bool bUpperCase) {
    char *p = buf;
    char *q = p;
    const char *pDigits = bUpperCase ? Digits16U : Digits16L;
    
    if (uval == 0) {
        *p++ = '0';
        *p = '\0';
        return 1;
    }
    
    while (uval > 0) {
        *p++ = pDigits[uval & 0xF];
        uval >>= 4;
    }
    *p = '\0';
    ReverseDigits(q, p-1);
    return p - buf;
}

// Convert unsigned to decimal string
static size_t utoa(unsigned int uval, char *buf) {
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

// Convert unsigned to octal string
static size_t utoo(unsigned int uval, char *buf) {
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

// Convert signed to decimal string
static size_t ltoa(int val, char *buf) {
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

// Simplified vprintf - outputs to console via putchar
int vprintf(const char *format, va_list ap) {
    int count = 0;
    const char *p = format;
    
    while (*p) {
        if (*p != '%') {
            putchar(*p);
            count++;
            p++;
            continue;
        }
        
        p++; // Skip '%'
        
        // Handle format specifiers
        switch (*p) {
            case 'd': // Signed decimal
            case 'i': {
                int val = va_arg(ap, int);
                char buf[12]; // Enough for -2147483648
                size_t len = ltoa(val, buf);
                for (size_t i = 0; i < len; i++) {
                    putchar(buf[i]);
                    count++;
                }
                break;
            }
            
            case 'u': { // Unsigned decimal
                unsigned int val = va_arg(ap, unsigned int);
                char buf[11]; // Enough for 4294967295
                size_t len = utoa(val, buf);
                for (size_t i = 0; i < len; i++) {
                    putchar(buf[i]);
                    count++;
                }
                break;
            }
            
            case 'x': // Lowercase hex
            case 'X': { // Uppercase hex
                unsigned int val = va_arg(ap, unsigned int);
                char buf[9]; // Enough for FFFFFFFF
                size_t len = utox(val, buf, *p == 'X');
                for (size_t i = 0; i < len; i++) {
                    putchar(buf[i]);
                    count++;
                }
                break;
            }
            
            case 'o': { // Octal
                unsigned int val = va_arg(ap, unsigned int);
                char buf[12]; // Enough for 37777777777
                size_t len = utoo(val, buf);
                for (size_t i = 0; i < len; i++) {
                    putchar(buf[i]);
                    count++;
                }
                break;
            }
            
            case 'p': { // Pointer (always with 0x prefix)
                unsigned int val = (unsigned int)va_arg(ap, void*);
                putchar('0');
                putchar('x');
                count += 2;
                char buf[9];
                size_t len = utox(val, buf, false);
                // Pad to 8 digits
                for (size_t i = len; i < 8; i++) {
                    putchar('0');
                    count++;
                }
                for (size_t i = 0; i < len; i++) {
                    putchar(buf[i]);
                    count++;
                }
                break;
            }
            
            case 'c': { // Character
                int c = va_arg(ap, int);
                putchar(c);
                count++;
                break;
            }
            
            case 's': { // String
                const char *str = va_arg(ap, const char*);
                if (str == NULL) {
                    str = "(null)";
                }
                while (*str) {
                    putchar(*str);
                    str++;
                    count++;
                }
                break;
            }
            
            case '%': // Literal %
                putchar('%');
                count++;
                break;
                
            default: // Unknown format - just print it
                putchar('%');
                putchar(*p);
                count += 2;
                break;
        }
        
        p++;
    }
    
    return count;
}

// Main printf function
int printf(const char *format, ...) {
    va_list ap;
    va_start(ap, format);
    int result = vprintf(format, ap);
    va_end(ap);
    return result;
}

// Simplified sprintf - writes to buffer
int vsprintf(char *buffer, const char *format, va_list ap) {
    char *out = buffer;
    const char *p = format;
    
    while (*p) {
        if (*p != '%') {
            *out++ = *p++;
            continue;
        }
        
        p++; // Skip '%'
        
        switch (*p) {
            case 'd':
            case 'i': {
                int val = va_arg(ap, int);
                char buf[12];
                size_t len = ltoa(val, buf);
                memcpy(out, buf, len);
                out += len;
                break;
            }
            
            case 'u': {
                unsigned int val = va_arg(ap, unsigned int);
                char buf[11];
                size_t len = utoa(val, buf);
                memcpy(out, buf, len);
                out += len;
                break;
            }
            
            case 'x':
            case 'X': {
                unsigned int val = va_arg(ap, unsigned int);
                char buf[9];
                size_t len = utox(val, buf, *p == 'X');
                memcpy(out, buf, len);
                out += len;
                break;
            }
            
            case 'o': {
                unsigned int val = va_arg(ap, unsigned int);
                char buf[12];
                size_t len = utoo(val, buf);
                memcpy(out, buf, len);
                out += len;
                break;
            }
            
            case 'p': {
                unsigned int val = (unsigned int)va_arg(ap, void*);
                *out++ = '0';
                *out++ = 'x';
                char buf[9];
                size_t len = utox(val, buf, false);
                for (size_t i = len; i < 8; i++) {
                    *out++ = '0';
                }
                memcpy(out, buf, len);
                out += len;
                break;
            }
            
            case 'c': {
                *out++ = (char)va_arg(ap, int);
                break;
            }
            
            case 's': {
                const char *str = va_arg(ap, const char*);
                if (str == NULL) {
                    str = "(null)";
                }
                size_t len = strlen(str);
                memcpy(out, str, len);
                out += len;
                break;
            }
            
            case '%':
                *out++ = '%';
                break;
                
            default:
                *out++ = '%';
                *out++ = *p;
                break;
        }
        
        p++;
    }
    
    *out = '\0';
    return out - buffer;
}

int sprintf(char *buffer, const char *format, ...) {
    va_list ap;
    va_start(ap, format);
    int result = vsprintf(buffer, format, ap);
    va_end(ap);
    return result;
}