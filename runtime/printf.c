// Minimal printf implementation for SLOW-32
// Based on TinyMUX sprintf code but simplified for our architecture

#include <stddef.h>
#include <stdbool.h>
#include <stdarg.h>
#include "convert.h"  // Integer conversion functions

// Forward declarations
void putchar(int c);
size_t strlen(const char *s);
void *memcpy(void *dest, const void *src, size_t n);

// Conversion functions are now in convert.c to prevent aggressive inlining
// This prevents the optimizer from incorrectly transforming the division-by-10 loops

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
                size_t len = slow32_ltoa(val, buf);
                for (size_t i = 0; i < len; i++) {
                    putchar(buf[i]);
                    count++;
                }
                break;
            }
            
            case 'u': { // Unsigned decimal
                unsigned int val = va_arg(ap, unsigned int);
                char buf[11]; // Enough for 4294967295
                size_t len = slow32_utoa(val, buf);
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
                size_t len = slow32_utox(val, buf, *p == 'X');
                for (size_t i = 0; i < len; i++) {
                    putchar(buf[i]);
                    count++;
                }
                break;
            }
            
            case 'o': { // Octal
                unsigned int val = va_arg(ap, unsigned int);
                char buf[12]; // Enough for 37777777777
                size_t len = slow32_utoo(val, buf);
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
                size_t len = slow32_utox(val, buf, false);
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
                size_t len = slow32_ltoa(val, buf);
                memcpy(out, buf, len);
                out += len;
                break;
            }
            
            case 'u': {
                unsigned int val = va_arg(ap, unsigned int);
                char buf[11];
                size_t len = slow32_utoa(val, buf);
                memcpy(out, buf, len);
                out += len;
                break;
            }
            
            case 'x':
            case 'X': {
                unsigned int val = va_arg(ap, unsigned int);
                char buf[9];
                size_t len = slow32_utox(val, buf, *p == 'X');
                memcpy(out, buf, len);
                out += len;
                break;
            }
            
            case 'o': {
                unsigned int val = va_arg(ap, unsigned int);
                char buf[12];
                size_t len = slow32_utoo(val, buf);
                memcpy(out, buf, len);
                out += len;
                break;
            }
            
            case 'p': {
                unsigned int val = (unsigned int)va_arg(ap, void*);
                *out++ = '0';
                *out++ = 'x';
                char buf[9];
                size_t len = slow32_utox(val, buf, false);
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
