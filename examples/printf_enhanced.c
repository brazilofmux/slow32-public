// Enhanced printf implementation for SLOW-32
// Based on TinyMUX mux_vsnprintf with optimizations

#include <stddef.h>
#include <stdbool.h>
#include <stdarg.h>
#include <stdint.h>

// Forward declarations
void putchar(int c);
size_t strlen(const char *s);
void *memcpy(void *dest, const void *src, size_t n);

// 64-bit division helpers provided by builtins.c
extern uint64_t __udivdi3(uint64_t, uint64_t);
extern uint64_t __umoddi3(uint64_t, uint64_t);

// Two-digit lookup table for fast conversion
// Each pair represents 00, 01, 02...98, 99
static const char Digits100[201] =
    "00010203040506070809"
    "10111213141516171819"
    "20212223242526272829"
    "30313233343536373839"
    "40414243444546474849"
    "50515253545556575859"
    "60616263646566676869"
    "70717273747576777879"
    "80818283848586878889"
    "90919293949596979899";

// Hex digits tables
static const char Digits16L[17] = "0123456789abcdef";
static const char Digits16U[17] = "0123456789ABCDEF";

// Reverse a string in place
static void reverse_string(char *start, char *end) {
    while (start < end) {
        char tmp = *start;
        *start = *end;
        *end = tmp;
        start++;
        end--;
    }
}

// Convert unsigned 32-bit to decimal using two-digit optimization
static size_t utoa32_fast(uint32_t val, char *buf) {
    if (val == 0) {
        buf[0] = '0';
        buf[1] = '\0';
        return 1;
    }

    char *p = buf;

    // Process two digits at a time, storing least significant digit first.
    while (val >= 100) {
        uint32_t rem = val % 100;
        val /= 100;
        const char *digits = Digits100 + (rem * 2);
        *p++ = digits[1];
        *p++ = digits[0];
    }

    if (val >= 10) {
        const char *digits = Digits100 + (val * 2);
        *p++ = digits[1];
        *p++ = digits[0];
    } else {
        *p++ = (char)('0' + val);
    }

    size_t len = (size_t)(p - buf);
    reverse_string(buf, buf + len - 1);
    buf[len] = '\0';
    return len;
}

// Convert unsigned 64-bit to decimal using runtime libcalls (base-100 chunks)
#define S32_MAX_DEC_DIGITS 20

static size_t utoa64_fast(uint64_t val, char *buf) {
    if (val == 0) {
        buf[0] = '0';
        buf[1] = '\0';
        return 1;
    }

    char scratch[S32_MAX_DEC_DIGITS];
    size_t idx = 0;

    while (val >= 100) {
        uint64_t rem = __umoddi3(val, 100ULL);
        val = __udivdi3(val, 100ULL);
        const char *digits = Digits100 + (rem * 2);
        scratch[idx++] = digits[1];
        scratch[idx++] = digits[0];
    }

    if (val >= 10) {
        const char *digits = Digits100 + (val * 2);
        scratch[idx++] = digits[1];
        scratch[idx++] = digits[0];
    } else {
        scratch[idx++] = (char)('0' + val);
    }

    size_t len = idx;
    char *out = buf;
    while (idx > 0) {
        *out++ = scratch[--idx];
    }
    *out = '\0';
    return len;
}

// Convert signed 32-bit to decimal
static size_t ltoa32_fast(int32_t val, char *buf) {
    char *p = buf;
    uint32_t uval;

    if (val < 0) {
        *p++ = '-';
        uval = (val == (-2147483647-1)) ? 2147483648U : (uint32_t)(-val);
    } else {
        uval = (uint32_t)val;
    }

    p += utoa32_fast(uval, p);
    return p - buf;
}

// Convert signed 64-bit to decimal
static size_t ltoa64_fast(int64_t val, char *buf) {
    char *p = buf;
    uint64_t uval;

    if (val < 0) {
        *p++ = '-';
        // Handle INT64_MIN specially
        uval = (val == (-9223372036854775807LL-1)) ? 9223372036854775808ULL : (uint64_t)(-val);
    } else {
        uval = (uint64_t)val;
    }

    p += utoa64_fast(uval, p);
    return p - buf;
}

// Convert to hexadecimal
static size_t utox32(uint32_t val, char *buf, bool uppercase) {
    char *p = buf;
    char *q = p;
    const char *digits = uppercase ? Digits16U : Digits16L;

    if (val == 0) {
        *p++ = '0';
        *p = '\0';
        return 1;
    }

    while (val > 0) {
        *p++ = digits[val & 0xF];
        val >>= 4;
    }

    *p = '\0';
    reverse_string(q, p - 1);
    return p - buf;
}

static size_t utox64(uint64_t val, char *buf, bool uppercase) {
    char *p = buf;
    char *q = p;
    const char *digits = uppercase ? Digits16U : Digits16L;

    if (val == 0) {
        *p++ = '0';
        *p = '\0';
        return 1;
    }

    while (val > 0) {
        *p++ = digits[val & 0xF];
        val >>= 4;
    }

    *p = '\0';
    reverse_string(q, p - 1);
    return p - buf;
}

// Convert to octal
static size_t utoo32(uint32_t val, char *buf) {
    char *p = buf;
    char *q = p;

    if (val == 0) {
        *p++ = '0';
        *p = '\0';
        return 1;
    }

    while (val > 0) {
        *p++ = '0' + (val & 7);
        val >>= 3;
    }

    *p = '\0';
    reverse_string(q, p - 1);
    return p - buf;
}

static size_t utoo64(uint64_t val, char *buf) {
    char *p = buf;
    char *q = p;

    if (val == 0) {
        *p++ = '0';
        *p = '\0';
        return 1;
    }

    while (val > 0) {
        *p++ = '0' + (val & 7ULL);
        val >>= 3;
    }

    *p = '\0';
    reverse_string(q, p - 1);
    return p - buf;
}

// Buffer size for conversions
#define CONV_BUF_SIZE 32

// Enhanced vsnprintf with width, precision, and flags
size_t vsnprintf_enhanced(char *buffer, size_t buffer_size, const char *format, va_list ap) {
    if (buffer == NULL || buffer_size < 1) {
        return 0;
    }

    size_t limit = buffer_size - 1;
    size_t out_idx = 0;
    const char *fmt = format;

    while (*fmt) {
        if (*fmt != '%') {
            // Ordinary character
            if (out_idx < limit) {
                buffer[out_idx++] = *fmt;
            }
            fmt++;
            continue;
        }

        fmt++; // Skip '%'

        // Parse flags
        bool left_justify = false;
        bool zero_pad = false;
        bool show_sign = false;
        bool space_sign = false;
        bool alt_form = false;

        while (*fmt) {
            if (*fmt == '-') {
                left_justify = true;
                fmt++;
            } else if (*fmt == '0') {
                zero_pad = true;
                fmt++;
            } else if (*fmt == '+') {
                show_sign = true;
                fmt++;
            } else if (*fmt == ' ') {
                space_sign = true;
                fmt++;
            } else if (*fmt == '#') {
                alt_form = true;
                fmt++;
            } else {
                break;
            }
        }

        // Parse width
        int width = 0;
        bool has_width = false;
        if (*fmt == '*') {
            width = va_arg(ap, int);
            if (width < 0) {
                left_justify = true;
                width = -width;
            }
            has_width = true;
            fmt++;
        } else {
            while (*fmt >= '0' && *fmt <= '9') {
                width = width * 10 + (*fmt - '0');
                has_width = true;
                fmt++;
            }
        }

        // Parse precision
        int precision = 0;
        bool has_precision = false;
        if (*fmt == '.') {
            fmt++;
            has_precision = true;
            if (*fmt == '*') {
                precision = va_arg(ap, int);
                if (precision < 0) {
                    has_precision = false;
                    precision = 0;
                }
                fmt++;
            } else {
                while (*fmt >= '0' && *fmt <= '9') {
                    precision = precision * 10 + (*fmt - '0');
                    fmt++;
                }
            }
        }

        // Parse length modifiers
        int length_mod = 0; // 0=default, 1=long, 2=long long
        if (*fmt == 'l') {
            length_mod++;
            fmt++;
            if (*fmt == 'l') {
                length_mod++;
                fmt++;
            }
        } else if (*fmt == 'h') {
            // Short modifier - we'll treat as int
            fmt++;
            if (*fmt == 'h') {
                // char modifier - we'll treat as int
                fmt++;
            }
        }

        // Handle conversion specifier
        char conv_buf[CONV_BUF_SIZE];
        char *conv_str = conv_buf;
        size_t conv_len = 0;
        bool is_negative = false;

        char spec = *fmt;
        bool pointer_format = false;

        switch (spec) {
            case 'd':
            case 'i': {
                if (length_mod >= 2) {
                    long long val = va_arg(ap, long long);
                    conv_len = ltoa64_fast(val, conv_buf);
                    if (val < 0) {
                        is_negative = true;
                    }
                } else if (length_mod == 1) {
                    long val = va_arg(ap, long);
                    if (sizeof(long) > 4) {
                        conv_len = ltoa64_fast(val, conv_buf);
                        if (val < 0) is_negative = true;
                    } else {
                        conv_len = ltoa32_fast((int32_t)val, conv_buf);
                        if (val < 0) is_negative = true;
                    }
                } else {
                    int val = va_arg(ap, int);
                    conv_len = ltoa32_fast(val, conv_buf);
                    if (val < 0) is_negative = true;
                }
                break;
            }

            case 'u': {
                if (length_mod >= 2) {
                    unsigned long long val = va_arg(ap, unsigned long long);
                    conv_len = utoa64_fast(val, conv_buf);
                } else if (length_mod == 1) {
                    unsigned long val = va_arg(ap, unsigned long);
                    if (sizeof(long) > 4) {
                        conv_len = utoa64_fast(val, conv_buf);
                    } else {
                        conv_len = utoa32_fast((uint32_t)val, conv_buf);
                    }
                } else {
                    unsigned val = va_arg(ap, unsigned);
                    conv_len = utoa32_fast(val, conv_buf);
                }
                break;
            }

            case 'x':
            case 'X': {
                bool uppercase = (spec == 'X');
                if (length_mod >= 2) {
                    unsigned long long val = va_arg(ap, unsigned long long);
                    conv_len = utox64((uint64_t)val, conv_buf, uppercase);
                } else if (length_mod == 1) {
                    unsigned long val = va_arg(ap, unsigned long);
                    if (sizeof(long) > 4) {
                        conv_len = utox64((uint64_t)val, conv_buf, uppercase);
                    } else {
                        conv_len = utox32((uint32_t)val, conv_buf, uppercase);
                    }
                } else {
                    unsigned val = va_arg(ap, unsigned);
                    conv_len = utox32(val, conv_buf, uppercase);
                }
                break;
            }

            case 'o': {
                if (length_mod >= 2) {
                    unsigned long long val = va_arg(ap, unsigned long long);
                    conv_len = utoo64(val, conv_buf);
                } else if (length_mod == 1) {
                    unsigned long val = va_arg(ap, unsigned long);
                    if (sizeof(long) > 4) {
                        conv_len = utoo64(val, conv_buf);
                    } else {
                        conv_len = utoo32((uint32_t)val, conv_buf);
                    }
                } else {
                    unsigned val = va_arg(ap, unsigned);
                    conv_len = utoo32(val, conv_buf);
                }
                break;
            }

            case 'p': {
                void *ptr = va_arg(ap, void*);
                uintptr_t val = (uintptr_t)ptr;
                pointer_format = true;
#if UINTPTR_MAX > 0xFFFFFFFFu
                conv_len = utox64((uint64_t)val, conv_buf, false);
#else
                conv_len = utox32((uint32_t)val, conv_buf, false);
#endif
                if (!has_width || (size_t)width < (2 + sizeof(uintptr_t) * 2)) {
                    width = (int)(2 + sizeof(uintptr_t) * 2);
                    has_width = true;
                }
                zero_pad = true;
                break;
            }

            case 'c': {
                int ch = va_arg(ap, int);
                conv_buf[0] = (char)ch;
                conv_buf[1] = '\0';
                conv_len = 1;
                break;
            }

            case 's': {
                conv_str = va_arg(ap, char*);
                if (conv_str == NULL) {
                    conv_str = "(null)";
                }
                conv_len = strlen(conv_str);
                if (has_precision && (size_t)precision < conv_len) {
                    conv_len = precision;
                }
                break;
            }

            case '%': {
                conv_buf[0] = '%';
                conv_buf[1] = '\0';
                conv_len = 1;
                break;
            }

            default: {
                // Unknown format - output as-is
                conv_buf[0] = '%';
                conv_buf[1] = spec;
                conv_buf[2] = '\0';
                conv_len = 2;
                break;
            }
        }


        // Calculate padding
        size_t field_width = conv_len;
        size_t padding = 0;

        // Add sign or space if needed
        char prefix[3];
        prefix[0] = '\0';
        prefix[1] = '\0';
        prefix[2] = '\0';
        size_t prefix_len = 0;
        if (is_negative && conv_str[0] == '-') {
            // Already has minus sign
        } else if (show_sign && !is_negative && (spec == 'd' || spec == 'i')) {
            prefix[prefix_len++] = '+';
        } else if (space_sign && !is_negative && (spec == 'd' || spec == 'i')) {
            prefix[prefix_len++] = ' ';
        }

        // Add 0x prefix for alt form hex
        if (pointer_format) {
            prefix[prefix_len++] = '0';
            prefix[prefix_len++] = 'x';
        } else if (alt_form && (spec == 'x' || spec == 'X') && conv_str[0] != '0') {
            prefix[prefix_len++] = '0';
            prefix[prefix_len++] = (spec == 'X') ? 'X' : 'x';
        } else if (alt_form && spec == 'o' && conv_str[0] != '0') {
            prefix[prefix_len++] = '0';
        }

        field_width = conv_len + prefix_len;

        if (has_width && (size_t)width > field_width) {
            padding = width - field_width;
        }

        // Check if everything fits
        size_t total_len = field_width + padding;
        if (out_idx + total_len > limit) {
            // Truncate output
            total_len = limit - out_idx;
        }

        // Output with padding
        char pad_char = zero_pad && !left_justify ? '0' : ' ';

        // For zero padding with sign, output sign first
        if (zero_pad && !left_justify) {
            if (is_negative && conv_str[0] == '-') {
                if (out_idx < limit) {
                    buffer[out_idx++] = '-';
                }
                conv_str++;
                conv_len--;
            }
            if (prefix_len > 0) {
                size_t copy_len = prefix_len;
                if (out_idx + copy_len > limit) {
                    copy_len = limit - out_idx;
                }
                if (copy_len > 0) {
                    memcpy(buffer + out_idx, prefix, copy_len);
                    out_idx += copy_len;
                }
                prefix_len = 0;
            }
        }

        // Left padding
        if (!left_justify && padding > 0) {
            size_t pad_count = padding;
            if (out_idx + pad_count > limit) {
                pad_count = limit - out_idx;
            }
            for (size_t i = 0; i < pad_count; i++) {
                buffer[out_idx++] = pad_char;
            }
        }

        // Prefix
        if (prefix_len > 0) {
            size_t copy_len = prefix_len;
            if (out_idx + copy_len > limit) {
                copy_len = limit - out_idx;
            }
            if (copy_len > 0) {
                memcpy(buffer + out_idx, prefix, copy_len);
                out_idx += copy_len;
            }
        }

        // Main content
        if (conv_len > 0 && out_idx < limit) {
            size_t copy_len = conv_len;
            if (out_idx + copy_len > limit) {
                copy_len = limit - out_idx;
            }
            memcpy(buffer + out_idx, conv_str, copy_len);
            out_idx += copy_len;
        }

        // Right padding
        if (left_justify && padding > 0) {
            size_t pad_count = padding;
            if (out_idx + pad_count > limit) {
                pad_count = limit - out_idx;
            }
            for (size_t i = 0; i < pad_count; i++) {
                buffer[out_idx++] = ' ';
            }
        }

        fmt++;
    }

    buffer[out_idx] = '\0';
    return out_idx;
}

// sprintf - format to buffer (no size limit)
int sprintf(char *buffer, const char *format, ...) {
    va_list ap;
    va_start(ap, format);
    // Use large buffer size since we don't have bounds checking in sprintf
    int result = vsnprintf_enhanced(buffer, 65536, format, ap);
    va_end(ap);
    return result;
}

// snprintf - format to buffer with size limit
int snprintf(char *buffer, size_t size, const char *format, ...) {
    va_list ap;
    va_start(ap, format);
    int result = vsnprintf_enhanced(buffer, size, format, ap);
    va_end(ap);
    return result;
}

// vsnprintf - format to buffer with size limit using va_list
int vsnprintf(char *buffer, size_t size, const char *format, va_list ap) {
    return vsnprintf_enhanced(buffer, size, format, ap);
}

// vsprintf - format to buffer using va_list (no size limit)
int vsprintf(char *buffer, const char *format, va_list ap) {
    return vsnprintf_enhanced(buffer, 65536, format, ap);
}

// printf - format and output to console
int printf(const char *format, ...) {
    char buffer[1024];
    va_list ap;
    va_start(ap, format);
    int len = vsnprintf_enhanced(buffer, sizeof(buffer), format, ap);
    va_end(ap);

    // Output to console
    for (int i = 0; i < len; i++) {
        putchar(buffer[i]);
    }

    return len;
}

// vprintf - format and output to console using va_list
int vprintf(const char *format, va_list ap) {
    char buffer[1024];
    int len = vsnprintf_enhanced(buffer, sizeof(buffer), format, ap);

    // Output to console
    for (int i = 0; i < len; i++) {
        putchar(buffer[i]);
    }

    return len;
}
