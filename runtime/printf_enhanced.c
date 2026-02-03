// Enhanced printf implementation for SLOW-32
// Based on TinyMUX mux_vsnprintf with optimizations

#include <stddef.h>
#include <stdbool.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "convert.h"

// Forward declarations
size_t strlen(const char *s);
void *memcpy(void *dest, const void *src, size_t n);

// Buffer size for conversions
#define CONV_BUF_SIZE 32

// dtoa (David Gay) interface
char *dtoa(double d, int mode, int ndigits, int *decpt, int *sign, char **rve);
void freedtoa(char *s);

static char *alloc_and_copy(const char *src, size_t len) {
    char *buf = (char *)malloc(len + 1);
    if (!buf) {
        return NULL;
    }
    memcpy(buf, src, len);
    buf[len] = '\0';
    return buf;
}

static size_t append_char(char *out, size_t pos, char ch) {
    out[pos++] = ch;
    return pos;
}

static size_t append_repeat(char *out, size_t pos, char ch, size_t count) {
    for (size_t i = 0; i < count; i++) {
        out[pos++] = ch;
    }
    return pos;
}

static size_t append_str(char *out, size_t pos, const char *s, size_t len) {
    memcpy(out + pos, s, len);
    return pos + len;
}

static size_t format_exponent(char *out, size_t pos, int exp, bool upper) {
    char exp_char = upper ? 'E' : 'e';
    out[pos++] = exp_char;

    if (exp < 0) {
        out[pos++] = '-';
        exp = -exp;
    } else {
        out[pos++] = '+';
    }

    char tmp[8];
    int idx = 0;
    do {
        tmp[idx++] = (char)('0' + (exp % 10));
        exp /= 10;
    } while (exp > 0 && idx < (int)sizeof(tmp));

    if (idx == 1) {
        out[pos++] = '0';
        out[pos++] = tmp[0];
    } else {
        for (int i = idx - 1; i >= 0; i--) {
            out[pos++] = tmp[i];
        }
    }

    return pos;
}

static size_t build_f(char *out, const char *digits, size_t len, int decpt,
                      int precision, bool force_decimal, bool pad_zeros) {
    size_t pos = 0;

    if (len == 0) {
        digits = "0";
        len = 1;
        decpt = 1;
    }

    if (decpt > 0) {
        size_t int_len = (decpt < (int)len) ? (size_t)decpt : len;
        pos = append_str(out, pos, digits, int_len);
        if (decpt > (int)len) {
            pos = append_repeat(out, pos, '0', (size_t)(decpt - (int)len));
        }

        size_t frac_len = (decpt < (int)len) ? (len - (size_t)decpt) : 0;
        size_t frac_target = pad_zeros ? (size_t)precision : frac_len;
        if (force_decimal || frac_target > 0) {
            pos = append_char(out, pos, '.');
            if (frac_len > 0) {
                pos = append_str(out, pos, digits + int_len, frac_len);
            }
            if (pad_zeros && frac_target > frac_len) {
                pos = append_repeat(out, pos, '0', frac_target - frac_len);
            }
        }
    } else {
        pos = append_char(out, pos, '0');
        size_t frac_len = len + (size_t)(-decpt);
        size_t frac_target = pad_zeros ? (size_t)precision : frac_len;
        if (force_decimal || frac_target > 0) {
            pos = append_char(out, pos, '.');
            if (decpt < 0) {
                pos = append_repeat(out, pos, '0', (size_t)(-decpt));
            }
            pos = append_str(out, pos, digits, len);
            if (pad_zeros && frac_target > frac_len) {
                pos = append_repeat(out, pos, '0', frac_target - frac_len);
            }
        }
    }

    return pos;
}

static size_t build_e(char *out, const char *digits, size_t len, int decpt,
                      int precision, bool force_decimal, bool pad_zeros, bool upper) {
    size_t pos = 0;

    if (len == 0) {
        digits = "0";
        len = 1;
        decpt = 1;
    }

    pos = append_char(out, pos, digits[0]);
    size_t frac_len = (len > 1) ? (len - 1) : 0;
    size_t frac_target = pad_zeros ? (size_t)precision : frac_len;

    if (force_decimal || frac_target > 0) {
        pos = append_char(out, pos, '.');
        if (frac_len > 0) {
            pos = append_str(out, pos, digits + 1, frac_len);
        }
        if (pad_zeros && frac_target > frac_len) {
            pos = append_repeat(out, pos, '0', frac_target - frac_len);
        }
    }

    int exp = decpt - 1;
    pos = format_exponent(out, pos, exp, upper);
    return pos;
}

__attribute__((noinline))
static char *format_float_alloc(double val, char spec, int precision, bool has_precision,
                                bool alt_form, bool upper, size_t *out_len,
                                bool *out_is_negative) {
    union {
        double d;
        uint64_t u;
    } bits;
    bits.d = val;

    bool sign = (bits.u >> 63) != 0;
    uint64_t exp = (bits.u >> 52) & 0x7ffu;
    uint64_t frac = bits.u & 0x000fffffffffffffu;

    if (exp == 0x7ffu) {
        if (frac != 0) {
            const char *nan_str = upper ? "NAN" : "nan";
            *out_is_negative = false;
            *out_len = 3;
            return alloc_and_copy(nan_str, 3);
        } else {
            const char *inf_str = upper ? "INF" : "inf";
            *out_is_negative = sign;
            *out_len = 3;
            return alloc_and_copy(inf_str, 3);
        }
    }

    *out_is_negative = sign;
    if (sign) {
        val = -val;
    }

    if (!has_precision) {
        precision = 6;
    }

    if (spec == 'g' || spec == 'G') {
        if (precision == 0) {
            precision = 1;
        }
    }

    int decpt = 0;
    int dtoa_sign = 0;
    char *rve = NULL;
    char *digits = NULL;
    int mode = 0;
    int ndigits = 0;

    if (spec == 'f' || spec == 'F') {
        mode = 3;
        ndigits = precision;
    } else if (spec == 'e' || spec == 'E') {
        mode = 2;
        ndigits = precision + 1;
    } else {
        mode = 2;
        ndigits = precision;
    }

    digits = dtoa(val, mode, ndigits, &decpt, &dtoa_sign, &rve);
    if (!digits) {
        *out_len = 0;
        return NULL;
    }

    size_t digits_len = (rve != NULL) ? (size_t)(rve - digits) : strlen(digits);
    int exp10 = decpt - 1;
    bool use_exp = (spec == 'e' || spec == 'E');
    if (spec == 'g' || spec == 'G') {
        use_exp = (exp10 < -4 || exp10 >= precision);
    }

    size_t extra = (size_t)precision + 32;
    size_t out_cap = digits_len + extra;
    if (decpt < 0) {
        out_cap += (size_t)(-decpt);
    } else {
        out_cap += (size_t)decpt;
    }

    char *out = (char *)malloc(out_cap + 1);
    if (!out) {
        freedtoa(digits);
        *out_len = 0;
        return NULL;
    }

    size_t len = 0;
    if (spec == 'f' || spec == 'F') {
        len = build_f(out, digits, digits_len, decpt, precision,
                      (precision > 0) || alt_form, true);
    } else if (spec == 'e' || spec == 'E') {
        len = build_e(out, digits, digits_len, decpt, precision,
                      (precision > 0) || alt_form, true, upper);
    } else {
        if (use_exp) {
            int e_precision = precision - 1;
            if (e_precision < 0) e_precision = 0;
            len = build_e(out, digits, digits_len, decpt, e_precision,
                          alt_form, alt_form, upper);
        } else {
            int frac_prec = precision - decpt;
            if (frac_prec < 0) frac_prec = 0;
            len = build_f(out, digits, digits_len, decpt, frac_prec,
                          alt_form, alt_form);
        }
    }

    out[len] = '\0';
    *out_len = len;
    freedtoa(digits);
    return out;
}

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
        } else if (*fmt == 'z') {
            // size_t modifier
            fmt++;
        } else if (*fmt == 'h') {
            // Short modifier
            fmt++;
            if (*fmt == 'h') {
                // char modifier
                fmt++;
            }
        }

        // Handle conversion specifier
        char conv_buf[CONV_BUF_SIZE];
        char *conv_str = conv_buf;
        size_t conv_len = 0;
        bool is_negative = false;

        int spec = (unsigned char)*fmt;
        bool pointer_format = false;
        char *conv_alloc = NULL;

        switch (spec) {
            case 'd':
            case 'i': {
                if (length_mod >= 2) {
                    long long val = va_arg(ap, long long);
                    conv_len = slow32_ltoa64(val, conv_buf);
                    if (val < 0) {
                        is_negative = true;
                    }
                } else if (length_mod == 1) {
                    long val = va_arg(ap, long);
                    if (sizeof(long) > 4) {
                        conv_len = slow32_ltoa64(val, conv_buf);
                        if (val < 0) is_negative = true;
                    } else {
                        conv_len = slow32_ltoa((int32_t)val, conv_buf);
                        if (val < 0) is_negative = true;
                    }
                } else {
                    int val = va_arg(ap, int);
                    conv_len = slow32_ltoa(val, conv_buf);
                    if (val < 0) is_negative = true;
                }
                break;
            }

            case 'u': {
                if (length_mod >= 2) {
                    unsigned long long val = va_arg(ap, unsigned long long);
                    conv_len = slow32_utoa64(val, conv_buf);
                } else if (length_mod == 1) {
                    unsigned long val = va_arg(ap, unsigned long);
                    if (sizeof(long) > 4) {
                        conv_len = slow32_utoa64(val, conv_buf);
                    } else {
                        conv_len = slow32_utoa((uint32_t)val, conv_buf);
                    }
                } else {
                    unsigned val = va_arg(ap, unsigned);
                    conv_len = slow32_utoa(val, conv_buf);
                }
                break;
            }

            case 'x':
            case 'X': {
                bool uppercase = (spec == 'X');
                if (length_mod >= 2) {
                    unsigned long long val = va_arg(ap, unsigned long long);
                    conv_len = slow32_utox64((uint64_t)val, conv_buf, uppercase);
                } else if (length_mod == 1) {
                    unsigned long val = va_arg(ap, unsigned long);
                    if (sizeof(long) > 4) {
                        conv_len = slow32_utox64((uint64_t)val, conv_buf, uppercase);
                    } else {
                        conv_len = slow32_utox((uint32_t)val, conv_buf, uppercase);
                    }
                } else {
                    unsigned val = va_arg(ap, unsigned);
                    conv_len = slow32_utox(val, conv_buf, uppercase);
                }
                break;
            }

            case 'o': {
                if (length_mod >= 2) {
                    unsigned long long val = va_arg(ap, unsigned long long);
                    conv_len = slow32_utoo64(val, conv_buf);
                } else if (length_mod == 1) {
                    unsigned long val = va_arg(ap, unsigned long);
                    if (sizeof(long) > 4) {
                        conv_len = slow32_utoo64(val, conv_buf);
                    } else {
                        conv_len = slow32_utoo((uint32_t)val, conv_buf);
                    }
                } else {
                    unsigned val = va_arg(ap, unsigned);
                    conv_len = slow32_utoo(val, conv_buf);
                }
                break;
            }

            case 'p': {
                void *ptr = va_arg(ap, void*);
                uintptr_t val = (uintptr_t)ptr;
                pointer_format = true;
#if UINTPTR_MAX > 0xFFFFFFFFu
                conv_len = slow32_utox64((uint64_t)val, conv_buf, false);
#else
                conv_len = slow32_utox((uint32_t)val, conv_buf, false);
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

            case 'f':
            case 'F':
            case 'e':
            case 'E':
            case 'g':
            case 'G': {
                double val = va_arg(ap, double);
                bool upper = (spec == 'F' || spec == 'E' || spec == 'G');
                bool neg = false;
                size_t flen = 0;

                conv_alloc = format_float_alloc(val, (char)spec, precision, has_precision,
                                                alt_form, upper, &flen, &neg);
                if (conv_alloc == NULL || flen == 0) {
                    conv_buf[0] = '?';
                    conv_buf[1] = '\0';
                    conv_len = 1;
                } else {
                    conv_str = conv_alloc;
                    conv_len = flen;
                }
                is_negative = neg;
                break;
            }

            default: {
                // Unknown format - output as-is
                conv_buf[0] = '%';
                conv_buf[1] = (char)spec;
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
        bool is_signed_conv =
            (spec == 'd' || spec == 'i' ||
             spec == 'f' || spec == 'F' ||
             spec == 'e' || spec == 'E' ||
             spec == 'g' || spec == 'G');
        if (is_negative && conv_str[0] == '-') {
            // Already has minus sign
        } else if (show_sign && !is_negative && is_signed_conv) {
            prefix[prefix_len++] = '+';
        } else if (space_sign && !is_negative && is_signed_conv) {
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

        if (conv_alloc) {
            free(conv_alloc);
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
    if (len > 0) {
        fwrite(buffer, 1, len, stdout);
    }

    return len;
}

// vprintf - format and output to console using va_list
int vprintf(const char *format, va_list ap) {
    char buffer[1024];
    int len = vsnprintf_enhanced(buffer, sizeof(buffer), format, ap);

    // Output to console
    if (len > 0) {
        fwrite(buffer, 1, len, stdout);
    }

    return len;
}

// vfprintf - format and output to FILE stream using va_list
int vfprintf(FILE *stream, const char *format, va_list ap) {
    char buffer[1024];
    int len = vsnprintf_enhanced(buffer, sizeof(buffer), format, ap);

    if (len > 0) {
        if (len >= (int)sizeof(buffer)) len = sizeof(buffer) - 1;
        fwrite(buffer, 1, len, stream);
    }

    return len;
}

// fprintf - format and output to FILE stream
int fprintf(FILE *stream, const char *format, ...) {
    va_list ap;
    va_start(ap, format);
    int len = vfprintf(stream, format, ap);
    va_end(ap);
    return len;
}
