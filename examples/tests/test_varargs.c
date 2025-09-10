#include <stdarg.h>

void debug_char(char c);

void printf(const char *format, ...) {
    va_list args;
    va_start(args, format);
    
    while (*format) {
        if (*format == '%') {
            format++;
            if (*format == 'd') {
                int num = va_arg(args, int);
                // Simple number printing (just digits)
                if (num < 0) {
                    debug_char('-');
                    num = -num;
                }
                if (num == 0) {
                    debug_char('0');
                } else {
                    // Reverse the digits
                    char buffer[12];
                    int i = 0;
                    while (num > 0) {
                        buffer[i++] = '0' + (num % 10);
                        num /= 10;
                    }
                    // Print reversed
                    while (i > 0) {
                        debug_char(buffer[--i]);
                    }
                }
            } else if (*format == 's') {
                char *str = va_arg(args, char*);
                while (*str) {
                    debug_char(*str++);
                }
            } else {
                debug_char(*format);
            }
        } else {
            debug_char(*format);
        }
        format++;
    }
    
    va_end(args);
}

int main() {
    printf("Hello, World!\n");
    printf("The answer is %d\n", 42);
    printf("Negative: %d\n", -123);
    return 0;
}