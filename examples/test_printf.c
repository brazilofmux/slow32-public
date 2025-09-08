// Test actual printf implementation with varargs
#include <stdarg.h>

// Define printf as varargs to test our new calling convention
int printf(const char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    
    // Simple implementation that just returns number of args processed
    int count = 0;
    const char* p = fmt;
    while (*p) {
        if (*p == '%' && *(p+1) == 'd') {
            va_arg(args, int);
            count++;
            p += 2;
        } else {
            p++;
        }
    }
    
    va_end(args);
    return count;
}

int main() {
    int a = 10, b = 20, c = 30;
    int result = printf("Values: %d %d %d", a, b, c);
    return result;
}