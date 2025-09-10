#include <stdarg.h>

void debug_char(char c);

// Simple varargs sum function
int sum(int count, ...) {
    va_list args;
    va_start(args, count);
    
    int total = 0;
    for (int i = 0; i < count; i++) {
        total += va_arg(args, int);
    }
    
    va_end(args);
    return total;
}

int main() {
    int result = sum(3, 10, 20, 30);
    
    // Output result as characters (60 = '<')
    if (result == 60) {
        debug_char('O');
        debug_char('K');
        debug_char('\n');
    } else {
        debug_char('E');
        debug_char('R');
        debug_char('R');
        debug_char('\n');
    }
    
    return 0;
}