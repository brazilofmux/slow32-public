#include <stdarg.h>

void debug_char(char c);

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
    
    // Expected: 60
    // Bug behavior: uses 24 instead of 3 for count
    if (result == 60) {
        debug_char('O');
        debug_char('K');
        debug_char('\n');
    } else {
        debug_char('F');
        debug_char('A');
        debug_char('I');
        debug_char('L');
        debug_char('\n');
    }
    
    return 0;
}