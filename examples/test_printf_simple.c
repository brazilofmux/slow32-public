// Simple test for enhanced printf
#include <stdint.h>
#include <stddef.h>

// Enhanced printf functions
int printf_enhanced(const char *format, ...);
void putchar(int c);

int main() {
    printf_enhanced("Enhanced Printf Test\n");
    printf_enhanced("Basic: %d %u %x\n", 42, 100, 0xABCD);
    printf_enhanced("Width: |%10d| |%-10d|\n", 123, 456);
    printf_enhanced("Zero pad: |%08x|\n", 0xFF);
    printf_enhanced("Sign: %+d %+d\n", 42, -42);

    // Test two-digit optimization
    printf_enhanced("Numbers: %d %d %d\n", 99, 100, 12345);

    printf_enhanced("Test complete!\n");
    return 0;
}
