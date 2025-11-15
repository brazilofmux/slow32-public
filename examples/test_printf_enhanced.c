// Test program for enhanced printf implementation
#include <stdint.h>
#include <stddef.h>

// Enhanced printf functions
int printf_enhanced(const char *format, ...);
int sprintf_enhanced(char *buffer, const char *format, ...);
int snprintf_enhanced(char *buffer, size_t size, const char *format, ...);

// Original functions for comparison
int printf(const char *format, ...);
int sprintf(char *buffer, const char *format, ...);

void putchar(int c);

// Test the enhanced printf features
int main() {
    char buffer[256];

    printf_enhanced("\n=== Enhanced Printf Test Suite ===\n\n");

    // Basic conversions
    printf_enhanced("Basic conversions:\n");
    printf_enhanced("  Decimal: %d, %i\n", 42, -42);
    printf_enhanced("  Unsigned: %u\n", 4294967295U);
    printf_enhanced("  Hex: %x, %X\n", 0xDEADBEEF, 0xDEADBEEF);
    printf_enhanced("  Octal: %o\n", 0755);
    printf_enhanced("  Character: %c\n", 'A');
    printf_enhanced("  String: %s\n", "Hello, World!");
    printf_enhanced("  Pointer: %p\n", (void*)0x12345678);
    printf_enhanced("  Percent: %%\n");

    // Width specifiers
    printf_enhanced("\nWidth specifiers:\n");
    printf_enhanced("  |%10d| (right-aligned)\n", 42);
    printf_enhanced("  |%-10d| (left-aligned)\n", 42);
    printf_enhanced("  |%010d| (zero-padded)\n", 42);
    printf_enhanced("  |%10s| (right-aligned string)\n", "test");
    printf_enhanced("  |%-10s| (left-aligned string)\n", "test");

    // Precision specifiers
    printf_enhanced("\nPrecision specifiers:\n");
    printf_enhanced("  |%.5s| (truncated string)\n", "Hello, World!");
    printf_enhanced("  |%10.5s| (width and precision)\n", "Hello, World!");
    printf_enhanced("  |%-10.5s| (left-aligned with precision)\n", "Hello, World!");

    // Sign flags
    printf_enhanced("\nSign flags:\n");
    printf_enhanced("  |%+d| (force sign)\n", 42);
    printf_enhanced("  |%+d| (negative with sign)\n", -42);
    printf_enhanced("  |% d| (space for positive)\n", 42);
    printf_enhanced("  |% d| (negative with space flag)\n", -42);

    // Alternate form
    printf_enhanced("\nAlternate form (hash):\n");
    char fmt_hash_hex[] = "  |%?x| (hex with 0x)\n";
    char fmt_hash_hex_upper[] = "  |%?X| (HEX with 0X)\n";
    char fmt_hash_oct[] = "  |%?o| (octal with leading 0)\n";
    fmt_hash_hex[4] = '#';
    fmt_hash_hex_upper[4] = '#';
    fmt_hash_oct[4] = '#';
    printf_enhanced(fmt_hash_hex, 0xABCD);
    printf_enhanced(fmt_hash_hex_upper, 0xABCD);
    printf_enhanced(fmt_hash_oct, 64);

    // Combined flags
    printf_enhanced("\nCombined flags:\n");
    printf_enhanced("  |%+10d| (sign and width)\n", 42);
    printf_enhanced("  |%-+10d| (left, sign, width)\n", 42);
    printf_enhanced("  |%0+10d| (zero, sign, width)\n", 42);
    char fmt_hash_zero[] = "  |%?08x| (alt, zero, width for hex)\n";
    fmt_hash_zero[4] = '#';
    printf_enhanced(fmt_hash_zero, 0xFF);

    // Long and long long
    printf_enhanced("\nLong values:\n");
    printf_enhanced("  Long: %ld\n", 1234567890L);
    printf_enhanced("  Long long: %lld\n", 9223372036854775807LL);
    printf_enhanced("  Unsigned long: %lu\n", 4294967295UL);
    printf_enhanced("  Unsigned long long: %llu\n", 18446744073709551615ULL);

    // Edge cases
    printf_enhanced("\nEdge cases:\n");
    printf_enhanced("  Zero: %d\n", 0);
    printf_enhanced("  Negative one: %d\n", -1);
    printf_enhanced("  INT32_MIN: %d\n", -2147483648);
    printf_enhanced("  INT32_MAX: %d\n", 2147483647);
    printf_enhanced("  UINT32_MAX: %u\n", 4294967295U);
    printf_enhanced("  NULL string: %s\n", (char*)0);

    // Large numbers to test two-digit optimization
    printf_enhanced("\nLarge numbers (two-digit optimization):\n");
    printf_enhanced("  %d\n", 123456789);
    printf_enhanced("  %d\n", 987654321);
    printf_enhanced("  %lld\n", 1234567890123456789LL);

    // sprintf test
    printf_enhanced("\nsprintf test:\n");
    sprintf_enhanced(buffer, "Formatted: %d, %s, 0x%x", 42, "test", 0xABCD);
    printf_enhanced("  Buffer contents: %s\n", buffer);

    // snprintf test
    printf_enhanced("\nsnprintf test (limit to 20 chars):\n");
    int len = snprintf_enhanced(buffer, 20, "This is a very long string that will be truncated");
    printf_enhanced("  Buffer: %s\n", buffer);
    printf_enhanced("  Returned length: %d\n", len);

    // Performance comparison
    printf_enhanced("\n=== Performance Test ===\n");
    printf_enhanced("Converting numbers 0-99999 with two-digit optimization...\n");

    // Do a simple performance test
    int sum = 0;
    for (int i = 0; i < 100000; i++) {
        sprintf_enhanced(buffer, "%d", i);
        sum += buffer[0]; // Just to prevent optimization
    }
    printf_enhanced("Done! (checksum: %d)\n", sum);

    // Test format string with multiple arguments
    printf_enhanced("\n=== Complex Format Test ===\n");
    printf_enhanced("Multiple args: %d %s %08x %c %p %% %+5d %-10s\n",
                   123, "hello", 0xABC, 'Z', (void*)0xDEAD, 42, "world");

    printf_enhanced("\n=== Test Complete ===\n");

    return 0;
}
