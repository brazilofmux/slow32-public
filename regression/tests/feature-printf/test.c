// Regression test for printf functionality
// Tests that printf correctly formats integers, strings, characters, hex, and octal
// Previously, there was a bug where %d formatting was broken at -O1 and -O2

#include <stdio.h>

int main() {
    // Test simple string
    printf("Hello, World!\n");

    // Test integer formatting (previously broken)
    printf("Positive: %d\n", 42);
    printf("Negative: %d\n", -42);
    printf("Zero: %d\n", 0);
    printf("Multiple: %d, %d, %d\n", 10, 20, 30);

    // Test unsigned
    printf("Unsigned: %u\n", 42u);
    // Note: There's a bug with large unsigned values (shows 246091775 instead of 4294967295)
    // This appears to be a separate issue from the %d formatting bug that was fixed
    printf("Unsigned max: %u\n", 4294967295u);

    // Test character and string
    printf("Char: %c, String: %s\n", 'X', "test");

    // Test hex (lowercase and uppercase)
    printf("Hex lower: %x\n", 255);
    printf("Hex upper: %X\n", 255);
    printf("Hex zero: %x\n", 0);

    // Test octal (previously showed wrong value)
    printf("Octal: %o\n", 255);
    printf("Octal zero: %o\n", 0);

    // Test pointer format
    printf("Pointer: %p\n", (void*)0xDEADBEEF);

    // Test percent literal
    printf("Percent: %%\n");

    // Complex format string
    printf("Complex: int=%d uint=%u hex=0x%x oct=%o char=%c str=\"%s\"\n",
           -100, 200u, 0xABCD, 0755, 'Z', "hello");

    return 0;
}