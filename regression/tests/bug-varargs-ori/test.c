// Test for LLVM backend varargs ORI bug
// The bug: backend uses ORI instead of ADDI for vararg slot offsets
#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>

typedef struct { int x; } Context;

// 6 mixed-type fixed params triggers the bug
void mixed_varargs(Context *ctx, int severity,
                   const char *check_name, const char *file,
                   int line_number, const char *fmt, ...)
{
    char buf[256];
    va_list args;
    va_start(args, fmt);
    vsnprintf(buf, sizeof(buf), fmt, args);
    va_end(args);
    printf("%s\n", buf);
}

int main() {
    Context ctx = {0};
    char code[] = "ABC";
    int64_t val1 = 123456789LL;
    int64_t val2 = 987654321LL;

    // Test 1: string + 64-bit value
    mixed_varargs(&ctx, 0, "test", NULL, 0, "%s:%lld", code, (long long)val1);

    // Test 2: two 64-bit values
    mixed_varargs(&ctx, 0, "test", NULL, 0, "%lld,%lld", (long long)val1, (long long)val2);

    // Test 3: string between two 64-bit values
    mixed_varargs(&ctx, 0, "test", NULL, 0, "%lld:%s:%lld",
                  (long long)val1, code, (long long)val2);

    return 0;
}
