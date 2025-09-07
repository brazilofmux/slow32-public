// Test 64-bit division with values from main arguments
// This prevents any compile-time optimization
typedef unsigned long long uint64_t;
typedef signed long long int64_t;

extern void debug_char(int c);

// Prevent inlining with noinline attribute
__attribute__((noinline))
uint64_t udiv64(uint64_t a, uint64_t b) {
    return a / b;
}

__attribute__((noinline))
int64_t sdiv64(int64_t a, int64_t b) {
    return a / b;
}

__attribute__((noinline))
uint64_t umod64(uint64_t a, uint64_t b) {
    return a % b;
}

__attribute__((noinline))
int64_t smod64(int64_t a, int64_t b) {
    return a % b;
}

int main(int argc, char **argv) {
    // Use argc to generate runtime values that can't be optimized
    // This ensures the division operations actually happen
    uint64_t base = (uint64_t)argc * 1000000000000ULL;
    uint64_t divisor = (uint64_t)argc * 1000000ULL;
    
    // Test unsigned division
    uint64_t ur = udiv64(base, divisor);
    
    // Test signed division
    int64_t sr = sdiv64(-(int64_t)base, (int64_t)divisor);
    
    // Test unsigned modulo  
    uint64_t um = umod64(base + 234567890123ULL, divisor);
    
    // Test signed modulo
    int64_t sm = smod64(-(int64_t)(base + 234567890123ULL), (int64_t)divisor);
    
    // Output something to show we ran
    debug_char('R');
    debug_char('U');
    debug_char('N');
    debug_char('\n');
    
    // Return based on results to prevent dead code elimination
    return (int)(ur + (uint64_t)sr + um + (uint64_t)sm) & 0xFF;
}