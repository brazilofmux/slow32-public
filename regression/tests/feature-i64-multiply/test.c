// Test i64 multiplication operations

typedef unsigned long long uint64_t;
typedef signed long long int64_t;

int main() {
    // Test 1: Simple multiplication (100 * 200 = 20000)
    uint64_t t1 = 100ULL * 200ULL;
    if (t1 != 20000ULL) return 1;
    
    // Test 2: Multiplication with larger values (1000000 * 1000 = 1000000000)
    uint64_t t2 = 1000000ULL * 1000ULL;
    if (t2 != 1000000000ULL) return 1;
    
    // Test 3: Multiplication by 0
    uint64_t t3 = 123456789ULL * 0ULL;
    if (t3 != 0ULL) return 1;
    
    // Test 4: Multiplication by 1
    uint64_t t4 = 987654321ULL * 1ULL;
    if (t4 != 987654321ULL) return 1;
    
    // Test 5: Multiplication by -1
    int64_t t5 = 123456789LL * -1LL;
    if (t5 != -123456789LL) return 1;
    
    // Test 6: Negative * Negative = Positive
    int64_t t6 = -1000LL * -2000LL;
    if (t6 != 2000000LL) return 1;
    
    // Test 7: Power of 2 multiplication (should optimize to shift)
    uint64_t t7 = 12345ULL * 256ULL;
    if (t7 != 3160320ULL) return 1;
    
    // Test 8: Large multiplication that uses high 32 bits
    // 0x100000 * 0x100000 = 0x10000000000 (uses 40 bits)
    uint64_t t8 = 1048576ULL * 1048576ULL;
    if (t8 != 1099511627776ULL) return 1;
    
    // Test 9: Multiplication that spans both 32-bit halves
    // 0xFFFFFFFF * 2 = 0x1FFFFFFFE
    uint64_t t9 = 4294967295ULL * 2ULL;
    if (t9 != 8589934590ULL) return 1;
    
    // Test 10: Square a 32-bit value
    // 65536^2 = 4294967296 (exactly 2^32)
    uint64_t t10 = 65536ULL * 65536ULL;
    if (t10 != 4294967296ULL) return 1;
    
    // Test 11: Mixed signs with carry
    int64_t t11 = -1000000LL * 1000000LL;
    if (t11 != -1000000000000LL) return 1;
    
    // Test 12: Test overflow wrapping (result truncated to 64 bits)
    // This multiplication would overflow but we just get the low 64 bits
    uint64_t t12 = 9223372036854775807ULL * 2ULL;
    if (t12 != 0xFFFFFFFFFFFFFFFEULL) return 1;
    
    return 0;  // Success
}