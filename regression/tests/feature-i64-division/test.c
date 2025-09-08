// Test i64 division and remainder operations

typedef unsigned long long uint64_t;
typedef signed long long int64_t;

int main() {
    // Test 1: Simple unsigned division (1000000 / 1000 = 1000)
    uint64_t t1 = 1000000ULL / 1000ULL;
    if (t1 != 1000ULL) return 1;
    
    // Test 2: Unsigned division with 64-bit values (10^12 / 10^6 = 10^6)
    uint64_t t2 = 1000000000000ULL / 1000000ULL;
    if (t2 != 1000000ULL) return 1;
    
    // Test 3: Signed division with negative dividend (-1000000 / 1000 = -1000)
    int64_t t3 = -1000000LL / 1000LL;
    if (t3 != -1000LL) return 1;
    
    // Test 4: Signed division with negative divisor (1000000 / -1000 = -1000)
    int64_t t4 = 1000000LL / -1000LL;
    if (t4 != -1000LL) return 1;
    
    // Test 5: Signed division with both negative (-1000000 / -1000 = 1000)
    int64_t t5 = -1000000LL / -1000LL;
    if (t5 != 1000LL) return 1;
    
    // Test 6: Unsigned remainder (1234567 % 1000 = 567)
    uint64_t t6 = 1234567ULL % 1000ULL;
    if (t6 != 567ULL) return 1;
    
    // Test 7: Unsigned remainder with large values (10^12 + 123 % 10^6 = 123)
    uint64_t t7 = 1000000000123ULL % 1000000ULL;
    if (t7 != 123ULL) return 1;
    
    // Test 8: Signed remainder with negative dividend (-1234567 % 1000 = -567)
    int64_t t8 = -1234567LL % 1000LL;
    if (t8 != -567LL) return 1;
    
    // Test 9: Signed remainder with negative divisor (1234567 % -1000 = 567)
    int64_t t9 = 1234567LL % -1000LL;
    if (t9 != 567LL) return 1;
    
    // Test 10: Division by power of 2 (should optimize to shift)
    uint64_t t10 = 65536ULL / 256ULL;
    if (t10 != 256ULL) return 1;
    
    // Test 11: Remainder by power of 2 (should optimize to mask)
    uint64_t t11 = 65537ULL % 256ULL;
    if (t11 != 1ULL) return 1;
    
    // Test 12: Edge case - division by 1
    uint64_t t12 = 123456789ULL / 1ULL;
    if (t12 != 123456789ULL) return 1;
    
    // Test 13: Edge case - 0 divided by anything
    uint64_t t13 = 0ULL / 12345ULL;
    if (t13 != 0ULL) return 1;
    
    // Test 14: Large 64-bit division (2^63 / 2^32)
    uint64_t t14 = 9223372036854775808ULL / 4294967296ULL;
    if (t14 != 2147483648ULL) return 1;
    
    return 0;  // Success
}