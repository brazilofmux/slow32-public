// Comprehensive test of all i64 operations

typedef unsigned long long uint64_t;
typedef signed long long int64_t;

int main() {
    // === ARITHMETIC TESTS ===
    
    // Addition with carry
    uint64_t add1 = 4294967295ULL + 1ULL;  // 0xFFFFFFFF + 1 = 0x100000000
    if (add1 != 4294967296ULL) return 1;
    
    // Subtraction with borrow
    uint64_t sub1 = 4294967296ULL - 1ULL;  // 0x100000000 - 1 = 0xFFFFFFFF
    if (sub1 != 4294967295ULL) return 1;
    
    // Multiplication spanning 32-bit boundary
    uint64_t mul1 = 65536ULL * 65536ULL;  // 2^16 * 2^16 = 2^32
    if (mul1 != 4294967296ULL) return 1;
    
    // Unsigned division
    uint64_t udiv1 = 1000000000000ULL / 1000000ULL;  // 10^12 / 10^6 = 10^6
    if (udiv1 != 1000000ULL) return 1;
    
    // Signed division with negative
    int64_t sdiv1 = -1000000000000LL / 1000000LL;  // -10^12 / 10^6 = -10^6
    if (sdiv1 != -1000000LL) return 1;
    
    // Unsigned remainder
    uint64_t urem1 = 1234567890123ULL % 1000000ULL;  // Should get 890123
    if (urem1 != 890123ULL) return 1;
    
    // Signed remainder
    int64_t srem1 = -1234567890123LL % 1000000LL;  // Should get -890123
    if (srem1 != -890123LL) return 1;
    
    // === LOGICAL TESTS ===
    
    // AND operation
    uint64_t and1 = 0xFFFFFFFFFFFFFFFFULL & 0x00000000FFFFFFFFULL;  // = 0xFFFFFFFF
    if (and1 != 4294967295ULL) return 1;
    
    // OR operation
    uint64_t or1 = 0x0000000100000000ULL | 0x00000000000000FFULL;  // = 0x1000000FF
    if (or1 != 4294967551ULL) return 1;
    
    // XOR operation
    uint64_t xor1 = 0xFFFFFFFFFFFFFFFFULL ^ 0x00000000FFFFFFFFULL;  // = 0xFFFFFFFF00000000
    if (xor1 != 0xFFFFFFFF00000000ULL) return 1;
    
    // Shift left
    uint64_t shl1 = 1ULL << 32;  // 1 << 32 = 0x100000000
    if (shl1 != 4294967296ULL) return 1;
    
    // Logical shift right
    uint64_t lshr1 = 4294967296ULL >> 32;  // 0x100000000 >>> 32 = 1
    if (lshr1 != 1ULL) return 1;
    
    // Arithmetic shift right (sign extension)
    int64_t ashr1 = ((int64_t)0xFFFFFFFF00000000LL) >> 32;  // Should sign-extend to -1
    if (ashr1 != -1LL) return 1;
    
    // === COMPARISON TESTS ===
    
    // Equal comparison
    if (!(1234567890123ULL == 1234567890123ULL)) return 1;
    
    // Not equal comparison
    if (!(1234567890123ULL != 1234567890124ULL)) return 1;
    
    // Signed less than (negative < positive)
    if (!(-1LL < 1LL)) return 1;
    
    // Unsigned less than (large positive when viewed as unsigned)
    if (!(1ULL < 0xFFFFFFFFFFFFFFFFULL)) return 1;
    
    // Signed greater than
    if (!(1LL > -1LL)) return 1;
    
    // Unsigned greater than
    if (!(0xFFFFFFFFFFFFFFFFULL > 1ULL)) return 1;
    
    // Less or equal
    if (!(100LL <= 100LL)) return 1;
    
    // Greater or equal
    if (!(100LL >= 100LL)) return 1;
    
    return 0;  // All tests passed
}