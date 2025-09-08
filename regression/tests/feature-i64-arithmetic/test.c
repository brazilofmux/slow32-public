// Test i64 arithmetic operations (add/sub with carry/borrow)

typedef unsigned long long uint64_t;
typedef signed long long int64_t;

int main() {
    // Test 1: Simple add without carry (100 + 200 = 300)
    uint64_t a1 = 100ULL + 200ULL;
    if (a1 != 300ULL) return 1;
    
    // Test 2: Add with carry propagation (0xFFFFFFFF + 1 = 0x100000000)
    uint64_t a2 = 4294967295ULL + 1ULL;
    if (a2 != 4294967296ULL) return 1;
    
    // Test 3: Add that wraps around (0xFFFFFFFFFFFFFFFF + 1 = 0)
    uint64_t a3 = 0xFFFFFFFFFFFFFFFFULL + 1ULL;
    if (a3 != 0ULL) return 1;
    
    // Test 4: Simple subtraction (500 - 200 = 300)
    uint64_t s1 = 500ULL - 200ULL;
    if (s1 != 300ULL) return 1;
    
    // Test 5: Subtraction with borrow (0x100000000 - 1 = 0xFFFFFFFF)
    uint64_t s2 = 4294967296ULL - 1ULL;
    if (s2 != 4294967295ULL) return 1;
    
    // Test 6: Subtraction that underflows (0 - 1 = 0xFFFFFFFFFFFFFFFF)
    uint64_t s3 = 0ULL - 1ULL;
    if (s3 != 0xFFFFFFFFFFFFFFFFULL) return 1;
    
    // Test 7: Large numbers addition
    uint64_t a4 = 1311768467463790320ULL + 1147797409030816545ULL;
    if (a4 != 2459565876494606865ULL) return 1;
    
    return 0;  // Success
}