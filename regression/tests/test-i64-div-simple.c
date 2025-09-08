// Simple test for 64-bit division
typedef unsigned long long uint64_t;
typedef signed long long int64_t;

int main() {
    // Test 1: Simple unsigned division
    uint64_t a = 1000000ULL;
    uint64_t b = 1000ULL;
    uint64_t result = a / b;
    
    if (result != 1000ULL) {
        return 1;  // Test 1 failed
    }
    
    // Test 2: Signed division
    int64_t c = -1000000LL;
    int64_t d = 1000LL;
    int64_t result2 = c / d;
    
    if (result2 != -1000LL) {
        return 2;  // Test 2 failed
    }
    
    // Test 3: Unsigned modulo
    uint64_t e = 1234567ULL;
    uint64_t f = 1000ULL;
    uint64_t result3 = e % f;
    
    if (result3 != 567ULL) {
        return 3;  // Test 3 failed
    }
    
    // Test 4: Signed modulo
    int64_t g = -1234567LL;
    int64_t h = 1000LL;
    int64_t result4 = g % h;
    
    if (result4 != -567LL) {
        return 4;  // Test 4 failed
    }
    
    return 0;  // All tests passed
}