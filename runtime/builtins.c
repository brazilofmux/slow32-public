// 64-bit integer arithmetic builtins for SLOW32
// These implement the compiler-rt/libgcc functions that LLVM expects
// when expanding 64-bit operations on a 32-bit target

typedef unsigned int uint32_t;
typedef signed int int32_t;
typedef unsigned long long uint64_t;
typedef signed long long int64_t;

// Union for accessing 64-bit values as two 32-bit parts
typedef union {
    int64_t ll;
    struct {
        uint32_t lo;
        int32_t hi;
    } s;
} di_int;

typedef union {
    uint64_t ll;
    struct {
        uint32_t lo;
        uint32_t hi;
    } s;
} du_int;

extern uint32_t __udivsi3(uint32_t n, uint32_t d);

// 32-bit unsigned modulo helper expected by LLVM when lowering UREM
uint32_t __umodsi3(uint32_t n, uint32_t d) {
    if (d == 0)
        return n; // match compiler-rt semantics

    uint32_t q = __udivsi3(n, d);
    return n - q * d;
}

// 64-bit unsigned division
uint64_t __udivdi3(uint64_t n, uint64_t d) {
    if (d == 0) {
        // Division by zero - return max value
        return ~0ULL;
    }
    
    // Special case: dividend < divisor
    if (n < d) {
        return 0;
    }
    
    // Special case: divisor is power of 2
    if ((d & (d - 1)) == 0) {
        // Find shift amount
        int shift = 0;
        uint64_t temp = d;
        while (temp > 1) {
            temp >>= 1;
            shift++;
        }
        return n >> shift;
    }
    
    // Long division algorithm
    uint64_t quotient = 0;
    uint64_t remainder = 0;
    
    for (int i = 63; i >= 0; i--) {
        remainder = (remainder << 1) | ((n >> i) & 1);
        if (remainder >= d) {
            remainder -= d;
            quotient |= (1ULL << i);
        }
    }
    
    return quotient;
}

// 64-bit signed division
int64_t __divdi3(int64_t n, int64_t d) {
    // Handle signs
    int neg = 0;
    uint64_t un = n;
    uint64_t ud = d;
    
    if (n < 0) {
        neg = !neg;
        un = -n;
    }
    
    if (d < 0) {
        neg = !neg;
        ud = -d;
    }
    
    uint64_t uq = __udivdi3(un, ud);
    
    return neg ? -(int64_t)uq : (int64_t)uq;
}

// 64-bit unsigned modulo
uint64_t __umoddi3(uint64_t n, uint64_t d) {
    if (d == 0) {
        // Division by zero - return dividend
        return n;
    }
    
    // Special case: dividend < divisor
    if (n < d) {
        return n;
    }
    
    // Special case: divisor is power of 2
    if ((d & (d - 1)) == 0) {
        return n & (d - 1);
    }
    
    // Long division algorithm to get remainder
    uint64_t remainder = 0;
    
    for (int i = 63; i >= 0; i--) {
        remainder = (remainder << 1) | ((n >> i) & 1);
        if (remainder >= d) {
            remainder -= d;
        }
    }
    
    return remainder;
}

// 64-bit signed modulo
int64_t __moddi3(int64_t n, int64_t d) {
    // Result has sign of dividend
    int neg = (n < 0);
    uint64_t un = n < 0 ? -n : n;
    uint64_t ud = d < 0 ? -d : d;
    
    uint64_t ur = __umoddi3(un, ud);
    
    return neg ? -(int64_t)ur : (int64_t)ur;
}

// Additional helper functions that might be needed

// Count leading zeros for 64-bit
int __clzdi2(uint64_t x) {
    if (x == 0) return 64;
    
    int count = 0;
    if ((x & 0xFFFFFFFF00000000ULL) == 0) { count += 32; x <<= 32; }
    if ((x & 0xFFFF000000000000ULL) == 0) { count += 16; x <<= 16; }
    if ((x & 0xFF00000000000000ULL) == 0) { count += 8; x <<= 8; }
    if ((x & 0xF000000000000000ULL) == 0) { count += 4; x <<= 4; }
    if ((x & 0xC000000000000000ULL) == 0) { count += 2; x <<= 2; }
    if ((x & 0x8000000000000000ULL) == 0) { count += 1; }
    
    return count;
}

// Arithmetic shift right for 64-bit
int64_t __ashrdi3(int64_t a, int shift) {
    di_int x;
    x.ll = a;
    
    if (shift == 0) return a;
    
    if (shift < 32) {
        // Shift within word
        di_int result;
        result.s.lo = (x.s.lo >> shift) | (((uint32_t)x.s.hi) << (32 - shift));
        result.s.hi = x.s.hi >> shift;
        return result.ll;
    } else {
        // Shift >= 32
        di_int result;
        result.s.lo = x.s.hi >> (shift - 32);
        result.s.hi = x.s.hi >> 31;  // Sign extend
        return result.ll;
    }
}

// Logical shift right for 64-bit
uint64_t __lshrdi3(uint64_t a, int shift) {
    du_int x;
    x.ll = a;
    
    if (shift == 0) return a;
    
    if (shift < 32) {
        // Shift within word
        du_int result;
        result.s.lo = (x.s.lo >> shift) | (x.s.hi << (32 - shift));
        result.s.hi = x.s.hi >> shift;
        return result.ll;
    } else {
        // Shift >= 32
        du_int result;
        result.s.lo = x.s.hi >> (shift - 32);
        result.s.hi = 0;
        return result.ll;
    }
}

// Shift left for 64-bit
uint64_t __ashldi3(uint64_t a, int shift) {
    du_int x;
    x.ll = a;
    
    if (shift == 0) return a;
    
    if (shift < 32) {
        // Shift within word
        du_int result;
        result.s.hi = (x.s.hi << shift) | (x.s.lo >> (32 - shift));
        result.s.lo = x.s.lo << shift;
        return result.ll;
    } else {
        // Shift >= 32
        du_int result;
        result.s.hi = x.s.lo << (shift - 32);
        result.s.lo = 0;
        return result.ll;
    }
}
