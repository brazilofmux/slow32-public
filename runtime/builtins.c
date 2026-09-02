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
    if ((int32_t)(n | d) >= 0)
        return (uint32_t)((int32_t)n % (int32_t)d);   // the hardware REM serves
    uint32_t q = __udivsi3(n, d);
    return n - q * d;
}

#ifdef __clang__
#define S32_NO_OPT
#else
#define S32_NO_OPT
#endif

static int nlz32(uint32_t x) {
    int n = 0;
    if (x <= 0x0000FFFFu) { n += 16; x <<= 16; }
    if (x <= 0x00FFFFFFu) { n += 8; x <<= 8; }
    if (x <= 0x0FFFFFFFu) { n += 4; x <<= 4; }
    if (x <= 0x3FFFFFFFu) { n += 2; x <<= 2; }
    if (x <= 0x7FFFFFFFu) n += 1;
    return n;
}

// (u1:u0) / v with u1 < v: Hacker's Delight divlu2 -- two 32-bit hardware
// divisions and a correction step each, in place of 64 shift-subtract
// rounds.  The arithmetic wraps modulo 2^32 as the algorithm intends.
static uint32_t divlu32(uint32_t u1, uint32_t u0, uint32_t v, uint32_t *r) {
    const uint32_t b = 65536u;
    int s = nlz32(v);
    v <<= s;
    uint32_t vn1 = v >> 16, vn0 = v & 0xFFFFu;
    uint32_t un32 = s ? (u1 << s) | (u0 >> (32 - s)) : u1;
    uint32_t un10 = u0 << s;
    uint32_t un1 = un10 >> 16, un0 = un10 & 0xFFFFu;
    uint32_t q1 = un32 / vn1, rhat = un32 - q1 * vn1;
    while (q1 >= b || q1 * vn0 > b * rhat + un1) { q1--; rhat += vn1; if (rhat >= b) break; }
    uint32_t un21 = un32 * b + un1 - q1 * v;
    uint32_t q0 = un21 / vn1;
    rhat = un21 - q0 * vn1;
    while (q0 >= b || q0 * vn0 > b * rhat + un0) { q0--; rhat += vn1; if (rhat >= b) break; }
    *r = (un21 * b + un0 - q0 * v) >> s;
    return q1 * b + q0;
}

static uint64_t udivmoddi3_core(uint64_t n, uint64_t d, uint64_t *rem) {
    if (d == 0) {
        // Division by zero - return max value
        if (rem)
            *rem = n;
        return ~0ULL;
    }
    
    // Special case: dividend < divisor
    if (n < d) {
        if (rem)
            *rem = n;
        return 0;
    }

    // The divisor fits 32 bits (every power of ten to 10^9, every time_t
    // step, most of what programs divide by): hardware steps.
    if ((d >> 32) == 0) {
        uint32_t d32 = (uint32_t)d, hi = (uint32_t)(n >> 32), lo = (uint32_t)n, r;
        if (hi == 0) {
            uint32_t q = lo / d32;
            if (rem) *rem = lo - q * d32;
            return q;
        }
        uint32_t qhi = hi / d32;
        hi -= qhi * d32;
        uint32_t qlo = divlu32(hi, lo, d32, &r);
        if (rem) *rem = r;
        return ((uint64_t)qhi << 32) | qlo;
    }
    
    // The divisor is 64 bits wide (so is the dividend, since n >= d), and
    // the quotient fits in 32.  Start the shift-subtract loop where the
    // quotient starts: only sr = msb(n) - msb(d) + 1 rounds can set a
    // quotient bit, and the top 64 - sr bits of n seed the remainder below
    // d, which is the invariant the loop needs.  This mirrors what
    // selfhost/stage08/builtins64.s does (GitHub #30); before this the C
    // ran all 64 rounds for such a divisor, and the ledger's truncating
    // MOVE divides by 10^13 on every scaled-decimal store.
    int sr = nlz32((uint32_t)(d >> 32)) - nlz32((uint32_t)(n >> 32)) + 1;
    uint64_t remainder = n >> sr;
    uint32_t q = 0;
    for (int i = sr - 1; i >= 0; i--) {
        remainder = (remainder << 1) | ((n >> i) & 1);
        if (remainder >= d) {
            remainder -= d;
            q |= 1u << i;
        }
    }

    if (rem)
        *rem = remainder;
    return q;
}

// 64-bit unsigned division
uint64_t __udivdi3(uint64_t n, uint64_t d) S32_NO_OPT;
uint64_t __udivdi3(uint64_t n, uint64_t d) {
    return udivmoddi3_core(n, d, 0);
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
uint64_t __umoddi3(uint64_t n, uint64_t d) S32_NO_OPT;
uint64_t __umoddi3(uint64_t n, uint64_t d) {
    if (d == 0) {
        // Match compiler-rt semantics: modulo by zero yields the dividend.
        return n;
    }

    uint64_t quotient = __udivdi3(n, d);
    return n - (quotient * d);
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
