// Benchmark: tight loops, branches, and function calls for DBT stage timing
#include <stdint.h>
#include <stdio.h>

// Prevent inlining to force function call overhead
__attribute__((noinline))
uint32_t fibonacci(int n) {
    uint32_t a = 0, b = 1;
    for (int i = 0; i < n; i++) {
        uint32_t t = a + b;
        a = b;
        b = t;
    }
    return a;
}

__attribute__((noinline))
int is_prime(uint32_t n) {
    if (n < 2) return 0;
    if (n < 4) return 1;
    if ((n & 1) == 0) return 0;
    for (uint32_t i = 3; i * i <= n; i += 2) {
        if (n % i == 0) return 0;
    }
    return 1;
}

__attribute__((noinline))
uint32_t sieve_count(uint32_t limit) {
    // Count primes up to limit using trial division (no array needed)
    uint32_t count = 0;
    for (uint32_t i = 2; i <= limit; i++) {
        if (is_prime(i)) count++;
    }
    return count;
}

__attribute__((noinline))
uint32_t sort_checksum(void) {
    // Simple bubble sort of a small array, repeated many times
    uint32_t arr[32];
    uint32_t checksum = 0;

    for (int rep = 0; rep < 500; rep++) {
        // Initialize with pseudo-random values
        uint32_t seed = (uint32_t)(rep * 2654435761u);
        for (int i = 0; i < 32; i++) {
            seed ^= seed << 13;
            seed ^= seed >> 17;
            seed ^= seed << 5;
            arr[i] = seed;
        }

        // Bubble sort
        for (int i = 0; i < 31; i++) {
            for (int j = 0; j < 31 - i; j++) {
                if (arr[j] > arr[j + 1]) {
                    uint32_t t = arr[j];
                    arr[j] = arr[j + 1];
                    arr[j + 1] = t;
                }
            }
        }

        checksum += arr[0] ^ arr[15] ^ arr[31];
    }
    return checksum;
}

int main(void) {
    // Fibonacci: tight loop with few branches
    uint32_t fib_sum = 0;
    for (int i = 0; i < 10000; i++) {
        fib_sum += fibonacci(40);
    }

    // Prime sieve: nested loops with many branches
    uint32_t prime_count = sieve_count(5000);

    // Sort: nested loops with conditional swaps
    uint32_t sort_ck = sort_checksum();

    // Combine results to prevent dead-code elimination
    uint32_t result = fib_sum ^ prime_count ^ sort_ck;

    printf("fib_sum=0x%08X prime_count=%u sort_ck=0x%08X\n",
           (unsigned)fib_sum, (unsigned)prime_count, (unsigned)sort_ck);
    printf("result=0x%08X\n", (unsigned)result);

    // Return 0 on success (we just care about timing, not exact values)
    return 0;
}
