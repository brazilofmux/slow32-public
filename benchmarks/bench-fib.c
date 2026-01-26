// Benchmark: Iterative Fibonacci - register-intensive
// Tests: Register allocation, simple loop

#include <stdio.h>

#define FIB_N 40  // Calculate fib(40)
#define ITERATIONS 500000

__attribute__((noinline))
int fib_iter(int n) {
    if (n <= 1) return n;

    int a = 0, b = 1;
    for (int i = 2; i <= n; i++) {
        int c = a + b;
        a = b;
        b = c;
    }
    return b;
}

int main() {
    // Run multiple iterations to get measurable time
    // Sum results to prevent optimization from hoisting out of loop
    int sum = 0;
    for (int i = 0; i < ITERATIONS; i++) {
        // Vary input slightly to prevent hoisting
        sum += fib_iter(FIB_N + (i & 1));  // Alternates between fib(40) and fib(41)
    }

    // Just verify fib(40) works correctly
    int result = fib_iter(FIB_N);
    printf("fib(%d) = %d, sum=%d\n", FIB_N, result, sum);

    if (result == 102334155) {
        printf("PASS\n");
        return 0;
    } else {
        printf("FAIL: expected 102334155\n");
        return 1;
    }
}
