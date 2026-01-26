// Benchmark: Prime counting - mixed loops and conditions
// Tests: Division (expensive), nested loops, early exit

#include <stdio.h>

#define MAX_N 50000

__attribute__((noinline))
int is_prime(int n) {
    if (n < 2) return 0;
    if (n == 2) return 1;
    if (n % 2 == 0) return 0;

    for (int i = 3; i * i <= n; i += 2) {
        if (n % i == 0) return 0;
    }
    return 1;
}

__attribute__((noinline))
int count_primes(int max) {
    int count = 0;
    for (int n = 2; n <= max; n++) {
        if (is_prime(n)) {
            count++;
        }
    }
    return count;
}

int main() {
    int primes = count_primes(MAX_N);

    // There are 5133 primes from 2 to 50000
    printf("Primes up to %d: %d\n", MAX_N, primes);

    if (primes == 5133) {
        printf("PASS\n");
        return 0;
    } else {
        printf("FAIL: expected 5133\n");
        return 1;
    }
}
