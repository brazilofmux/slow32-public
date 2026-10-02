/* The rest of <stdlib.h>: abs, labs, atol, div, ldiv, rand, srand
 * (system is system.c's).  The library had none of them (selfhost ISSUES-75).  Built in
 * phase 2 only. */
#include <stdlib.h>

int abs(int n) {
    return n < 0 ? -n : n;
}

long labs(long n) {
    return n < 0 ? -n : n;
}

long atol(const char *s) {
    return strtol(s, (char **)0, 10);
}

/* C99: the quotient is truncated toward zero, which is what / does */
div_t div(int num, int den) {
    div_t r;
    r.quot = num / den;
    r.rem = num % den;
    return r;
}

ldiv_t ldiv(long num, long den) {
    ldiv_t r;
    r.quot = num / den;
    r.rem = num % den;
    return r;
}

/* The generator is the clang runtime's (runtime/stdlib_utils.c), number
 * for number: a program prints the same sequence whichever compiler
 * built it.  The top bits of a 32-bit linear congruential state, which
 * are the ones worth having. */
static unsigned int rand_state = 1;

int rand(void) {
    rand_state = rand_state * 1103515245u + 12345u;
    return (int)((rand_state >> 16) & RAND_MAX);
}

void srand(unsigned int seed) {
    rand_state = seed;
}
