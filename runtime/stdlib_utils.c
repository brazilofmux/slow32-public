#include <stdlib.h>
#include <string.h>

int abs(int n) {
    return (n < 0) ? -n : n;
}

long labs(long n) {
    return (n < 0) ? -n : n;
}

int atoi(const char *nptr) {
    int result = 0;
    int sign = 1;
    
    while (*nptr == ' ' || (*nptr >= '\t' && *nptr <= '\r')) nptr++;
    
    if (*nptr == '-') {
        sign = -1;
        nptr++;
    } else if (*nptr == '+') {
        nptr++;
    }
    
    while (*nptr >= '0' && *nptr <= '9') {
        result = result * 10 + (*nptr - '0');
        nptr++;
    }
    
    return result * sign;
}

long atol(const char *nptr) {
    long result = 0;
    int sign = 1;
    
    while (*nptr == ' ' || (*nptr >= '\t' && *nptr <= '\r')) nptr++;
    
    if (*nptr == '-') {
        sign = -1;
        nptr++;
    } else if (*nptr == '+') {
        nptr++;
    }
    
    while (*nptr >= '0' && *nptr <= '9') {
        result = result * 10 + (*nptr - '0');
        nptr++;
    }
    
    return result * sign;
}

static unsigned int rand_seed = 1;

int rand(void) {
    rand_seed = rand_seed * 1103515245 + 12345;
    return (rand_seed / 65536) & RAND_MAX;
}

void srand(unsigned int seed) {
    rand_seed = seed;
}

// getenv() is now implemented in envp_mmio.c (MMIO) or envp_stub.c (debug)
