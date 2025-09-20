#include <stdlib.h>
#include <string.h>

extern void yield(void);

#include "mmio_ring.h"

void exit(int status) {
    unsigned int req_head = S32_MMIO_REQ_HEAD;
    unsigned int req_tail = S32_MMIO_REQ_TAIL;
    volatile unsigned int *req_ring = S32_MMIO_REQ_RING;

    while (((req_head + 1u) % S32_MMIO_RING_ENTRIES) == req_tail) {
        yield();
        req_tail = S32_MMIO_REQ_TAIL;
    }

    unsigned int idx = req_head * S32_MMIO_DESC_WORDS;
    req_ring[idx + 0] = S32_MMIO_OP_EXIT;
    req_ring[idx + 1] = 0;
    req_ring[idx + 2] = 0;
    req_ring[idx + 3] = (unsigned int)status;

    S32_MMIO_REQ_HEAD = (req_head + 1u) % S32_MMIO_RING_ENTRIES;

    while (1) yield();
}

void abort(void) {
    exit(EXIT_FAILURE);
}

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

char *getenv(const char *name) {
    return NULL;
}
