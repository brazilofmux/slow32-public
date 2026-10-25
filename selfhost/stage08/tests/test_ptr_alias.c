/* Store-to-load forwarding and dead-store elimination tell two words
 * off one pointer apart by their bytes; anything off two different
 * pointers, or overlapping bytes, still counts as the same memory.
 * Each case below must see the store a forwarded or dropped one would
 * hide. */
#include <stdint.h>

static uint64_t g64[4];

/* Words off one pointer: the high half's store keeps the low half known
 * (forwarded), and the first of two stores to a word is dead. */
uint64_t pairs(uint64_t *z, uint64_t k) {
    z[0] = z[0] + k;
    z[1] = z[1] ^ z[0];
    z[0] = z[0] * 3;
    z[2] = z[0] + z[1];
    return z[0] + z[1] + z[2];
}

/* p and q may be one pointer: a store through q must be seen by p. */
int same_ptr(int *p, int *q) {
    p[0] = 11;
    q[0] = 22;
    return p[0];
}

/* Bytes of a word written off the same pointer: the word's value
 * changes under it. */
unsigned bytes_in_word(unsigned *w) {
    unsigned char *b = (unsigned char *)w;
    w[0] = 0x01020304u;
    b[1] = 0xee;
    return w[0];
}

/* A store read back through an overlapping, unaligned view is live. */
unsigned overlap(unsigned char *buf) {
    unsigned *a = (unsigned *)buf;
    unsigned *b = (unsigned *)(buf + 2);
    a[0] = 0x11223344u;
    b[0] = 0x55667788u;
    return a[0];
}

/* The first store is read by a load of a neighbour that overlaps it,
 * so it is not dead though the word is stored again. */
unsigned live_first(unsigned short *h) {
    unsigned *w = (unsigned *)h;
    unsigned r;
    h[0] = 0x1234;
    h[1] = 0x5678;
    r = w[0];
    h[0] = 0x9abc;
    return r + h[0];
}

int main(void) {
    uint64_t z[3];
    int x;
    unsigned u;
    unsigned char buf[8];
    unsigned short hw[2];
    z[0] = 0x0123456789abcdefULL; z[1] = 0xfedcba9876543210ULL; z[2] = 7;
    g64[0] = pairs(z, 0x1111);
    if (g64[0] != 0x06d3a06d3a071420ULL) return 1;
    if (same_ptr(&x, &x) != 22) return 2;
    if (bytes_in_word(&u) != 0x0102ee04u) return 3;
    if (overlap(buf) != 0x77883344u) return 4;
    if (live_first(hw) != 0x56781234u + 0x9abc) return 5;
    return 0;
}
