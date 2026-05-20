/* Long if-else-if chain.  Each arm exercises a different bit pattern
 * test against the input.  Stresses HI_BRC fusion (the compiler should
 * fuse `if (x == K) { ... }` into a single compare-and-branch) and
 * branch-target layout (no fall-through to the right successor for
 * many branches in a row). */
static int classify(unsigned int x) {
    if (x == 0)               return 0;
    if (x == 1)               return 1;
    if ((x & 0xFF) == 0xFF)   return 2;
    if ((x & 0xFFFF) == 0)    return 3;
    if (x < 16)               return 4;
    if (x < 256)              return 5;
    if (x < 65536)            return 6;
    if ((x & 1) == 0)         return 7;
    if ((x & 3) == 1)         return 8;
    if ((x & 7) == 5)         return 9;
    if ((x & 0xF0) == 0xA0)   return 10;
    if (x > 0xFF000000u)      return 11;
    if ((int)x < 0)           return 12;
    return 13;
}

int main(void) {
    unsigned int seed = 0xDEADBEEFu;
    unsigned int acc = 0;
    int i;
    for (i = 0; i < 200; i++) {
        seed = seed * 1103515245u + 12345u;
        acc = acc * 17u + (unsigned int)classify(seed);
        acc = acc + (unsigned int)classify(seed >> 16);
    }
    return acc & 0xff;
}
