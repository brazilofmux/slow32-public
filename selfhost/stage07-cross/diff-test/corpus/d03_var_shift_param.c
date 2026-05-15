/* Variable shifts where the shift count is a PARAM whose live range
 * spans the entire function — directly stresses the CX-clobber
 * coalesce path that broke sign_extend_u32. */
static unsigned int rot_mix(unsigned int v, unsigned int n) {
    unsigned int a = v << n;
    unsigned int b = v >> (32 - n);
    unsigned int c = (v << (n & 31)) ^ v;
    unsigned int d = v >> ((n + 1) & 31);
    /* All four uses keep `n` (and `v`) alive across multiple variable
     * shifts.  If either ends up in RCX without the clobber-cross
     * mark, output diverges. */
    return a ^ b ^ c ^ d;
}

int main(void) {
    unsigned int acc = 0;
    unsigned int n;
    for (n = 1; n < 31; n++) {
        unsigned int v;
        for (v = 1; v < 16; v++) {
            acc = acc * 17u + rot_mix(v * 0xDEADBEEFu, n);
        }
    }
    return acc & 0xff;
}
