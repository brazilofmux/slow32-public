/* The function that broke after the edge-fix in cc-x64.
 * PARAM v gets coalesced into a downstream PHI; if cross-CX-clobber
 * isn't propagated through coalesce, v lands in RCX and gets clobbered
 * by the variable shift `1 << (bits - 1)`.
 *
 * Calls the function with many bit widths and accumulates a checksum
 * that depends on every output being correct.
 */
static unsigned int sign_extend_u32(unsigned int v, unsigned int bits) {
    unsigned int sign_bit = 1u << (bits - 1);
    unsigned int mask;
    if (bits == 32) mask = v;
    else mask = v & ((1u << bits) - 1);
    return (mask ^ sign_bit) - sign_bit;
}

int main(void) {
    unsigned int acc = 0;
    unsigned int b;
    for (b = 1; b <= 32; b++) {
        unsigned int v;
        for (v = 0; v < 16; v++) {
            acc = acc * 31u + sign_extend_u32(v ^ 0xCAFEBABEu, b);
        }
    }
    /* exit code: low byte of accumulator */
    return acc & 0xff;
}
