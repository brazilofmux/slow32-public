/* Operations mixing signed/unsigned types — promotions, casts, and
 * comparisons across signedness boundaries.  Avoids signed overflow
 * (which is UB) but exercises the cases where the C standard says
 * "convert to unsigned" or "convert to signed" implicitly.
 *
 * Catches casts to/from narrow types (char/short → int → unsigned)
 * that earlier had narrow-load fold bugs (#49 Bug D). */
static unsigned int mix(int s, unsigned int u, signed char sc, unsigned char uc) {
    /* Force each promotion path explicitly. */
    unsigned int a = (unsigned int)s + u;
    unsigned int b = u - (unsigned int)sc;       /* sign-extend then convert */
    unsigned int c = (unsigned int)((int)sc + (int)uc);
    int d = (int)u - s;                          /* signed result */
    unsigned int e = (s > 0) ? u : (unsigned int)(-s);
    unsigned int f = ((int)uc << (s & 7));       /* shift count from signed */
    return a ^ b ^ c ^ (unsigned int)d ^ e ^ f;
}

int main(void) {
    int s_seeds[6];
    s_seeds[0] = -1000;
    s_seeds[1] = -1;
    s_seeds[2] = 0;
    s_seeds[3] = 1;
    s_seeds[4] = 1000;
    s_seeds[5] = 0x40000000;

    unsigned int u_seeds[4];
    u_seeds[0] = 0u;
    u_seeds[1] = 1u;
    u_seeds[2] = 0xFEDCBA98u;
    u_seeds[3] = 0xFFFFFFFFu;

    unsigned int acc = 0;
    int i, j;
    for (i = 0; i < 6; i++) {
        for (j = 0; j < 4; j++) {
            int sc;
            for (sc = -3; sc <= 3; sc++) {
                int uc;
                for (uc = 0; uc < 8; uc++) {
                    acc = acc * 31u + mix(s_seeds[i], u_seeds[j],
                                          (signed char)sc, (unsigned char)uc);
                }
            }
        }
    }
    return acc & 0xff;
}
