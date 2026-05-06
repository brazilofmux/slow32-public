/* Exercise codegen-quality patterns added in the surgical bundle:
 *   - X-form logical immediates (AND/ORR/EOR Xd with bit-pattern imm)
 *   - BIC / ORN / EON register peephole (a & ~b, a | ~b, a ^ ~b)
 *   - Wide ADD/SUB imm12 fast path through SEXT32(ICONST) widening
 *
 * The test runs each pattern once and combines the results into a single
 * exit code (chosen so that a regressed instruction selects the wrong
 * register-form path but keeps the same semantics — making this a
 * correctness check, not a codegen-shape check). */

unsigned long mask_low12(unsigned long p) { return p & 0xFFFFFFFFFFFFF000UL; }
unsigned long set_bit3  (unsigned long p) { return p | 0x8UL; }

unsigned int  bic_w(unsigned int a, unsigned int b) { return a & ~b; }
unsigned int  orn_w(unsigned int a, unsigned int b) { return a | ~b; }
unsigned int  eon_w(unsigned int a, unsigned int b) { return a ^ ~b; }

unsigned long bic_x(unsigned long a, unsigned long b) { return a & ~b; }

unsigned long sub_neg_w(unsigned long a) { return a - (unsigned long)(-5); }
unsigned long sub_pos_w(unsigned long a) { return a - 5UL; }

int main(void) {
    unsigned long m;
    unsigned long s;
    unsigned int  bw, ow, ew;
    unsigned long bx;
    unsigned long sn, sp;

    m  = mask_low12(0x12345678UL);                 /* 0x12345000 */
    s  = set_bit3(0x10UL);                         /* 0x18 */
    bw = bic_w(0xF0F0u, 0x00FFu);                  /* F0F0 & ~00FF = F000 */
    ow = orn_w(0x0F0Fu, 0x00FFu);                  /* 0F0F | ~00FF = FFFFFF0F */
    ew = eon_w(0xF0F0u, 0x00FFu);                  /* F0F0 ^ ~00FF = ~(F0F0 ^ 00FF) = ~F00F = FFFF0FF0 */
    bx = bic_x(0xFFFFul, 0x00FFul);                /* FFFF & ~00FF = FF00 */
    sn = sub_neg_w(10UL);                          /* 10 - (-5) = 15 */
    sp = sub_pos_w(10UL);                          /* 10 - 5 = 5 */

    /* Compose into a small integer that's stable under correct codegen. */
    return (m == 0x12345000UL)
         + (s  == 0x18UL)
         + (bw == 0xF000u)
         + (ow == 0xFFFFFF0Fu)
         + (ew == 0xFFFF0FF0u)
         + (bx == 0xFF00UL)
         + (sn == 15UL)
         + (sp == 5UL);   /* 8 if all correct */
}
