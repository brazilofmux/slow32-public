static unsigned int rd32(const unsigned char *p) {
    return (unsigned int)p[0]
         | ((unsigned int)p[1] << 8)
         | ((unsigned int)p[2] << 16)
         | ((unsigned int)p[3] << 24);
}

static int rd_i32(const unsigned char *p) {
    return (int)rd32(p);
}

static int jal_window_ok(unsigned int patched, unsigned int pc) {
    int off;
    unsigned int off_u;
    off = (int)(patched - pc);
    off_u = (unsigned int)off;
    if ((off_u & 1u) != 0u) return 0;
    if ((off_u + 1048576u) > 2097150u) return 0;
    return 1;
}

int main(void) {
    unsigned char rel[16];
    unsigned int sym_abs;
    unsigned int patched;
    int addend;

    rel[0] = 0;
    rel[1] = 0;
    rel[2] = 0;
    rel[3] = 0;
    rel[4] = 0;
    rel[5] = 0;
    rel[6] = 0;
    rel[7] = 0;
    rel[8] = 0;
    rel[9] = 0;
    rel[10] = 0;
    rel[11] = 0;
    rel[12] = 0xFC; /* addend = -4 */
    rel[13] = 0xFF;
    rel[14] = 0xFF;
    rel[15] = 0xFF;

    addend = rd_i32(rel + 12);
    if (addend != -4) return 1;

    sym_abs = 0x1000u;
    patched = sym_abs + (unsigned int)addend;
    if (patched != 0x0FFCu) return 1;
    if (!jal_window_ok(patched, 0x0800u)) return 1;
    if (jal_window_ok(0u, 0x300000u)) return 1;
    return 0;
}
