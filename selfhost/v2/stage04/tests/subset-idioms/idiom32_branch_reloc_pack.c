int main(void) {
    int off;
    unsigned int inst;
    unsigned int imm12;
    unsigned int imm11;
    unsigned int imm10_5;
    unsigned int imm4_1;
    int rec;

    off = -18; /* even and in range */
    inst = 0x00000048u;

    imm12 = ((unsigned int)off >> 12) & 1u;
    imm11 = ((unsigned int)off >> 11) & 1u;
    imm10_5 = ((unsigned int)off >> 5) & 0x3Fu;
    imm4_1 = ((unsigned int)off >> 1) & 0xFu;

    inst = (inst & 0x01FFF07Fu)
         | (imm11 << 7)
         | (imm4_1 << 8)
         | (imm10_5 << 25)
         | (imm12 << 31);

    rec = 0;
    rec = rec | (((inst >> 31) & 1u) << 12);
    rec = rec | (((inst >> 25) & 0x3Fu) << 5);
    rec = rec | (((inst >> 8) & 0xFu) << 1);
    rec = rec | (((inst >> 7) & 1u) << 11);
    if ((rec & 0x1000) != 0) rec = rec | ~0x1FFF;

    if (rec != off) return 1;
    return 0;
}
