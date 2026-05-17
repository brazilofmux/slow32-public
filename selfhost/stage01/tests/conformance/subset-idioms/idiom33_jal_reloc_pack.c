int main(void) {
    int off;
    unsigned int inst;
    unsigned int imm20;
    unsigned int imm19_12;
    unsigned int imm11;
    unsigned int imm10_1;
    int rec;

    off = 0x5AA;
    inst = 0x00000040u;

    imm20 = ((unsigned int)off >> 20) & 1u;
    imm19_12 = ((unsigned int)off >> 12) & 0xFFu;
    imm11 = ((unsigned int)off >> 11) & 1u;
    imm10_1 = ((unsigned int)off >> 1) & 0x3FFu;

    inst = (inst & 0x00000FFFu)
         | (imm19_12 << 12)
         | (imm11 << 20)
         | (imm10_1 << 21)
         | (imm20 << 31);

    rec = 0;
    rec = rec | (((inst >> 31) & 1u) << 20);
    rec = rec | (((inst >> 21) & 0x3FFu) << 1);
    rec = rec | (((inst >> 20) & 1u) << 11);
    rec = rec | (((inst >> 12) & 0xFFu) << 12);
    if ((rec & 0x100000) != 0) rec = rec | ~0x1FFFFF;

    if (rec != off) return 1;
    return 0;
}
