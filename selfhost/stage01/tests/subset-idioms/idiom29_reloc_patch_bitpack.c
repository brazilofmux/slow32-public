int main(void) {
    unsigned int inst;
    unsigned int imm;
    unsigned int patched;
    unsigned int recovered;

    /* S-type immediate patch pattern used by relocations. */
    inst = 0x0000003AU;
    imm = 0xABCU;

    patched = (inst & 0xFE000F80U) | ((imm & 0x1FU) << 7) | (((imm >> 5) & 0x7FU) << 25);

    recovered = ((patched >> 7) & 0x1FU) | (((patched >> 25) & 0x7FU) << 5);
    if ((recovered & 0xFFFU) != (imm & 0xFFFU)) return 1;

    /* I-type immediate patch pattern used by relocations. */
    inst = 0x00000010U;
    imm = 0xFEDU;
    patched = (inst & 0x000FFFFFU) | ((imm & 0xFFFU) << 20);
    recovered = (patched >> 20) & 0xFFFU;
    if (recovered != (imm & 0xFFFU)) return 2;

    return 0;
}
