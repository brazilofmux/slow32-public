int main(void) {
    unsigned int value;
    unsigned int lo12;
    unsigned int hi20;
    unsigned int recon;

    value = 0x00123ABCu;
    lo12 = value & 0xFFFu;
    hi20 = (value >> 12) & 0xFFFFFu;
    if ((lo12 & 0x800u) != 0u) {
        hi20 = (hi20 + 1u) & 0xFFFFFu;
    }
    recon = (hi20 << 12) + ((int)(lo12 << 20) >> 20);
    if ((recon & 0xFFFFFFFFu) != value) return 1;

    value = 0x000127FFu;
    lo12 = value & 0xFFFu;
    hi20 = (value >> 12) & 0xFFFFFu;
    if ((lo12 & 0x800u) != 0u) {
        hi20 = (hi20 + 1u) & 0xFFFFFu;
    }
    recon = (hi20 << 12) + ((int)(lo12 << 20) >> 20);
    if ((recon & 0xFFFFFFFFu) != value) return 2;

    return 0;
}
