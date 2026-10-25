/* Masks the known-bits pass may drop and the ones it must keep.  A
 * byte xor byte is already a byte (run-tests.sh checks that mixb has
 * no `andi 255` left); a byte plus a byte, a byte shifted left, and an
 * arithmetic shift of a full word are not.  Sign extension of a value
 * that fits in seven bits is a no-op; of one that needs eight it is
 * not.  Plus parameters of the narrow types, read, written and taken
 * by address. */
unsigned char mixb(unsigned char *p) {
    return (unsigned char)(p[0] ^ p[1]);
}

unsigned char sumb(unsigned char *p) {
    return (unsigned char)(p[0] + p[1]);
}

unsigned short sumh(unsigned char *p) {
    return (unsigned short)(p[0] + p[1] + p[2]);
}

unsigned char dbl(unsigned char *p) {
    return (unsigned char)(p[0] << 1);
}

int low7(unsigned char *p) {
    return (signed char)(p[0] & 0x7f);
}

int full8(unsigned char *p) {
    return (signed char)p[0];
}

unsigned char topu(unsigned int w) {
    return (unsigned char)(w >> 24);
}

unsigned int tops(unsigned int w) {
    return (unsigned char)((int)w >> 20);
}

unsigned char foldx(unsigned char *p, int n) {
    unsigned int acc;
    int i;
    acc = 0;
    for (i = 0; i < n; i++) acc = acc ^ p[i];
    return (unsigned char)acc;
}

unsigned char foldm(unsigned char *p, int n) {
    unsigned int acc;
    int i;
    acc = 1;
    for (i = 0; i < n; i++) acc = (acc << 3) + p[i];
    return (unsigned char)acc;
}

unsigned char shr4(int n) {
    unsigned int acc;
    acc = 0xf00;
    while (n > 0) {
        acc = acc >> 4;
        n = n - 1;
    }
    return (unsigned char)acc;
}

unsigned char nextc(unsigned char c) {
    unsigned char *q;
    q = &c;
    *q = (unsigned char)(*q + 9);
    return c;
}

int widen(signed char sc, unsigned short us, short ss) {
    return sc * 100000 + us - ss;
}

int main(void) {
    unsigned char b[6];

    b[0] = 0xc3; b[1] = 0x5e; b[2] = 0xff; b[3] = 0x80; b[4] = 0x01; b[5] = 0x7f;
    if (mixb(b) != 0x9d) return 1;
    if (sumb(b) != 0x21) return 2;
    if (sumh(b) != 0x220) return 3;
    if (dbl(b) != 0x86) return 4;
    if (low7(b) != 0x43) return 5;
    if (full8(b) != -61) return 6;
    if (low7(b + 5) != 127 || full8(b + 3) != -128) return 7;
    if (topu(0xab12cd34u) != 0xab) return 8;
    if (tops(0x9e7a0000u) != 0xe7) return 9;
    if (foldx(b, 6) != (0xc3 ^ 0x5e ^ 0xff ^ 0x80 ^ 0x01 ^ 0x7f)) return 10;
    if (foldm(b, 4) != (unsigned char)(((((1u << 3) + 0xc3) << 3) + 0x5e) * 64 + 0xff * 8 + 0x80))
        return 11;
    if (shr4(0) != 0 || shr4(1) != 0xf0 || shr4(2) != 0x0f) return 15;
    if (nextc(250) != 3) return 12;
    if (nextc(17) != 26) return 13;
    if (widen(-3, 65535, -2) != -300000 + 65535 + 2) return 14;
    return 0;
}
