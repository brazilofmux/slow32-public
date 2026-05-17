static unsigned r32(const unsigned char *p) {
    unsigned v = 0;
    v |= (unsigned)p[0] & 255u;
    v |= ((unsigned)p[1] & 255u) << 8;
    v |= ((unsigned)p[2] & 255u) << 16;
    v |= ((unsigned)p[3] & 255u) << 24;
    return v;
}

int main(void) {
    unsigned char h[16];
    h[0] = 'A'; h[1] = '2'; h[2] = '3'; h[3] = 'S';
    h[8] = 2; h[9] = 0; h[10] = 0; h[11] = 0;
    h[12] = 32; h[13] = 0; h[14] = 0; h[15] = 0;
    if (h[0] != 'A' || h[1] != '2' || h[2] != '3' || h[3] != 'S') return 1;
    if (r32(h + 8) != 2u) return 2;
    if (r32(h + 12) != 32u) return 3;
    return 0;
}
