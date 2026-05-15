/* Pointer arithmetic, string-ish operations, byte/word mixing.
 * Catches narrow-load + RMW patterns that broke earlier (#49 Bug D). */
static unsigned int hash(unsigned char *s, int n) {
    unsigned int h = 5381;
    int i;
    for (i = 0; i < n; i++) {
        h = h * 33u + s[i];
    }
    return h;
}

static void scramble(unsigned char *buf, int n) {
    int i;
    for (i = 0; i < n; i++) {
        buf[i] = (unsigned char)(buf[i] * 7 + (i & 0xff));
    }
    for (i = 0; i + 3 < n; i += 4) {
        unsigned char t = buf[i];
        buf[i]     = buf[i + 3];
        buf[i + 3] = t;
        t = buf[i + 1];
        buf[i + 1] = buf[i + 2];
        buf[i + 2] = t;
    }
}

int main(void) {
    unsigned char buf[64];
    int i;
    for (i = 0; i < 64; i++) buf[i] = (unsigned char)((i * 17 + 3) & 0xff);
    scramble(buf, 64);
    unsigned int h = hash(buf, 64);
    return h & 0xff;
}
