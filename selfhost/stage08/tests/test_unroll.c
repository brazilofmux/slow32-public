/* Counted for loops are unrolled when the body cannot change the trip
 * count.  Each shape here either unrolls or must not, and either way
 * must compute what the loop says, the variable's value after the loop
 * included.  run-tests.sh checks that tally8 has no branch left. */
int tally8(void) {
    int w[8];
    int i;
    int t;
    for (i = 0; i < 8; i++) w[i] = i * i + 1;
    t = 0;
    for (i = 0; i < 8; i++) t = t + w[(i + 3) % 8] * (i + 1);
    return t;
}

int after(void) {
    int i;
    int n;
    n = 0;
    for (i = 3; i <= 12; i += 3) n = n * 10 + i;
    return n * 100 + i;
}

int down(void) {
    unsigned int k;
    int acc;
    acc = 0;
    for (k = 15; k != 3; k--) acc = acc * 3 + (int)k;
    return acc + (int)k;
}

int skipper(void) {
    int i;
    int n;
    n = 0;
    for (i = 0; i < 10; i++) {
        if (i == 2) i = i + 3;
        n = n + i;
    }
    return n;
}

int leaver(int stop) {
    int i;
    int n;
    n = 0;
    for (i = 0; i < 9; i++) {
        if (i == stop) break;
        if (i % 3 == 1) continue;
        n = n + i;
    }
    return n * 10 + i;
}

int odds(void) {
    int i;
    int n;
    n = 0;
    for (i = 0; i < 7; i++) {
        if (i % 2 == 0) continue;
        n = n * 10 + i;
    }
    return n * 10 + i;
}

int firstbig(int *v) {
    int i;
    int n;
    n = -1;
    for (i = 0; i < 5; i++) {
        if (v[i] > 5) {
            n = i;
            break;
        }
    }
    return n * 10 + i;
}

int sorter(int m) {
    int i;
    int n;
    n = 0;
    for (i = 0; i < 6; i++) {
        switch ((i + m) % 3) {
        case 0: n = n + 1; break;
        case 1: n = n + 10; break;
        default: n = n + 100; break;
        }
    }
    return n;
}

int finder(int *v, int want) {
    int i;
    for (i = 0; i < 5; i++) {
        int seen;
        seen = v[i];
        if (seen == want) return i;
    }
    return -1;
}

static void poke(int *p) { *p = *p + 2; }

int aimed(void) {
    int i;
    int n;
    n = 0;
    for (i = 0; i < 8; i++) {
        n = n + 1;
        poke(&i);
    }
    return n * 100 + i;
}

int grid(void) {
    int r;
    int c;
    int g;
    g = 0;
    for (r = 0; r < 3; r++)
        for (c = 0; c < 4; c++)
            g = g + (r * 4 + c) * (c + 1);
    return g;
}

int once(void) {
    int i;
    int n;
    n = 7;
    for (i = 0; i < 1; i++) n = n * 2;
    for (i = 5; i < 5; i++) n = n + 1000;
    return n * 10 + i;
}

int main(void) {
    int v[5];
    if (tally8() != 580) return 1;
    if (after() != 370215) return 2;
    if (down() != 3852949) return 3;
    if (skipper() != 36) return 4;
    if (leaver(99) != 249) return 5;
    if (leaver(5) != 55) return 6;
    if (sorter(0) != 222 || sorter(2) != 222) return 7;
    v[0] = 4; v[1] = 9; v[2] = 2; v[3] = 9; v[4] = 6;
    if (finder(v, 9) != 1 || finder(v, 6) != 4 || finder(v, 5) != -1) return 8;
    if (aimed() != 309) return 9;
    if (odds() != 1357) return 12;
    if (firstbig(v) != 11 || firstbig(v + 2) != 11) return 13;
    if (grid() != 180) return 10;
    if (once() != 145) return 11;
    return 0;
}
