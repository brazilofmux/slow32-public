/* A counted loop whose body tests the variable one way on every pass
 * but the last is lowered as the loop for those passes, the test
 * decided, then the last pass on its own.  Each shape here either peels
 * or must not, and either way must compute what the loop says, the
 * variable's value after the loop included.  run-tests.sh checks that
 * rounds' loop keeps one conditional branch (its own test). */
int rounds(void) {
    unsigned char s[4];
    int r;
    int i;
    int t;
    for (i = 0; i < 4; i++) s[i] = (unsigned char)(i * 37 + 5);
    for (r = 1; r <= 14; r++) {
        for (i = 0; i < 4; i++) s[i] = (unsigned char)(s[i] * 3 + 1);
        if (r != 14) {
            unsigned char a0 = s[0], a1 = s[1], a2 = s[2], a3 = s[3];
            s[0] = (unsigned char)(a0 ^ a1);
            s[1] = (unsigned char)(a1 ^ a2);
            s[2] = (unsigned char)(a2 ^ a3);
            s[3] = (unsigned char)(a3 ^ a0 ^ 0x1b);
        }
        for (i = 0; i < 4; i++) s[i] = (unsigned char)(s[i] ^ (r * 16 + i));
    }
    t = 0;
    for (i = 0; i < 4; i++) t = t * 256 + s[i];
    return t + r;
}

/* == on the last value, with an else */
int eqlast(void) {
    int k;
    int t;
    t = 0;
    for (k = 0; k < 10; k++) {
        if (k == 9) t = t * 2 + 100;
        else t = t + k;
    }
    return t * 100 + k;
}

/* counting down by four, the variable on the right of the test */
int down(void) {
    int k;
    int a;
    a = 1;
    for (k = 20; k > 0; k -= 4) {
        if (4 != k) a = a * 3 + k;
        else a = a - 7;
    }
    return a * 10 + k;
}

/* < decided on all passes but the last; an unsigned variable */
int lessthan(void) {
    unsigned int i;
    int t;
    t = 0;
    for (i = 0; i < 10; i++) {
        if (i < 9) t = t + (int)i;
        else t = t * 3;
    }
    return t * 100 + (int)i;
}

/* the body keeps a loop of its own, and two decided tests */
int nested(int n) {
    int i;
    int j;
    int t;
    t = 0;
    for (i = 1; i <= 6; i++) {
        j = 0;
        while (j < n) { t = t + i * j; j = j + 1; }
        if (i != 6) t = t + 1000;
        if (i == 6) t = t * 2;
    }
    return t;
}

/* the last pass returns: no step after it */
int leaves(int x) {
    int i;
    for (i = 0; i < 6; i++) {
        x = x * 2 + i;
        if (i == 5) return x + 1000;
    }
    return -1;
}

/* not peeled: a break of its own (the last pass need not come) */
int breaker(int lim) {
    int i;
    int t;
    t = 0;
    for (i = 0; i < 8; i++) {
        if (t > lim) break;
        t = t + i;
        if (i != 7) t = t + 1;
    }
    return t * 10 + i;
}

/* a continue in the decided arm: not peeled either */
int skipper(void) {
    int i;
    int t;
    t = 0;
    for (i = 0; i < 7; i++) {
        if (i != 6) { t = t + 2; continue; }
        t = t * 5;
    }
    return t * 10 + i;
}

/* a test the variable decides on more than the last pass is left alone */
int notlast(void) {
    int i;
    int t;
    t = 0;
    for (i = 0; i < 8; i++) {
        if (i != 3) t = t + i;
        else t = t * 2;
        if (i != 7) t = t + 1;
    }
    return t * 10 + i;
}

int main(void) {
    if (rounds() != 1295532659) return 1;
    if (eqlast() != 17210) return 2;
    if (down() != 8020) return 3;
    if (lessthan() != 10810) return 4;
    if (nested(3) != 10126 || nested(0) != 10000) return 5;
    if (leaves(1) != 1121) return 6;
    if (breaker(100) != 358 || breaker(5) != 63) return 7;
    if (skipper() != 607) return 8;
    if (notlast() != 388) return 9;
    return 0;
}
