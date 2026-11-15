/* A compiler temporary lives no longer than its statement, and the
 * lowering gives a statement's temporaries back when it ends.  They
 * were taken from one count that only grew: a function of many
 * statements each calling something that returns a structure took a
 * slot per call, and a 4,600-line program xbc generated asked for a
 * 1.5 MB frame and ran off its 1 MB stack.  many() below took 16 slots
 * of 256 bytes, a 4 KB frame; run-tests.sh holds it under 1 KB.
 *
 * Once the room is shared, two splices of inlined statics in different
 * statements can land at the same place (tripled and scaled below), and
 * csq's complex temporaries, in two statements, share room too: the
 * answers must not change. */
#include <complex.h>

typedef struct { int v[64]; } big_t;

big_t mk(int k) {
    big_t b;
    int i;
    for (i = 0; i < 64; i++) b.v[i] = k * 100 + i;
    return b;
}

int many(int k) {
    int r;
    r = 0;
    r += mk(k + 0).v[k];   r += mk(k + 1).v[k];
    r += mk(k + 2).v[k];   r += mk(k + 3).v[k];
    r += mk(k + 4).v[k];   r += mk(k + 5).v[k];
    r += mk(k + 6).v[k];   r += mk(k + 7).v[k];
    r += mk(k + 8).v[k];   r += mk(k + 9).v[k];
    r += mk(k + 10).v[k];  r += mk(k + 11).v[k];
    r += mk(k + 12).v[k];  r += mk(k + 13).v[k];
    r += mk(k + 14).v[k];  r += mk(k + 15).v[k];
    return r;
}

/* two values in flight in one statement keep their own slots */
int both(int k) {
    return mk(k).v[1] * 1000 + mk(k + 1).v[2];
}

static int tripled(int x) {
    int a;
    a = x * 3;
    return a + 1;
}

static float scaled(float y) {
    float b;
    b = y * 2.5f;
    return b - 0.5f;
}

static double csq(double re, double im) {
    double complex z;
    z = re + im * I;
    z = z * 2.0 + z;   /* real scaling: no __muldc3 in the test's link */
    return creal(z) * 10 + cimag(z);
}

int main(void) {
    int i;
    int t;
    double d;
    if (many(2) != 16 * 2 + 100 * (16 * 2 + 120)) return 1;
    if (both(3) != 301 * 1000 + 402) return 2;
    i = 4;
    t = tripled(i);
    d = scaled(i);
    t = t * 100 + tripled(i + 1);
    if (t != 1316) return 3;
    if (d != 9.5) return 4;
    d = csq(1.0, 2.0);
    t = tripled(2);
    d = d * 100 + csq(3.0, 1.0);
    if (d != 3600 + 93) return 5;
    if (t != 7) return 6;
    return 0;
}
