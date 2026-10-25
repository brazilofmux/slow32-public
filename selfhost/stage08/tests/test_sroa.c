/* A small local array whose every index is a constant once the
 * optimizer is done is split into one value per element and kept in
 * registers (ssa_sroa_late); a small memcpy to or from one is element
 * copies.  Arrays indexed at run time, passed to a callee, punned or
 * overlapping stay in memory.  run-tests.sh checks that spin keeps no
 * array in its frame. */
#include <string.h>

void spin(const unsigned char *in, unsigned char *out) {
    unsigned char st[8], tmp[8];
    int r, i;
    for (i = 0; i < 8; i++) st[i] = in[i];
    for (r = 0; r < 5; r++) {
        for (i = 0; i < 8; i++) tmp[i] = (unsigned char)(st[i] * 3 + r);
        for (i = 0; i < 8; i++) st[i] = tmp[(i + 3) % 8];
        if (r & 1) {
            for (i = 0; i < 8; i += 2) {
                unsigned char a = st[i];
                st[i] = st[i + 1];
                st[i + 1] = (unsigned char)(a ^ 0x5a);
            }
        }
    }
    memcpy(out, st, 8);
}

/* Signed elements keep their sign through the split. */
int signs(int k) {
    signed char v[4];
    short w[3];
    int i;
    for (i = 0; i < 4; i++) v[i] = (signed char)(k - 60 * i);
    for (i = 0; i < 3; i++) w[i] = (short)(k * 1000 - 40000 * i);
    return v[0] + v[1] + v[2] + v[3] + w[0] + w[1] + w[2];
}

/* An element merged across a loop and an if. */
int merge(int n) {
    int acc[3];
    int i;
    for (i = 0; i < 3; i++) acc[i] = i + 1;
    while (n > 0) {
        if (n & 1) acc[0] = acc[0] + acc[2];
        else acc[1] = acc[1] * 2 - acc[0];
        acc[2] = acc[2] + 1;
        n = n - 1;
    }
    return acc[0] * 100 + acc[1] * 10 + acc[2];
}

/* Stays in memory: a run-time index. */
int runtime_index(int j) {
    int t[4];
    t[0] = 7; t[1] = 11; t[2] = 13; t[3] = 17;
    return t[j & 3] + t[0];
}

static int total(const int *p, int n) {
    int s = 0;
    while (n-- > 0) s += *p++;
    return s;
}

/* Stays in memory: the address goes to a callee. */
int handed(int a) {
    int t[3];
    t[0] = a; t[1] = a * 4; t[2] = a - 9;
    return total(t, 3);
}

/* Stays in memory: a word written, its bytes read back. */
int punned(void) {
    union { unsigned u; unsigned char b[4]; } x;
    x.u = 0x0a0b0c0dU;
    return x.b[0] + x.b[3];
}

/* memcpy into a local array from memory, then element reads. */
int copied_in(const unsigned char *src) {
    unsigned char t[6];
    memcpy(t, src, 6);
    return t[0] * 1 + t[2] * 3 + t[5] * 5;
}

struct pt { int x, y; };

/* A small struct's fields split like an array's. */
int fields(int a, int b) {
    struct pt p;
    p.x = a;
    p.y = b;
    p.x = p.x + p.y;
    return p.x * 2 - p.y;
}

int main(void) {
    unsigned char in[8], out[8];
    unsigned char raw[6];
    int i, sum;
    for (i = 0; i < 8; i++) in[i] = (unsigned char)(i * 29 + 3);
    spin(in, out);
    sum = 0;
    for (i = 0; i < 8; i++) sum = sum * 7 + out[i];
    if (sum != 83888308) return 1;
    if (signs(50) != -35440) return 2;
    if (merge(7) != 2170) return 3;
    if (runtime_index(6) != 20) return 4;
    if (handed(5) != 21) return 5;
    if (punned() != 23) return 6;
    for (i = 0; i < 6; i++) raw[i] = (unsigned char)(200 + i * 9);
    if (copied_in(raw) != 200 + 218 * 3 + 245 * 5) return 7;
    if (fields(6, 4) != 16) return 8;
    return 0;
}
