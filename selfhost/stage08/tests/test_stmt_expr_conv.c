/* A GNU statement expression's statements had no semantic pass: every
 * conversion sema makes was missing inside ({ ... }).  A call's int
 * argument reached a double parameter as the integer bits, and an
 * assignment's right side was not converted -- the code s32-xbc writes
 * for every Clipper expression, ({ ...; t = val_num_dec(3, 0, 0); ... }),
 * handed the runtime 0 for 3. */
struct big { int a[6]; };

/* defined below main and not static: called, not inlined; fabs, from
 * the library, is known only by its prototype */
double tenfold(double d, int a);
struct big mk(double d, int a);
double fabs(double x);

int main(void) {
    struct big x;
    double d;
    long long w;
    int e;
    ({ x = mk(3, 4); 0; });
    if (x.a[0] != 30 || x.a[1] != 4) return 1;
    d = ({ double t; t = tenfold(3, 1); t; });
    if (d != 31) return 2;
    e = ({ int k = 0; d = 5; k = k || d > 4; k; });     /* assignment converts */
    if (!e || d != 5) return 3;
    w = ({ long long q; q = 1; q <<= 40; q; });
    if (w != 1099511627776LL) return 4;
    if (({ struct big t; t = mk(2.5, 7); t.a[0]; }) != 25) return 5;
    d = ({ double t; t = fabs(-3); t; });
    if (d != 3) return 6;
    ({ d = fabs(-7) + fabs(2); 0; });
    if (d != 9) return 7;
    return 0;
}

double tenfold(double d, int a) { return d * 10 + a; }

struct big mk(double d, int a) {
    struct big r;
    int i;
    for (i = 0; i < 6; i++) r.a[i] = 0;
    r.a[0] = (int)(d * 10);
    r.a[1] = a;
    return r;
}
