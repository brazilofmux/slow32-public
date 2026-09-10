/* GitHub issue 69: an indirect call returning double was typed as int,
 * so the lo word of the r1:r2 pair was converted with fcvt.d.w.
 * Direct calls already emitted CALLHI; CALLP did not.
 *
 * Returns the index of the first failing check, 0 if all pass. */

double acc;

double sink(double x) {
    acc = acc + x;
    return x * 0.5;
}

long long lsink(long long x) {
    return x + 1;
}

double indirect(double x, double (*fn)(double)) {
    double y = x + 1.0;
    double r = fn(y);
    return r + y * 4.0;
}

long long indirect_ll(long long x, long long (*fn)(long long)) {
    long long r = fn(x);
    return r + x;
}

typedef double (*dfn_t)(double);

dfn_t gtab[2];

double via_typedef(double x, dfn_t fn) {
    return fn(x) + x;
}

double via_local(double x) {
    double (*fn)(double);
    fn = sink;
    return fn(x) + x;
}

/* GitHub issue 70: the signature has to survive a '*' or a '[]'.
 * A LOCAL array lost what the global kept (add_local_array did not
 * propagate it), and `dfn_t *tab` dropped it at the star, so tab[0](x)
 * and (*tab)(x) both fell back to the TY_INT default and skipped
 * CALLHI -- issue 69's failure in the shapes its fix did not reach. */
double via_local_array(double x) {
    dfn_t t[2];
    t[0] = sink; t[1] = sink;
    return t[0](x) + x;
}

double via_global_array(double x) { return gtab[0](x) + x; }
double via_ptr_index(dfn_t *tab, double x) { return tab[0](x) + x; }
double via_ptr_deref(dfn_t *tab, double x) { return (*tab)(x) + x; }

int main(void) {
    acc = 0.0;
    /* sink(2.5) + 2.5*4.0 = 1.25 + 10.0 = 11.25 */
    if (indirect(1.5, sink) != 11.25) return 1;
    if (acc != 2.5) return 2;

    acc = 0.0;
    if (via_typedef(4.0, sink) != 6.0) return 3;

    acc = 0.0;
    if (via_local(4.0) != 6.0) return 4;

    if (indirect_ll(10, lsink) != 21) return 5;

    {
        dfn_t t[2];
        t[0] = sink; t[1] = sink;
        gtab[0] = sink; gtab[1] = sink;

        acc = 0.0;
        if (via_local_array(4.0) != 6.0) return 6;
        acc = 0.0;
        if (via_global_array(4.0) != 6.0) return 7;
        acc = 0.0;
        if (via_ptr_index(t, 4.0) != 6.0) return 8;
        acc = 0.0;
        if (via_ptr_deref(t, 4.0) != 6.0) return 9;
    }
    return 0;
}
