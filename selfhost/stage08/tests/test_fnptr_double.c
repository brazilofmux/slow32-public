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

double via_typedef(double x, dfn_t fn) {
    return fn(x) + x;
}

double via_local(double x) {
    double (*fn)(double);
    fn = sink;
    return fn(x) + x;
}

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
    return 0;
}
