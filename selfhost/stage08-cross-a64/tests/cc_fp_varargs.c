/* FP varargs smoke for cc-a64 --hir (V0..V7 save + control-block va_list).
 * Exit 0 on success; 1 = one_double failed; 2 = sum_mix failed. */

typedef char *va_list;

int sum_mix(int n, ...) {
    va_list ap;
    int i;
    int si;
    double sd;
    si = 0;
    sd = 0.0;
    va_start(ap, n);
    i = 0;
    while (i < n) {
        /* alternate int, double */
        if ((i & 1) == 0) si = si + va_arg(ap, int);
        else sd = sd + va_arg(ap, double);
        i = i + 1;
    }
    va_end(ap);
    /* 10 + 20 + 1.5 + 2.5 = 34; return (int)(si + sd) */
    return (int)(si + sd);
}

int one_double(int dummy, ...) {
    va_list ap;
    double d;
    va_start(ap, dummy);
    d = va_arg(ap, double);
    va_end(ap);
    /* 3.75 → 3 */
    return (int)d;
}

int main(void) {
    if (one_double(0, 3.75) != 3) return 1;
    if (sum_mix(4, 10, 1.5, 20, 2.5) != 34) return 2;
    return 0;
}
