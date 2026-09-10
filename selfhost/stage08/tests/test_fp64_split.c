/* GitHub issue 67 -- live-range splitting at calls, fp64 half.
 *
 * A double that is BOTH a call argument and live after the call gets its
 * live range split (V' = COPY V before the call).  A double is two HIR
 * values held as a register pair, so ra_split_one recursed onto the pair
 * partner -- and the partner's ra_pair_of[] still named the original, so
 * it bounced straight back and split it again.  The recursion stopped
 * only when the instruction table ran out: every function below used to
 * compile to ~1.3M lines of assembly (262144 copies, exactly
 * HIR_MAX_INST) and the assembler died with "Instruction buffer size
 * overflow".  In SQLite it took out dekkerMul2, kahanBabuskaNeumaierStep,
 * absFunc and strftimeFunc.
 *
 * The whole small suite stayed green through it: the shape needs a
 * double specifically, and nothing in the suite passed one that was
 * still live after the call.
 *
 * The same shape through a function pointer is deliberately NOT tested
 * here: an indirect call returning a double is separately miscompiled
 * (the return value is run through fcvt.d.w as if it were an int), which
 * reproduces with no split in sight and on the pre-#67 kit compiler.
 * See GitHub issue 69.
 *
 * Returns the index of the first failing block, 0 if all pass. */

double acc;

double sink(double x) {
    acc = acc + x;
    return x * 0.5;
}

double diff2(double a, double b) {
    acc = acc + a - b;
    return a - b;
}

/* v is an argument and live after -> the pair split fires */
double step(double v, double s) {
    double t = v * s;
    double r = sink(t);
    return r + t * 2.0;
}

/* two fp64 arguments, both surviving the call */
double two(double a, double b) {
    double r = diff2(a, b);
    double q = sink(a);
    return r + a * 3.0 + b * 5.0 + q;
}

/* fp64 live across a call inside a loop (survives round the back edge) */
double loopy(double x, int n) {
    double s = 0.0;
    int i = 0;
    while (i < n) {
        s = s + sink(x) + x;
        i = i + 1;
    }
    return s + x;
}

int main(void) {
    acc = 0.0;
    if (step(3.0, 4.0) != 30.0) return 1;

    acc = 0.0;
    if (two(7.25, 2.5) != 42.625) return 2;

    acc = 0.0;
    if (loopy(1.5, 4) != 10.5) return 3;

    /* the side effects prove the arguments really reached the callees
     * with the values the caller thought it was passing */
    acc = 0.0;
    step(3.0, 4.0);
    if (acc != 12.0) return 5;

    acc = 0.0;
    two(7.25, 2.5);
    if (acc != 12.0) return 6;

    return 0;
}
