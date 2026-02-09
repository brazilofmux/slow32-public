/*
 * CORDIC vs Taylor Math Shootout
 *
 * Compares accuracy, performance, and code size of both implementations
 * on SLOW-32.  Functions tested: sin, cos, atan2, exp.
 *
 * Taylor functions from math_taylor.c (always Taylor, regardless of
 * the library's USE_CORDIC setting).  CORDIC functions from math_cordic.c.
 */
#include <stdio.h>
#include <math.h>

/* Taylor implementations (math_taylor.c) */
extern double taylor_sin(double);
extern double taylor_cos(double);
extern double taylor_atan2(double, double);
extern double taylor_exp(double);

/* CORDIC implementations (math_cordic.c) */
extern double cordic_sin(double);
extern double cordic_cos(double);
extern double cordic_atan2(double, double);
extern double cordic_exp(double);

static double dabs(double x) { return x < 0 ? -x : x; }

/* ---- Accuracy tests ---- */

static void test_sin(void) {
    double angles[] = {0.0, 0.5235987756, 0.7853981634, 1.0471975512,
                       1.5707963268, 2.0943951024, 3.1415926536,
                       4.7123889804, -0.7853981634, -1.0, 7.0, 100.0};
    double refs[]   = {0.0, 0.5, 0.7071067812, 0.8660254038,
                       1.0, 0.8660254038, 0.0,
                       -1.0, -0.7071067812, -0.8414709848, 0.6569865987, -0.5063656411};
    const char *names[] = {"0", "pi/6", "pi/4", "pi/3",
                           "pi/2", "2pi/3", "pi",
                           "3pi/2", "-pi/4", "-1", "7", "100"};
    int n = 12;
    double max_te = 0, max_ce = 0;

    printf("--- SIN ---\n");
    printf("%-8s %13s %13s  %9s %9s\n", "angle", "Taylor", "CORDIC", "T_err", "C_err");
    for (int i = 0; i < n; i++) {
        double ts = taylor_sin(angles[i]);
        double cs = cordic_sin(angles[i]);
        double te = dabs(ts - refs[i]);
        double ce = dabs(cs - refs[i]);
        if (te > max_te) max_te = te;
        if (ce > max_ce) max_ce = ce;
        printf("%-8s %13.10f %13.10f  %.2e %.2e\n", names[i], ts, cs, te, ce);
    }
    printf("Max err: %41.2e %.2e\n\n", max_te, max_ce);
}

static void test_cos(void) {
    double angles[] = {0.0, 0.5235987756, 0.7853981634, 1.0471975512,
                       1.5707963268, 2.0943951024, 3.1415926536,
                       4.7123889804, -0.7853981634, -1.0, 7.0, 100.0};
    double refs[]   = {1.0, 0.8660254038, 0.7071067812, 0.5,
                       0.0, -0.5, -1.0,
                       0.0, 0.7071067812, 0.5403023059, 0.7539022543, 0.8623188723};
    const char *names[] = {"0", "pi/6", "pi/4", "pi/3",
                           "pi/2", "2pi/3", "pi",
                           "3pi/2", "-pi/4", "-1", "7", "100"};
    int n = 12;
    double max_te = 0, max_ce = 0;

    printf("--- COS ---\n");
    printf("%-8s %13s %13s  %9s %9s\n", "angle", "Taylor", "CORDIC", "T_err", "C_err");
    for (int i = 0; i < n; i++) {
        double tc = taylor_cos(angles[i]);
        double cc = cordic_cos(angles[i]);
        double te = dabs(tc - refs[i]);
        double ce = dabs(cc - refs[i]);
        if (te > max_te) max_te = te;
        if (ce > max_ce) max_ce = ce;
        printf("%-8s %13.10f %13.10f  %.2e %.2e\n", names[i], tc, cc, te, ce);
    }
    printf("Max err: %41.2e %.2e\n\n", max_te, max_ce);
}

static void test_atan2(void) {
    printf("--- ATAN2 ---\n");
    printf("%-10s %13s %13s  %9s %9s\n", "args", "Taylor", "CORDIC", "T_err", "C_err");
    double max_te = 0, max_ce = 0;

    struct { const char *n; double y, x, ref; } cases[] = {
        {"(1,1)",     1.0,  1.0,  0.7853981634},
        {"(1,0)",     1.0,  0.0,  1.5707963268},
        {"(0,-1)",    0.0, -1.0,  3.1415926536},
        {"(-1,-1)",  -1.0, -1.0, -2.3561944902},
        {"(-1,1)",   -1.0,  1.0, -0.7853981634},
        {"(.1,100)",  0.1, 100.0, 0.0009999997},
        {"(100,.1)", 100.0, 0.1,  1.5697963271},
    };
    int n = 7;
    for (int i = 0; i < n; i++) {
        double ta = taylor_atan2(cases[i].y, cases[i].x);
        double ca = cordic_atan2(cases[i].y, cases[i].x);
        double te = dabs(ta - cases[i].ref);
        double ce = dabs(ca - cases[i].ref);
        if (te > max_te) max_te = te;
        if (ce > max_ce) max_ce = ce;
        printf("%-10s %13.10f %13.10f  %.2e %.2e\n", cases[i].n, ta, ca, te, ce);
    }
    printf("Max err: %43.2e %.2e\n\n", max_te, max_ce);
}

static void test_exp(void) {
    printf("--- EXP ---\n");
    printf("%-8s %16s %16s  %9s %9s\n", "input", "Taylor", "CORDIC", "T_relerr", "C_relerr");
    double max_te = 0, max_ce = 0;

    struct { const char *n; double in, ref; } cases[] = {
        {"0",     0.0,    1.0},
        {"1",     1.0,    2.7182818285},
        {"-1",   -1.0,    0.3678794412},
        {"0.5",   0.5,    1.6487212707},
        {"2",     2.0,    7.3890560989},
        {"5",     5.0,  148.4131591026},
        {"10",   10.0, 22026.4657948067},
        {"-5",   -5.0,    0.0067379470},
        {"-10", -10.0,    0.0000453999},
    };
    int n = 9;
    for (int i = 0; i < n; i++) {
        double te_v = taylor_exp(cases[i].in);
        double ce_v = cordic_exp(cases[i].in);
        double ref = cases[i].ref;
        double denom = dabs(ref) > 1e-15 ? dabs(ref) : 1e-15;
        double te = dabs(te_v - ref) / denom;
        double ce = dabs(ce_v - ref) / denom;
        if (te > max_te) max_te = te;
        if (ce > max_ce) max_ce = ce;
        printf("%-8s %16.10f %16.10f  %.2e %.2e\n", cases[i].n, te_v, ce_v, te, ce);
    }
    printf("Max rel: %46.2e %.2e\n\n", max_te, max_ce);
}

/* ---- Performance tests ---- */

#define REPS 200

static void test_perf(void) {
    volatile double sink;
    int r;

    printf("--- PERFORMANCE (%d reps each) ---\n", REPS);

    printf("sin(0.7):   Taylor...");
    for (r = 0; r < REPS; r++) sink = taylor_sin(0.7);
    printf(" done.  CORDIC...");
    for (r = 0; r < REPS; r++) sink = cordic_sin(0.7);
    printf(" done.\n");

    printf("cos(0.7):   Taylor...");
    for (r = 0; r < REPS; r++) sink = taylor_cos(0.7);
    printf(" done.  CORDIC...");
    for (r = 0; r < REPS; r++) sink = cordic_cos(0.7);
    printf(" done.\n");

    printf("atan2(1,1): Taylor...");
    for (r = 0; r < REPS; r++) sink = taylor_atan2(1.0, 1.0);
    printf(" done.  CORDIC...");
    for (r = 0; r < REPS; r++) sink = cordic_atan2(1.0, 1.0);
    printf(" done.\n");

    printf("exp(2.0):   Taylor...");
    for (r = 0; r < REPS; r++) sink = taylor_exp(2.0);
    printf(" done.  CORDIC...");
    for (r = 0; r < REPS; r++) sink = cordic_exp(2.0);
    printf(" done.\n");

    (void)sink;
}

int main() {
    printf("=== CORDIC vs Taylor Shootout ===\n\n");
    test_sin();
    test_cos();
    test_atan2();
    test_exp();
    test_perf();
    printf("\nDone.\n");
    return 0;
}
