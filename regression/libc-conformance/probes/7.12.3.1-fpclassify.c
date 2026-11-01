/* 7.12.3.1 fpclassify: each class, judged in the argument's own type (float subnormal, double normal) */
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

static const char *cls(int c) {
    return c == FP_NAN ? "nan" : c == FP_INFINITE ? "inf" : c == FP_NORMAL ? "normal"
         : c == FP_SUBNORMAL ? "subnormal" : c == FP_ZERO ? "zero" : "?";
}

int main(void) {
    volatile double d0 = 0.0, d1 = -1.5, ds = 1e-310;
    volatile float f0 = -0.0f, f1 = 3.0f, fs = 1e-40f;
    double inf = strtod("inf", 0), nan = strtod("nan", 0);
    printf("%s %s %s %s %s\n", cls(fpclassify(d0)), cls(fpclassify(d1)), cls(fpclassify(ds)),
           cls(fpclassify(inf)), cls(fpclassify(nan)));
    printf("%s %s %s %s\n", cls(fpclassify(f0)), cls(fpclassify(f1)), cls(fpclassify(fs)),
           cls(fpclassify((float)inf)));
    return 0;
}
