// Regression for CORRECTNESS-AUDIT-PASS2 findings 1-3: FP comparison
// lowering must honor ordered vs unordered predicate semantics on NaN.
//   ult/ule/ugt/uge (via negated ordered compares) must be TRUE on NaN,
//   ueq (via !islessgreater) must be TRUE on NaN,
//   one (via islessgreater) must be FALSE on NaN.
#include <stdio.h>

static void row(const char *name, double x, double y) {
    printf("%s lt=%d le=%d gt=%d ge=%d eq=%d ne=%d ult=%d ule=%d ugt=%d uge=%d one=%d ueq=%d uno=%d\n",
           name,
           x < y, x <= y, x > y, x >= y, x == y, x != y,
           !(x >= y), !(x > y), !(x <= y), !(x < y),
           __builtin_islessgreater(x, y), !__builtin_islessgreater(x, y),
           __builtin_isunordered(x, y));
}

static void rowf(const char *name, float x, float y) {
    printf("%s lt=%d le=%d gt=%d ge=%d eq=%d ne=%d ult=%d ule=%d ugt=%d uge=%d one=%d ueq=%d uno=%d\n",
           name,
           x < y, x <= y, x > y, x >= y, x == y, x != y,
           !(x >= y), !(x > y), !(x <= y), !(x < y),
           __builtin_islessgreater(x, y), !__builtin_islessgreater(x, y),
           __builtin_isunordered(x, y));
}

int main(void) {
    // volatile zeros so the NaNs are produced at runtime, not constant-folded
    volatile double zd = 0.0;
    double nan = zd / zd;
    volatile float zf = 0.0f;
    float nanf = zf / zf;

    row("d nan,1  ", nan, 1.0);
    row("d 1,nan  ", 1.0, nan);
    row("d nan,nan", nan, nan);
    row("d 1,2    ", 1.0, 2.0);
    row("d 2,2    ", 2.0, 2.0);
    rowf("f nan,1  ", nanf, 1.0f);
    rowf("f 1,nan  ", 1.0f, nanf);
    rowf("f nan,nan", nanf, nanf);
    rowf("f 1,2    ", 1.0f, 2.0f);
    rowf("f 2,2    ", 2.0f, 2.0f);
    return 0;
}
