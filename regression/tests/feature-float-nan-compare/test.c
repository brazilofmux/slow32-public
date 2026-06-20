// Test NaN handling in f32/f64 comparison lowering.
// Regression for CORRECTNESS-AUDIT-PASS2 findings #1-#3: the unordered
// relational predicates (ult/ule/ugt/uge), unordered-equal (ueq), and
// ordered not-equal (one) were miscompiled to bare hardware compares,
// silently returning the wrong value whenever an operand is NaN.
// FLT/FLE/FEQ all return 0 on NaN, so:
//   - an unordered predicate must be TRUE  on NaN (was returning FALSE)
//   - ordered not-equal (setone) must be FALSE on NaN (was returning TRUE)
// volatile + runtime-built NaN (0.0/0.0) forces real SETCC lowering rather
// than compile-time constant folding.
#include <stdio.h>

volatile double dN, d1;
volatile float  fN, f1;

int main() {
    volatile double z  = 0.0;  dN = z / z;   d1 = 1.0;
    volatile float  zf = 0.0f; fN = zf / zf; f1 = 1.0f;

    // ---- double (a = NaN, b = 1.0) ----
    // Finding 1: unordered relationals -> TRUE on NaN
    printf("d uge: %d\n", !(dN <  d1));            // 1
    printf("d ugt: %d\n", !(dN <= d1));            // 1
    printf("d ule: %d\n", !(dN >  d1));            // 1
    printf("d ult: %d\n", !(dN >= d1));            // 1
    // Finding 3: setone (ordered !=) -> FALSE on NaN
    printf("d one: %d\n", (dN < d1) || (dN > d1)); // 0
    // Finding 2: setueq (unordered ==) -> TRUE on NaN
    printf("d ueq: %d\n", !((dN < d1) || (dN > d1))); // 1
    // ordered / une sanity
    printf("d olt: %d\n", (dN <  d1));             // 0
    printf("d oeq: %d\n", (dN == d1));             // 0
    printf("d une: %d\n", (dN != d1));             // 1

    // ---- float ----
    printf("f uge: %d\n", !(fN <  f1));            // 1
    printf("f ugt: %d\n", !(fN <= f1));            // 1
    printf("f ule: %d\n", !(fN >  f1));            // 1
    printf("f ult: %d\n", !(fN >= f1));            // 1
    printf("f one: %d\n", (fN < f1) || (fN > f1)); // 0
    printf("f ueq: %d\n", !((fN < f1) || (fN > f1))); // 1
    printf("f olt: %d\n", (fN <  f1));             // 0
    printf("f oeq: %d\n", (fN == f1));             // 0
    printf("f une: %d\n", (fN != f1));             // 1

    // ---- non-NaN sanity: ordinary ordering still correct ----
    printf("d ord lt: %d\n", (d1 < dN));           // 0
    printf("d ord eq: %d\n", (d1 == d1));          // 1
    printf("f ord le: %d\n", (f1 <= f1));          // 1

    return 0;
}
