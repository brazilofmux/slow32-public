/* One function past the fixed tables lowering had: more than 1024 case
 * labels in a switch (HL_MAX_CASE), 4096 goto labels (HL_MAX_GOTO,
 * P_MAX_LABELS) and 32768 call arguments (HIR_MAX_CARG, most of whose
 * writers had no bound).  They grow now (tables.h). */
static int add4(int a, int b, int c, int d) { return a + b + c + d; }

#define C1(n) case n: return n + 1;
#define C10(n) C1(n##0) C1(n##1) C1(n##2) C1(n##3) C1(n##4) C1(n##5) C1(n##6) C1(n##7) C1(n##8) C1(n##9)
#define C100(n) C10(n##0) C10(n##1) C10(n##2) C10(n##3) C10(n##4) C10(n##5) C10(n##6) C10(n##7) C10(n##8) C10(n##9)

static int sw(int k) {
    switch (k) {
    C100(1) C100(2) C100(3) C100(4) C100(5) C100(6) C100(7) C100(8) C100(9)
    C100(10) C100(11)
    default: return -1;
    }
}

#define L1(n) l##n: s = s + 1; if (s > limit) goto out;
#define L10(n) L1(n##0) L1(n##1) L1(n##2) L1(n##3) L1(n##4) L1(n##5) L1(n##6) L1(n##7) L1(n##8) L1(n##9)
#define L100(n) L10(n##0) L10(n##1) L10(n##2) L10(n##3) L10(n##4) L10(n##5) L10(n##6) L10(n##7) L10(n##8) L10(n##9)
#define L1000(n) L100(n##0) L100(n##1) L100(n##2) L100(n##3) L100(n##4) L100(n##5) L100(n##6) L100(n##7) L100(n##8) L100(n##9)

static int labels(int limit) {
    int s = 0;
    goto l1000;
    L1000(1) L1000(2) L1000(3) L1000(4) L100(50) L100(51)
out:
    return s;
}

#define A1(n) t = t + add4(n, 1, 2, 3);
#define A10(n) A1(n##0) A1(n##1) A1(n##2) A1(n##3) A1(n##4) A1(n##5) A1(n##6) A1(n##7) A1(n##8) A1(n##9)
#define A100(n) A10(n##0) A10(n##1) A10(n##2) A10(n##3) A10(n##4) A10(n##5) A10(n##6) A10(n##7) A10(n##8) A10(n##9)
#define A1000(n) A100(n##0) A100(n##1) A100(n##2) A100(n##3) A100(n##4) A100(n##5) A100(n##6) A100(n##7) A100(n##8) A100(n##9)

static int calls(void) {
    int t = 0;
    A1000(1) A1000(2) A1000(3) A1000(4) A1000(5) A1000(6) A1000(7) A1000(8) A1000(9)
    return t;
}

int main(void) {
    long long want;
    int i;
    if (sw(100) != 101 || sw(1199) != 1200 || sw(5) != -1) return 1;
    if (labels(1000000) != 4200) return 2;
    if (labels(10) != 11) return 3;
    /* sum over n of (n + 6), n = 1000..9999 */
    want = 0;
    for (i = 1000; i <= 9999; i++) want = want + i + 6;
    if (calls() != (int)want) return 4;
    return 0;
}
