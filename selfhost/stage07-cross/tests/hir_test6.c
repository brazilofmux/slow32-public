/* Test division, modulo, ternary, compound expressions */

int gcd(int a, int b) {
    while (b != 0) {
        int t;
        t = b;
        b = a % b;
        a = t;
    }
    return a;
}

int fib(int n) {
    int a;
    int b;
    int t;
    a = 0;
    b = 1;
    while (n > 0) {
        t = a + b;
        a = b;
        b = t;
        n = n - 1;
    }
    return a;
}

int abs_val(int x) {
    if (x < 0) return 0 - x;
    return x;
}

int main() {
    int r;
    r = 0;
    r = r + gcd(48, 18);      /* 6 */
    r = r + fib(10);           /* 6 + 55 = 61 */
    r = r + abs_val(-37);      /* 61 + 37 = 98 */
    r = r + (100 / 7);        /* 98 + 14 = 112 */
    r = r + (100 % 7);        /* 112 + 2 = 114 */
    return r;
}
