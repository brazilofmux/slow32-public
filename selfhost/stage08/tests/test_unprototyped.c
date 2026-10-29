/* An empty parameter list declares no prototype (C11 6.7.6.3p14): a call
 * may pass arguments, which are promoted (6.5.2.2p6).  stage08 took
 * `int f();` for `int f(void)` and refused the call -- the cross libcs'
 * `long __syscall();`, called with seven arguments, stopped building. */
int add();
long sum3();
static int none() { return 5; }

int main(void) {
    char c = 7;
    if (add(3, 4) != 7) return 1;
    if (sum3(c, (short)-2, 10) != 15) return 2;   /* char, short promote to int */
    if (none() != 5) return 3;
    return 0;
}

int add(a, b) int a, b; { return a + b; }
long sum3(a, b, c) int a, b, c; { return a + b + c; }
