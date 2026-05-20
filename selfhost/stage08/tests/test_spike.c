int add(int a, int b) {
    return a + b;
}

int factorial(int n) {
    int result;
    result = 1;
    while (n > 1) {
        result = result * n;
        n = n - 1;
    }
    return result;
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

int test_arith(void) {
    int x;
    int y;
    x = 10;
    y = 3;
    if (x + y != 13) return 1;
    if (x - y != 7) return 2;
    if (x * y != 30) return 3;
    if (x / y != 3) return 4;
    if (x % y != 1) return 5;
    return 0;
}

int test_cmp(void) {
    if (!(5 == 5)) return 1;
    if (5 != 5) return 2;
    if (!(3 < 5)) return 3;
    if (!(5 > 3)) return 4;
    if (!(3 <= 3)) return 5;
    if (!(3 >= 3)) return 6;
    return 0;
}

int test_if(void) {
    int x;
    x = 42;
    if (x == 42) {
        x = 1;
    } else {
        x = 0;
    }
    if (x != 1) return 1;
    return 0;
}

int test_call(void) {
    int r;
    r = add(3, 4);
    if (r != 7) return 1;
    r = add(100, -50);
    if (r != 50) return 2;
    return 0;
}

int test_factorial(void) {
    if (factorial(1) != 1) return 1;
    if (factorial(5) != 120) return 2;
    if (factorial(10) != 3628800) return 3;
    return 0;
}

int test_fib(void) {
    if (fib(0) != 0) return 1;
    if (fib(1) != 1) return 2;
    if (fib(10) != 55) return 3;
    return 0;
}

int main(void) {
    int rc;
    rc = test_arith();
    if (rc) return rc;
    rc = test_cmp();
    if (rc) return rc + 10;
    rc = test_if();
    if (rc) return rc + 20;
    rc = test_call();
    if (rc) return rc + 30;
    rc = test_factorial();
    if (rc) return rc + 40;
    rc = test_fib();
    if (rc) return rc + 50;
    return 0;
}
