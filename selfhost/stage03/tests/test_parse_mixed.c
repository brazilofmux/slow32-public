/* Integration test: multi-function programs with real C patterns */

/* Forward declarations */
int is_prime(int n);
int fibonacci(int n);

/* Global array */
int g_results[8];

int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

int is_prime(int n) {
    int i;
    if (n < 2) return 0;
    i = 2;
    while (i * i <= n) {
        if (n % i == 0) return 0;
        i = i + 1;
    }
    return 1;
}

int fibonacci(int n) {
    int a;
    int b;
    int t;
    int i;
    if (n <= 0) return 0;
    if (n == 1) return 1;
    a = 0;
    b = 1;
    i = 2;
    while (i <= n) {
        t = a + b;
        a = b;
        b = t;
        i = i + 1;
    }
    return b;
}

int sum_array(int *arr, int len) {
    int i;
    int s;
    s = 0;
    for (i = 0; i < len; i = i + 1) {
        s = s + arr[i];
    }
    return s;
}

int strlen_manual(char *s) {
    int n;
    n = 0;
    while (s[n] != 0) n = n + 1;
    return n;
}

int test_factorial(void) {
    if (factorial(0) != 1) return 1;
    if (factorial(1) != 1) return 2;
    if (factorial(5) != 120) return 3;
    if (factorial(10) != 3628800) return 4;
    return 0;
}

int test_prime(void) {
    if (is_prime(1)) return 1;
    if (!is_prime(2)) return 2;
    if (!is_prime(7)) return 3;
    if (is_prime(9)) return 4;
    if (!is_prime(97)) return 5;
    return 0;
}

int test_fib(void) {
    if (fibonacci(0) != 0) return 1;
    if (fibonacci(1) != 1) return 2;
    if (fibonacci(10) != 55) return 3;
    return 0;
}

int test_array_sum(void) {
    int arr[5];
    arr[0] = 1;
    arr[1] = 2;
    arr[2] = 3;
    arr[3] = 4;
    arr[4] = 5;
    if (sum_array(arr, 5) != 15) return 1;
    return 0;
}

int test_strlen(void) {
    if (strlen_manual("hello") != 5) return 1;
    if (strlen_manual("") != 0) return 2;
    if (strlen_manual("a") != 1) return 3;
    return 0;
}

int test_global_array(void) {
    int i;
    for (i = 0; i < 8; i = i + 1) {
        g_results[i] = i * i;
    }
    if (g_results[0] != 0) return 1;
    if (g_results[3] != 9) return 2;
    if (g_results[7] != 49) return 3;
    return 0;
}

struct rect {
    int w;
    int h;
};

int rect_area(struct rect *r) {
    return r->w * r->h;
}

int test_struct_func(void) {
    struct rect r;
    r.w = 6;
    r.h = 7;
    if (rect_area(&r) != 42) return 1;
    return 0;
}

/* Test block-scoped variables */
int test_block_scope(void) {
    int x;
    x = 10;
    {
        int y;
        y = 20;
        x = x + y;
    }
    /* y is no longer in scope (but our block scoping just hides it) */
    if (x != 30) return 1;
    return 0;
}

/* Test comma operator */
int test_comma(void) {
    int a;
    int b;
    /* Comma operator: evaluate left, discard, return right */
    a = (1, 2, 3);
    if (a != 3) return 1;
    /* Comma in for-loop increment */
    a = 0;
    b = 0;
    for (a = 0; a < 5; a = a + 1, b = b + 2) { }
    if (a != 5) return 2;
    if (b != 10) return 3;
    return 0;
}

/* Test do-while */
int test_do_while(void) {
    int n;
    int count;
    n = 1;
    count = 0;
    do {
        count = count + 1;
        n = n * 2;
    } while (n < 100);
    if (n != 128) return 1;
    if (count != 7) return 2;
    /* do-while with break */
    n = 0;
    do {
        n = n + 1;
        if (n == 3) break;
    } while (n < 10);
    if (n != 3) return 3;
    return 0;
}

/* Test switch/case */
int test_switch(void) {
    int r;
    int x;
    /* Basic switch */
    x = 2;
    switch (x) {
    case 1: r = 10; break;
    case 2: r = 20; break;
    case 3: r = 30; break;
    default: r = -1; break;
    }
    if (r != 20) return 1;
    /* Default case */
    x = 99;
    switch (x) {
    case 1: r = 10; break;
    default: r = -1; break;
    }
    if (r != -1) return 2;
    /* Fallthrough */
    r = 0;
    x = 1;
    switch (x) {
    case 1: r = r + 10;
    case 2: r = r + 20; break;
    case 3: r = r + 30; break;
    }
    if (r != 30) return 3;
    return 0;
}

int main(void) {
    int rc;
    rc = test_factorial();
    if (rc) return rc;
    rc = test_prime();
    if (rc) return rc + 10;
    rc = test_fib();
    if (rc) return rc + 20;
    rc = test_array_sum();
    if (rc) return rc + 30;
    rc = test_strlen();
    if (rc) return rc + 40;
    rc = test_global_array();
    if (rc) return rc + 50;
    rc = test_struct_func();
    if (rc) return rc + 60;
    rc = test_block_scope();
    if (rc) return rc + 70;
    rc = test_comma();
    if (rc) return rc + 80;
    rc = test_do_while();
    if (rc) return rc + 90;
    rc = test_switch();
    if (rc) return rc + 100;
    return 0;
}
