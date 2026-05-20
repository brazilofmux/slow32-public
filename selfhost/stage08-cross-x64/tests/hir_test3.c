int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

int sum_to(int n) {
    int s;
    int i;
    s = 0;
    i = 1;
    while (i <= n) {
        s = s + i;
        i = i + 1;
    }
    return s;
}

int main() {
    int a;
    int b;
    a = factorial(5);   /* 120 */
    b = sum_to(10);     /* 55 */
    return a + b;       /* 175 */
}
