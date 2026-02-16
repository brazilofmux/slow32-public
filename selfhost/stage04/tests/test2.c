/* test2.c — Recursive factorial */
int putchar(int c);

void print_int(int n) {
    if (n >= 10) {
        print_int(n / 10);
    }
    putchar(48 + n % 10);
}

int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

int main() {
    int i;
    for (i = 1; i <= 10; i = i + 1) {
        print_int(i);
        putchar(33);
        putchar(32);
        putchar(61);
        putchar(32);
        print_int(factorial(i));
        putchar(10);
    }
    return 0;
}
