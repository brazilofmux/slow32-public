int fib(int n) {
    if (n <= 1) return n;
    return fib(n-1) + fib(n-2);
}

int main() {
    volatile int sink = 0;
    for (int i = 0; i < 50000; i++) {
        sink = fib(20 + (i & 1));
    }
    return 0;
}