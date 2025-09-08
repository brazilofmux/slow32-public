// Force recursive factorial (no tail call optimization)
__attribute__((noinline))
int factorial(int n) {
    if (n <= 1) {
        return 1;
    }
    int temp = factorial(n - 1);
    return n * temp;
}

int main() {
    // Calculate 5! = 120
    return factorial(5);
}