// Simple factorial test program
int factorial(int n) {
    if (n <= 1) {
        return 1;
    }
    return n * factorial(n - 1);
}

int main() {
    // Calculate 5! = 120
    int result = factorial(5);
    return result;
}