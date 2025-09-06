// Factorial calculation - demonstrates recursion on SLOW-32
// Compile with: clang --target slow32-unknown-none -S -emit-llvm -O1 factorial.c

int factorial(int n) {
    if (n <= 1) {
        return 1;
    }
    return n * factorial(n - 1);
}

int main() {
    // Calculate 5! = 120
    // The return value can be checked with:
    // slow32 factorial.s32x; echo $?
    // (Note: shell will show 120 & 0xFF = 120)
    return factorial(5);
}