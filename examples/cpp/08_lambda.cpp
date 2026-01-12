// Test 8: Lambdas
// Expected: Should work - lambdas become anonymous structs

int apply(int x, int (*f)(int)) {
    return f(x);
}

template<typename F>
int apply_generic(int x, F f) {
    return f(x);
}

int main() {
    // Simple lambda (convertible to function pointer)
    auto square = [](int x) { return x * x; };
    int a = square(5);  // 25

    // Lambda with capture
    int multiplier = 3;
    auto times_n = [multiplier](int x) { return x * multiplier; };
    int b = times_n(7);  // 21

    // Mutable lambda
    int counter = 0;
    auto increment = [&counter]() { return ++counter; };
    increment();
    increment();
    int c = increment();  // 3

    // Lambda as function pointer (no capture)
    int d = apply(4, [](int x) { return x + 1; });  // 5

    // Lambda with template
    int e = apply_generic(6, [](int x) { return x * 2; });  // 12

    return (a == 25 && b == 21 && c == 3 && d == 5 && e == 12) ? 0 : 1;
}
