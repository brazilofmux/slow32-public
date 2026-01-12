// Test 6: Static local variables with non-trivial initialization
// Expected: Will need __cxa_guard_* functions

class Singleton {
    int value;
    Singleton() : value(42) {}
public:
    static Singleton& instance() {
        static Singleton s;  // Requires guard for thread-safe init
        return s;
    }
    int get() const { return value; }
    void set(int v) { value = v; }
};

int get_static_value() {
    static int x = 100;  // Simple static - may not need guard
    return x++;
}

int main() {
    // Static local with guard
    Singleton& s1 = Singleton::instance();
    int v1 = s1.get();  // 42

    s1.set(99);

    Singleton& s2 = Singleton::instance();  // Same instance
    int v2 = s2.get();  // 99

    // Simple static
    int a = get_static_value();  // 100
    int b = get_static_value();  // 101
    int c = get_static_value();  // 102

    return (v1 == 42 && v2 == 99 && a == 100 && b == 101 && c == 102) ? 0 : 1;
}
