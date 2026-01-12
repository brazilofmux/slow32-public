// Test 1: Basic class with constructor, destructor, methods
// Expected: Should mostly work - classes are just structs

class Counter {
    int value;
public:
    Counter() : value(0) {}
    Counter(int initial) : value(initial) {}
    ~Counter() { value = -1; }  // Mark as destroyed

    void increment() { value++; }
    void decrement() { value--; }
    int get() const { return value; }
};

int main() {
    Counter c;
    c.increment();
    c.increment();
    c.increment();
    c.decrement();
    return c.get();  // Should return 2
}
