// Test 10: References and move semantics basics
// Expected: Should work - references are pointers, moves are copies for simple types

void swap_ref(int& a, int& b) {
    int tmp = a;
    a = b;
    b = tmp;
}

class Buffer {
    int* data;
    int size;
public:
    Buffer(int n) : data(nullptr), size(n) {
        // Note: Would need new[] to actually allocate
        // For this test, just track size
    }

    // Copy constructor
    Buffer(const Buffer& other) : data(nullptr), size(other.size) {
        // Would copy data here
    }

    // Move constructor (C++11)
    Buffer(Buffer&& other) : data(other.data), size(other.size) {
        other.data = nullptr;
        other.size = 0;
    }

    // Copy assignment
    Buffer& operator=(const Buffer& other) {
        if (this != &other) {
            size = other.size;
        }
        return *this;
    }

    // Move assignment
    Buffer& operator=(Buffer&& other) {
        if (this != &other) {
            data = other.data;
            size = other.size;
            other.data = nullptr;
            other.size = 0;
        }
        return *this;
    }

    int get_size() const { return size; }
};

// Simulating std::move
template<typename T>
T&& my_move(T& x) {
    return static_cast<T&&>(x);
}

int main() {
    // Reference basics
    int x = 10, y = 20;
    swap_ref(x, y);
    // x=20, y=10

    // Const reference
    const int& cr = x;
    int val = cr;  // 20

    // Move semantics
    Buffer b1(100);
    Buffer b2(my_move(b1));  // Move construct

    int s1 = b1.get_size();  // 0 (moved from)
    int s2 = b2.get_size();  // 100

    Buffer b3(50);
    b3 = my_move(b2);  // Move assign
    int s3 = b3.get_size();  // 100

    return (x == 20 && y == 10 && val == 20 &&
            s1 == 0 && s2 == 100 && s3 == 100) ? 0 : 1;
}
