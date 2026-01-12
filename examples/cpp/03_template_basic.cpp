// Test 3: Function and class templates
// Expected: Should work - templates are fully instantiated by frontend

template<typename T>
T max(T a, T b) {
    return (a > b) ? a : b;
}

template<typename T, int N>
class Array {
    T data[N];
public:
    T& operator[](int i) { return data[i]; }
    const T& operator[](int i) const { return data[i]; }
    int size() const { return N; }

    void fill(T value) {
        for (int i = 0; i < N; i++) {
            data[i] = value;
        }
    }
};

int main() {
    // Function template
    int a = max(3, 7);      // 7
    int b = max(10, 5);     // 10

    // Class template
    Array<int, 5> arr;
    arr.fill(0);
    arr[0] = 1;
    arr[1] = 2;
    arr[2] = 3;

    int sum = 0;
    for (int i = 0; i < arr.size(); i++) {
        sum += arr[i];
    }

    // a=7, b=10, sum=6
    return (a == 7 && b == 10 && sum == 6) ? 0 : 1;
}
