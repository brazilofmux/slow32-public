// Test 9: Operator overloading
// Expected: Should work - just function calls

class Vector2 {
public:
    int x, y;

    Vector2() : x(0), y(0) {}
    Vector2(int x_, int y_) : x(x_), y(y_) {}

    Vector2 operator+(const Vector2& other) const {
        return Vector2(x + other.x, y + other.y);
    }

    Vector2 operator-(const Vector2& other) const {
        return Vector2(x - other.x, y - other.y);
    }

    Vector2 operator*(int scalar) const {
        return Vector2(x * scalar, y * scalar);
    }

    Vector2& operator+=(const Vector2& other) {
        x += other.x;
        y += other.y;
        return *this;
    }

    bool operator==(const Vector2& other) const {
        return x == other.x && y == other.y;
    }

    bool operator!=(const Vector2& other) const {
        return !(*this == other);
    }

    // Prefix increment
    Vector2& operator++() {
        ++x;
        ++y;
        return *this;
    }

    // Postfix increment
    Vector2 operator++(int) {
        Vector2 tmp = *this;
        ++(*this);
        return tmp;
    }
};

// Non-member operator
Vector2 operator*(int scalar, const Vector2& v) {
    return v * scalar;
}

int main() {
    Vector2 a(3, 4);
    Vector2 b(1, 2);

    Vector2 c = a + b;      // (4, 6)
    Vector2 d = a - b;      // (2, 2)
    Vector2 e = a * 2;      // (6, 8)
    Vector2 f = 3 * b;      // (3, 6)

    Vector2 g(0, 0);
    g += a;                 // (3, 4)
    ++g;                    // (4, 5)

    bool eq = (c == Vector2(4, 6));
    bool ne = (d != Vector2(0, 0));

    return (c.x == 4 && c.y == 6 &&
            d.x == 2 && d.y == 2 &&
            e.x == 6 && e.y == 8 &&
            f.x == 3 && f.y == 6 &&
            g.x == 4 && g.y == 5 &&
            eq && ne) ? 0 : 1;
}
