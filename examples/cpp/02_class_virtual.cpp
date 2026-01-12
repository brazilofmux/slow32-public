// Test 2: Virtual functions and vtables
// Expected: Should work - vtables are just function pointer tables

class Shape {
public:
    virtual ~Shape() {}
    virtual int area() const = 0;  // Pure virtual
};

class Rectangle : public Shape {
    int width, height;
public:
    Rectangle(int w, int h) : width(w), height(h) {}
    int area() const override { return width * height; }
};

class Square : public Rectangle {
public:
    Square(int side) : Rectangle(side, side) {}
};

int compute_area(const Shape& s) {
    return s.area();
}

int main() {
    Rectangle r(3, 4);
    Square s(5);

    int r_area = compute_area(r);  // 12
    int s_area = compute_area(s);  // 25

    return (r_area == 12 && s_area == 25) ? 0 : 1;
}
