// Test 4: Multiple and virtual inheritance
// Expected: May expose vtable layout edge cases

class A {
public:
    int a_val;
    A() : a_val(1) {}
    virtual int get_a() { return a_val; }
};

class B {
public:
    int b_val;
    B() : b_val(2) {}
    virtual int get_b() { return b_val; }
};

// Multiple inheritance
class C : public A, public B {
public:
    int c_val;
    C() : c_val(3) {}
    int get_a() override { return a_val + 10; }
    int get_b() override { return b_val + 20; }
    int get_c() { return c_val; }
};

// Diamond inheritance with virtual base
class VBase {
public:
    int v_val;
    VBase() : v_val(100) {}
    virtual int get_v() { return v_val; }
};

class D1 : virtual public VBase {
public:
    D1() { v_val += 1; }
};

class D2 : virtual public VBase {
public:
    D2() { v_val += 2; }
};

class Diamond : public D1, public D2 {
public:
    Diamond() { v_val += 10; }
};

int main() {
    // Multiple inheritance
    C c;
    A* pa = &c;
    B* pb = &c;

    int va = pa->get_a();  // 11
    int vb = pb->get_b();  // 22
    int vc = c.get_c();    // 3

    // Diamond inheritance - single VBase instance
    Diamond d;
    int vd = d.get_v();    // 100 + 1 + 2 + 10 = 113

    return (va == 11 && vb == 22 && vc == 3 && vd == 113) ? 0 : 1;
}
