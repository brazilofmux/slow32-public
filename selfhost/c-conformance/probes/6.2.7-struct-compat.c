// 6.2.7p1  distinct tagged structs declared in one TU are different types: assigning between them is a constraint violation
// expect: reject
struct A { int x; };
struct B { int x; };
int main(void) { struct A a = { 1 }; struct B b; b = a; return b.x; }
