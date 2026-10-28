// 6.5.16.1p1  struct assignment requires compatible struct types
// expect: reject
struct A { int x; }; struct B { int x; };
int main(void) { struct A a = {1}; struct B b; b = a; return b.x; }
