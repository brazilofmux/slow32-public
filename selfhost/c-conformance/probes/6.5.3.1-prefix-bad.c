// 6.5.3.1p1  operand of prefix ++ must be a modifiable lvalue of real or pointer type
// expect: reject
struct S { int a; };
int main(void) { struct S s = {0}; ++s; return 0; }
