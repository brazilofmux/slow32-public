// 6.5.4p2  a cast must name void or a scalar type, and the operand must be scalar
// expect: reject
struct S { int a; };
int main(void) { int x = 1; struct S s = (struct S)x; return s.a; }
