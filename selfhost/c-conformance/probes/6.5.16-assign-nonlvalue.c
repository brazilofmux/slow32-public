// 6.5.16p2  the left operand of = must be a modifiable lvalue
// expect: reject
int main(void) { int a = 1, b = 2; (a + b) = 3; return a; }
