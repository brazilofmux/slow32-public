// 6.5.12p2  operands of | must have integer type
// expect: reject
struct S { int a; };
int main(void) { struct S s = {0}; return s | 1; }
