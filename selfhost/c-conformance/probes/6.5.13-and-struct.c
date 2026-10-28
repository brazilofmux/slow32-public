// 6.5.13p2  operands of && must have scalar type
// expect: reject
struct S { int a; };
int main(void) { struct S s = {0}; return s && 1; }
