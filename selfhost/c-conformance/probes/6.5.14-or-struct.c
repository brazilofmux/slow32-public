// 6.5.14p2  operands of || must have scalar type
// expect: reject
struct S { int a; };
int main(void) { struct S s = {0}; return 1 || s; }
