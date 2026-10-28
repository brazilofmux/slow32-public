// 6.5.3.3p1  the operand of ! must have scalar type
// expect: reject
struct S { int a; };
int main(void) { struct S s = {0}; return !s; }
