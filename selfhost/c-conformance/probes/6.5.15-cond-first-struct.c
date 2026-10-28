// 6.5.15p2  the first operand of ?: must have scalar type
// expect: reject
struct S { int a; };
int main(void) { struct S s = {0}; return s ? 1 : 2; }
