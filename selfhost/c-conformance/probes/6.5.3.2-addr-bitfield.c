// 6.5.3.2p1  & may not be applied to a bit-field
// expect: reject
struct S { int b : 3; };
int main(void) { struct S s; s.b = 1; int *p = &s.b; return *p; }
