// 6.5.3.4p1  sizeof may not be applied to a bit-field member
// expect: reject
struct S { int b : 3; };
int main(void) { struct S s; s.b = 0; return (int)sizeof s.b; }
