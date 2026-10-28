// 6.5.2.3p1  the member named must be a member of the struct or union
// expect: reject
struct S { int a; };
int main(void) { struct S s = {0}; return s.b; }
