// 6.5.2.3p2  the left operand of -> must be a pointer to struct or union
// expect: reject
struct S { int a; };
int main(void) { struct S s = {0}; return s->a; }
