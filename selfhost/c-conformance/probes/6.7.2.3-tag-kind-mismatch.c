// 6.7.2.3p2  a tag declared as struct may not be used as union in the same scope
// expect: reject
struct t { int a; };
union t *p;
int main(void) { return 0; }
