// 6.7.2.3p1  a tagged type may be defined at most once in a scope
// expect: reject
struct t { int a; };
struct t { int a; };
int main(void) { return 0; }
