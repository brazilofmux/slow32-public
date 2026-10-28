// 6.5.2.2p2  each argument must be assignable to its parameter's type
// expect: reject
struct S { int x; };
static int f(int *p) { return *p; }
int main(void) { struct S s = {1}; return f(s); }
