// 6.10.3p4  a variadic macro needs more arguments than named parameters
// expect: reject
#define V(fmt, ...) f(fmt, __VA_ARGS__)
static int f(int a, int b) { return a + b; }
int main(void) { return V(1); }
