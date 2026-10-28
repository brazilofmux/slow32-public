// 6.7.9p4  initializer of a static object must be a constant expression
// expect: reject
int f(void);
static int x = f();
int main(void) { return x; }
