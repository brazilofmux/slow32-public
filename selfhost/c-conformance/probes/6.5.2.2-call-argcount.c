// 6.5.2.2p2  with a prototype, the number of arguments must match the parameters
// expect: reject
static int f(int a, int b) { return a + b; }
int main(void) { return f(1); }
