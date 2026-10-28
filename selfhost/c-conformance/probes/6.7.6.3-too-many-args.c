// 6.5.2.2p2  calling a prototyped function with the wrong number of arguments
// expect: reject
static int f(int a) { return a; }
int main(void) { return f(1, 2); }
