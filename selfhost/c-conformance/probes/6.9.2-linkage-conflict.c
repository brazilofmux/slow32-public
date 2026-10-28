// 6.9.2p2 / 6.2.2p7  a static tentative definition after an external one is a linkage conflict
// expect: reject
int x;
static int x;
int main(void) { return x; }
