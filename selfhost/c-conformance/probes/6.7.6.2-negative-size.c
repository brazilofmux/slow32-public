// 6.7.6.2p1  a constant array size must be greater than zero (negative)
// expect: reject
int a[-1];
int main(void) { return 0; }
