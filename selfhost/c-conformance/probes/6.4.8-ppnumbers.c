// 6.4.8p2  pp-numbers: 0x1e+1 is one pp-number (invalid as a constant), so this must be rejected
// expect: reject
int main(void) { return 0x1e+1; }
