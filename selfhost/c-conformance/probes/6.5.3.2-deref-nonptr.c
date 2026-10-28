// 6.5.3.2p2  the operand of unary * must have pointer type
// expect: reject
int main(void) { int x = 1; return *x; }
