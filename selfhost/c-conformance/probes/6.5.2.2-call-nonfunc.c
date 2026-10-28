// 6.5.2.2p1  the called expression must have pointer-to-function type
// expect: reject
int main(void) { int x = 0; return x(); }
