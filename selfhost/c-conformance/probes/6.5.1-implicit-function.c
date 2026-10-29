// 6.5.1p2, 6.5.2.2p1  a function must be declared before it is called (no implicit declaration since C99)
// expect: reject
int main(void) { return undeclared_fn(1); }
