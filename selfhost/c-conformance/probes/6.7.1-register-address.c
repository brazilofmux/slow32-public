// 6.5.3.2p1/6.7.1p6  address of a register object may not be taken
// expect: reject
int main(void) { register int r = 3; int *p = &r; return *p; }
