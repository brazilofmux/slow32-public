// 6.5.3.2p1  & may not be applied to a register object
// expect: reject
int main(void) { register int r = 1; int *p = &r; return *p; }
