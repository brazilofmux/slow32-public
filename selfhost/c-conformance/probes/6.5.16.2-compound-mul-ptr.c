// 6.5.16.2p2  *= requires arithmetic operands
// expect: reject
int main(void) { int a[2]; int *p = a; p *= 2; return 0; }
