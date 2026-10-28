// 6.7.6.3p2  the only storage class allowed on a parameter is register
// expect: reject
int f(static int x) { return x; }
int main(void) { return 0; }
