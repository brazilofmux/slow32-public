// 6.7.6.2p2  a VLA may not be declared at file scope
// expect: reject
int n = 3;
int a[n];
int main(void) { return 0; }
