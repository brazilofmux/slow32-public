// 6.7p4/6.7.6.2p6  one array declared twice with two different sizes
// expect: reject
int a[3];
int a[4];
int main(void) { return 0; }
