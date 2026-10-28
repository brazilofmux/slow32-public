// 6.7.6.2p1  element type may not be a function type
// expect: reject
int f[3](void);
int main(void) { return 0; }
