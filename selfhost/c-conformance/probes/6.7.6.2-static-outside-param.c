// 6.7.6.2p1  static in array brackets only in a function parameter
// expect: reject
int main(void) { int a[static 3]; return 0; }
