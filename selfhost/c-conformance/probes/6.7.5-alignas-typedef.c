// 6.7.5p2  _Alignas may not appear in a typedef declaration
// expect: reject
typedef _Alignas(8) int T;
int main(void) { return 0; }
