// 6.7.2p2/6.7.8  a typedef name may not be combined with other type specifiers
// expect: reject
typedef int T;
unsigned T x;
int main(void) { return 0; }
