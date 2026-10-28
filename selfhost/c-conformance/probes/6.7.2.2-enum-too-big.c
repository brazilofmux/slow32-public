// 6.7.2.2p2  enumerator value must be representable as int
// expect: reject
enum e { BIG = 0x100000000LL };
int main(void) { return 0; }
