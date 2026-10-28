// 6.7.2.2p2  enumerator value must be an integer constant expression
// expect: reject
int n = 3;
enum e { V = n };
int main(void) { return 0; }
