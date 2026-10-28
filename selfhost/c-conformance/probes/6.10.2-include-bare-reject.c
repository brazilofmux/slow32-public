// 6.10.2p4  after replacement the directive must match "..." or <...>
// expect: reject
#define NOT_A_NAME 42
#include NOT_A_NAME
int main(void) { return 0; }
