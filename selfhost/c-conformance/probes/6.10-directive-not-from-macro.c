// 6.10p8  a # produced by macro expansion never starts a directive
// expect: reject
#define EMPTY
EMPTY # define Z 1
int main(void) { return 0; }
