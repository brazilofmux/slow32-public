// 6.10.8p2  the identifier defined may not be #defined
// expect: reject
#define defined 1
int main(void) { return 0; }
