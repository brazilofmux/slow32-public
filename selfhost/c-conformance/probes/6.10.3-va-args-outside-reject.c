// 6.10.3p5  __VA_ARGS__ only in the list of a variadic macro
// expect: reject
#define NOTVAR(a) a + __VA_ARGS__
int main(void) { return 0; }
