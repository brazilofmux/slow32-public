// 6.10.1p1  a malformed #if expression (dangling operator) must be diagnosed
// expect: reject
#if 1 +
#endif
int main(void) { return 0; }
