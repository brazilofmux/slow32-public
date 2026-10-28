// 6.9p5 / 6.7p3  a function defined twice in one translation unit
// expect: reject
int f(void) { return 1; }
int f(void) { return 2; }
int main(void) { return f(); }
