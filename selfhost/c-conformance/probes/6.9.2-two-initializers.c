// 6.9p5 / 6.7p3  two definitions with initializers of the same external object
// expect: reject
int x = 1;
int x = 2;
int main(void) { return x; }
