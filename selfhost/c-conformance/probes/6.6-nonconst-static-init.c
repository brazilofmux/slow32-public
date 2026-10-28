// 6.6p2,7  a static initializer must be a constant expression
// expect: reject
static int g = 1;
static int h = g + 1;
int main(void) { return h; }
