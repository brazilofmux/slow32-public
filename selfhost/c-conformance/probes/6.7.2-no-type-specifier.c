// 6.7.2p2  a declaration must have at least one type specifier (no implicit int)
// expect: reject
static x = 3;
int main(void) { return x; }
