// 6.5.9p2  comparing pointers to incompatible object types is a constraint violation
// expect: reject
int main(void) { int x; double y; return &x == &y; }
