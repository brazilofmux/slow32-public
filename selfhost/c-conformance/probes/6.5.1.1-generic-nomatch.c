// 6.5.1.1p2  without default, exactly one association must match the controlling type
// expect: reject
int main(void) { return _Generic(1.0, int: 1, long: 2); }
