// 6.5.1p2  an identifier used as a primary expression must be declared
// expect: reject
int main(void) { return undeclared_thing; }
