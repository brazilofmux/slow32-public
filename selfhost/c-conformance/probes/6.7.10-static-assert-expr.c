// 6.7.10p2  a static assertion whose (non-literal) expression is false must fail
// expect: reject
_Static_assert(1 == 2, "one is not two");
int main(void) { return 0; }
