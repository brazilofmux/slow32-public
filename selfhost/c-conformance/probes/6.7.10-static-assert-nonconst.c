// 6.7.10p3  the static-assert expression must be an integer constant expression
// expect: reject
int n = 1;
_Static_assert(n, "not constant");
int main(void) { return 0; }
