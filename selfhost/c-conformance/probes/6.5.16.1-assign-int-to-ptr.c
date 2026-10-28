// 6.5.16.1p1  assigning a nonzero integer to a pointer is a constraint violation
// expect: reject
int main(void) { int *p; p = 5; return p != 0; }
