// 6.7.9p11/6.5.16.1p1  a file-scope pointer initialized with a nonzero integer: an assignment constraint
// expect: reject
int *p = 5;
int main(void) { return p != 0; }
