// 6.5.11p2  operands of ^ must have integer type
// expect: reject
int main(void) { int x; int *p = &x; return (int)(p ^ 1); }
