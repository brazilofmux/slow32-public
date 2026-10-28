// 6.5.6p2  pointer + pointer is a constraint violation
// expect: reject
int main(void) { int a[2]; int *p = a, *q = a; return (int)(p + q - p); }
