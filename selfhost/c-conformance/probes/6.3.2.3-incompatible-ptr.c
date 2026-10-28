// 6.3.2.3p7  assigning between incompatible object pointer types needs a cast (6.5.16.1p1)
// expect: reject
int main(void) { long l = 0; int *p = &l; return *p; }
