// 6.3.2.3p5  a non-constant integer to pointer needs a cast (constraint 6.5.16.1p1)
// expect: reject
int main(void) { int n = 4; int *p = n; return p != 0; }
