// 6.5.2.5p1  a compound literal's type may not be a variable length array
// expect: reject
int main(void) { int n = 3; int *p = (int[n]){1, 2, 3}; return p[0]; }
