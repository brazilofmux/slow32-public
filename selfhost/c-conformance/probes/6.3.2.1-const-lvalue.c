// 6.3.2.1p1  a const-qualified object is not a modifiable lvalue (6.5.16p2 constraint)
// expect: reject
int main(void) { const int c = 1; c = 2; return c; }
