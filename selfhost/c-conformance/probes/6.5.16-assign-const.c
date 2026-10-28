// 6.5.16p2  a const-qualified object is not a modifiable lvalue
// expect: reject
int main(void) { const int c = 1; c = 2; return c; }
