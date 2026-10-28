// 6.5.2.4p1/6.7.3  incrementing a const-qualified object is a constraint violation
// expect: reject
int main(void) { const int c = 1; c++; return c; }
