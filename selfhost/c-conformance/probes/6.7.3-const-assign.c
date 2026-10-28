// 6.5.16p2/6.7.3  assigning to a const-qualified local is a constraint violation
// expect: reject
int main(void) { const int c = 1; c = 2; return c; }
