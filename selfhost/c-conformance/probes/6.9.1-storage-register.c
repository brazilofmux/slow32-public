// 6.9.1p4  only extern or static may appear as storage class in a function definition
// expect: reject
register int f(void) { return 1; }
int main(void) { return f(); }
