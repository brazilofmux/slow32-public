// 6.7.6.3p10  void as a parameter must be the only, unnamed parameter
// expect: reject
int f(void, int x);
int main(void) { return 0; }
