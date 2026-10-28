// 6.2.7p3  prototypes with different parameter types are not compatible
// expect: reject
int g(int);
int g(double);
int main(void) { return 0; }
