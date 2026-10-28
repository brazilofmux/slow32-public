// 6.7.6.3p15/6.7p4  two declarations of a function with incompatible prototypes
// expect: reject
int f(int);
int f(double);
int main(void) { return 0; }
