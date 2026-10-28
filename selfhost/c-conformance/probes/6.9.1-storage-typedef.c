// 6.9.1p4  typedef is not a valid storage class for a function definition
// expect: reject
typedef int f(void) { return 1; }
int main(void) { return 0; }
