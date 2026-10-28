// 6.10.3p2  object-like macro redefined as function-like
// expect: reject
#define M 1
#define M(x) 1
int main(void) { return 0; }
