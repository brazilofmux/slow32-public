// 6.9.1p3  a function shall not return a function type
// expect: reject
typedef int G(void);
G f(void) { return 0; }
int main(void) { return 0; }
