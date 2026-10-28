// 6.7.6.3p1  a function may not return a function type
// expect: reject
typedef int F(void);
F g(void);
int main(void) { return 0; }
