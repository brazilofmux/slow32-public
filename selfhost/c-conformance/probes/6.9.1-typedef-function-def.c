// 6.9.1p2  the function type must come from the declarator, not a typedef
// expect: reject
typedef int F(void);
F f { return 0; }
int main(void) { return f(); }
