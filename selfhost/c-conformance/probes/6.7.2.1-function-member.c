// 6.7.2.1p3  a member may not have function type
// expect: reject
struct s { int f(void); };
int main(void) { return 0; }
