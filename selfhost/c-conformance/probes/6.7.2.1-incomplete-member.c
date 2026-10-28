// 6.7.2.1p3  a member may not have incomplete type (struct containing itself)
// expect: reject
struct s { int n; struct s inner; };
int main(void) { return 0; }
