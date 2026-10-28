// 6.7.2.1p3  a flexible array member must be the last member
// expect: reject
struct s { int a[]; int n; };
int main(void) { return 0; }
