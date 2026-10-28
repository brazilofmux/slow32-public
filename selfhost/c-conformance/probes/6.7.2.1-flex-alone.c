// 6.7.2.1p3  a struct with a flexible array member must have another named member
// expect: reject
struct s { int a[]; };
int main(void) { return 0; }
