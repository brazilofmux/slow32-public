// 6.7.9p7  a member designator must name a member of the struct
// expect: reject
struct s { int a; };
struct s v = { .nope = 1 };
int main(void) { return v.a; }
